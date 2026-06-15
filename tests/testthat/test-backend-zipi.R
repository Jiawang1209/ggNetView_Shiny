## Tests for ggnetview_zipi / safe_zipi / adjacency_from_graph correctness
## Covers audit findings L6 (Pi mixes degree sources), M3 (unvalidated thresholds),
## L5 (missing weight attr crash in adjacency_from_graph).

if (!exists("ggnetview_zipi")) pkgload::load_all(".", quiet = TRUE)

library(igraph)

# ---------------------------------------------------------------------------
# Helper: build a small multi-module igraph with known structure.
# 6 nodes split into 2 modules of 3. Every within-module pair is connected;
# only node 1 bridges between modules.
#
# Module A: nodes 1,2,3 (fully connected clique)
# Module B: nodes 4,5,6 (fully connected clique)
# Bridge:   1 -- 4  (only cross-module edge)
#
# True total degree (from edges):
#   1 -> 3 (within A) + 1 (bridge) = 4 ... wait, within clique of 3: each node
#   connects to 2 others.  Node 1 also connects to node 4.
#   So: 1->deg 3, 2->deg 2, 3->deg 2, 4->deg 3, 5->deg 2, 6->deg 2
# ---------------------------------------------------------------------------
make_two_module_graph <- function() {
  el <- rbind(
    c("n1","n2"), c("n1","n3"), c("n2","n3"),   # module A clique
    c("n4","n5"), c("n4","n6"), c("n5","n6"),   # module B clique
    c("n1","n4")                                  # bridge
  )
  g <- igraph::graph_from_edgelist(el, directed = FALSE)

  # Assign module membership
  igraph::V(g)$Modularity <- c("A","A","A","B","B","B")
  igraph::V(g)$Degree     <- igraph::degree(g)
  g
}

# ---------------------------------------------------------------------------
# L6 — Pi (participation coefficient) must lie in [0, 1] for every node.
#
# Bug: k_tot was taken from the node-table `deg` column (which comes from
# igraph::degree() on the full graph including self-loop handling differences),
# while k_is was derived from the binarized adjacency A.  When they diverge,
# `1 - sum_kis2 / k_tot^2` can exceed 1 or go negative.
#
# Fix: derive k_tot from rowSums(A) so both quantities use the same matrix.
# ---------------------------------------------------------------------------
test_that("L6: Pi is within [0, 1] for every node in a multi-module network", {
  g <- make_two_module_graph()
  nodes <- igraph::as_data_frame(g, what = "vertices")
  nodes$name <- rownames(nodes)
  adj   <- as.matrix(igraph::as_adjacency_matrix(g, sparse = FALSE))

  res <- ggnetview_zipi(
    nodes_bulk     = nodes,
    z_bulk_mat     = adj,
    modularity_col = "Modularity",
    degree_col     = "Degree"
  )

  pi_vals <- res$data$among_module_connectivities
  finite_pi <- pi_vals[is.finite(pi_vals)]

  expect_true(
    length(finite_pi) > 0,
    label = "At least one finite Pi value expected"
  )
  expect_true(
    all(finite_pi >= 0),
    label = paste("All Pi >= 0; got min =", min(finite_pi))
  )
  expect_true(
    all(finite_pi <= 1),
    label = paste("All Pi <= 1; got max =", max(finite_pi))
  )
})

test_that("L6: Pi stays in [0,1] when node-table degree and matrix degree could diverge", {
  # Artificially inflate the degree column beyond what the adjacency encodes.
  # This simulates the divergence scenario: node-table says degree=100
  # but the adjacency matrix only has a few connections.
  g <- make_two_module_graph()
  nodes <- igraph::as_data_frame(g, what = "vertices")
  nodes$name <- rownames(nodes)

  # Deliberately wrong degree column (too large) — exactly what causes Pi < 0
  # when k_tot^2 < sum_kis2 after the bug lets this pass through.
  # Under the fix k_tot is derived from A, so this inflated value is ignored.
  nodes$Degree <- 100L

  adj <- as.matrix(igraph::as_adjacency_matrix(g, sparse = FALSE))

  res <- ggnetview_zipi(
    nodes_bulk     = nodes,
    z_bulk_mat     = adj,
    modularity_col = "Modularity",
    degree_col     = "Degree"
  )

  pi_vals   <- res$data$among_module_connectivities
  finite_pi <- pi_vals[is.finite(pi_vals)]

  expect_true(all(finite_pi >= 0), label = paste("Pi >= 0; got min =", min(finite_pi)))
  expect_true(all(finite_pi <= 1), label = paste("Pi <= 1; got max =", max(finite_pi)))
})

# ---------------------------------------------------------------------------
# M3 — safe_zipi must reject NA / out-of-range thresholds up-front.
#
# Bug: thresholds were forwarded unvalidated; NA thresholds cause all
# comparisons to return NA so every node ends up type=NA (unclassified)
# with no error or warning emitted.
#
# Fix: validate before calling ggnetview_zipi; return app_failure() with a
# clear message when thresholds are not finite or pi_threshold ∉ [0,1].
# ---------------------------------------------------------------------------
test_that("M3: safe_zipi returns app_failure when zi_threshold is NA", {
  g <- make_two_module_graph()

  result <- safe_zipi(g, zi_threshold = NA, pi_threshold = 0.62)

  expect_false(
    result$ok,
    label = "safe_zipi with NA zi_threshold must return ok=FALSE"
  )
  expect_false(
    is.null(result$message),
    label = "app_failure must carry a non-NULL message"
  )
})

test_that("M3: safe_zipi returns app_failure when pi_threshold is NA", {
  g <- make_two_module_graph()

  result <- safe_zipi(g, zi_threshold = 2.5, pi_threshold = NA)

  expect_false(result$ok)
})

test_that("M3: safe_zipi returns app_failure when pi_threshold is out of range (> 1)", {
  g <- make_two_module_graph()

  result <- safe_zipi(g, zi_threshold = 2.5, pi_threshold = 2)

  expect_false(
    result$ok,
    label = "safe_zipi with pi_threshold=2 must return ok=FALSE"
  )
})

test_that("M3: safe_zipi returns app_failure when pi_threshold is out of range (< 0)", {
  g <- make_two_module_graph()

  result <- safe_zipi(g, zi_threshold = 2.5, pi_threshold = -0.1)

  expect_false(result$ok)
})

test_that("M3: safe_zipi returns app_failure when zi_threshold is non-finite (Inf)", {
  g <- make_two_module_graph()

  result <- safe_zipi(g, zi_threshold = Inf, pi_threshold = 0.62)

  expect_false(result$ok)
})

# ---------------------------------------------------------------------------
# L5 — adjacency_from_graph must not error on a graph with no weight attribute.
#
# Bug: the fallback path called igraph::as_adjacency_matrix(..., attr = "weight")
# unconditionally; if the graph has no `weight` edge attribute igraph throws
# "no such edge attribute".
#
# Fix: guard with `if ('weight' %in% igraph::edge_attr_names(graph))` before
# setting attr, defaulting to NULL (unweighted adjacency) if absent.
# ---------------------------------------------------------------------------
test_that("L5: adjacency_from_graph does not error on graph with no weight attribute", {
  g <- igraph::make_ring(5)
  # Confirm no weight attribute on edges
  expect_false("weight" %in% igraph::edge_attr_names(g))

  # adjacency_from_graph is an internal helper; call it directly after load_all
  expect_no_error(
    adjacency_from_graph(g)
  )

  mat <- adjacency_from_graph(g)
  expect_true(is.matrix(mat))
  expect_equal(nrow(mat), 5L)
})

test_that("L5: safe_zipi on a graph with no weight attribute does not crash", {
  g <- igraph::make_ring(5)
  # Assign required node attributes
  igraph::V(g)$name       <- paste0("n", seq_len(5))
  igraph::V(g)$Modularity <- c("A","A","B","B","B")
  igraph::V(g)$Degree     <- igraph::degree(g)

  # This must not throw "no such edge attribute"
  expect_no_error(
    safe_zipi(g)
  )
})
