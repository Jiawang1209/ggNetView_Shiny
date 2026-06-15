## Tests for get_subgraph and get_graph_adjacency robustness/correctness
## Covers audit findings M1 (character Modularity), L3a (missing column),
## L3b (invalid select_module), L2 (adjacency weights honored)

if (!exists("get_subgraph")) pkgload::load_all(".", quiet = TRUE)

library(igraph)
library(tidygraph)

# ---------------------------------------------------------------------------
# Helper: build a minimal tbl_graph with a Modularity node attribute.
# nodes_df must have columns: name, Modularity (and optionally weight on edges).
# edges_df must have columns: from, to (and optionally weight).
# ---------------------------------------------------------------------------
make_test_graph <- function(mod_type = c("character", "factor", "none"),
                            weighted = FALSE) {
  mod_type <- match.arg(mod_type)

  nodes <- data.frame(
    name = c("A", "B", "C", "D", "E"),
    stringsAsFactors = FALSE
  )

  if (mod_type == "character") {
    nodes$Modularity <- c("mod1", "mod1", "mod2", "mod2", "mod2")
  } else if (mod_type == "factor") {
    nodes$Modularity <- factor(c("mod1", "mod1", "mod2", "mod2", "mod2"))
  }
  # "none" → no Modularity column at all

  edges <- data.frame(
    from   = c("A", "B", "C", "D"),
    to     = c("B", "C", "D", "E"),
    stringsAsFactors = FALSE
  )
  if (weighted) {
    edges$weight <- c(0.9, 0.5, 0.3, 0.7)
  }

  ig <- igraph::graph_from_data_frame(edges, directed = FALSE, vertices = nodes)
  tidygraph::as_tbl_graph(ig)
}

# ---------------------------------------------------------------------------
# M1 — CHARACTER Modularity column must produce a non-empty sub_graph_all
#
# Bug: levels() on a character vector returns NULL → names(module_list) = NULL
# → the for-loop over NULL iterates zero times → sub_graph_all is empty list.
# Fix: use sort(unique(as.character(...))) to derive module names.
# ---------------------------------------------------------------------------
test_that("M1: get_subgraph works when Modularity column is CHARACTER (not factor)", {
  g <- make_test_graph(mod_type = "character")

  result <- get_subgraph(g, select_module = NULL)

  # sub_graph_all must be a non-empty named list
  expect_type(result, "list")
  expect_true("sub_graph_all" %in% names(result))

  sub <- result$sub_graph_all
  expect_true(is.list(sub),
    label = "sub_graph_all must be a list")
  expect_gt(length(sub), 0L,
    label = "sub_graph_all must not be empty when Modularity is CHARACTER")

  # Both modules must be present
  expect_true("mod1" %in% names(sub),
    label = "Module 'mod1' must appear in sub_graph_all")
  expect_true("mod2" %in% names(sub),
    label = "Module 'mod2' must appear in sub_graph_all")

  # mod1 has nodes A, B (2 nodes); mod2 has nodes C, D, E (3 nodes)
  n_mod1 <- igraph::gorder(tidygraph::as.igraph(sub[["mod1"]]))
  n_mod2 <- igraph::gorder(tidygraph::as.igraph(sub[["mod2"]]))
  expect_equal(n_mod1, 2L,
    label = paste0("mod1 should have 2 nodes; got ", n_mod1))
  expect_equal(n_mod2, 3L,
    label = paste0("mod2 should have 3 nodes; got ", n_mod2))
})

# Sanity: factor Modularity must still work (regression guard)
test_that("M1-sanity: get_subgraph still works when Modularity is FACTOR", {
  g <- make_test_graph(mod_type = "factor")

  result <- get_subgraph(g, select_module = NULL)
  sub <- result$sub_graph_all

  expect_gt(length(sub), 0L)
  expect_true("mod1" %in% names(sub))
  expect_true("mod2" %in% names(sub))
})

# ---------------------------------------------------------------------------
# L3a — Missing Modularity column must throw an informative error
# ---------------------------------------------------------------------------
test_that("L3a: get_subgraph errors with informative message when Modularity column is absent", {
  g <- make_test_graph(mod_type = "none")

  expect_error(
    get_subgraph(g),
    regexp = "Modularity",
    label = "Error message must mention 'Modularity'"
  )
})

# ---------------------------------------------------------------------------
# L3b — A select_module that matches no module must produce a warning
# ---------------------------------------------------------------------------
test_that("L3b: get_subgraph warns when select_module matches no module (empty subgraph)", {
  g <- make_test_graph(mod_type = "character")

  expect_warning(
    result <- get_subgraph(g, select_module = "nonexistent_module"),
    regexp = "empty|0 node|no node",
    ignore.case = TRUE,
    label = "Must warn when select_module yields empty subgraph"
  )

  # The selected subgraph should have 0 nodes
  n_selected <- igraph::gorder(tidygraph::as.igraph(result$sub_graph_select))
  expect_equal(n_selected, 0L,
    label = paste0("select_module='nonexistent_module' must yield 0-node graph; got ", n_selected))
})

# ---------------------------------------------------------------------------
# L2 — get_graph_adjacency must honour edge weights
#
# Decision: HONOR THE DOCS (return weighted adjacency).
# Downstream: ggnetview_zipi immediately binarizes with (abs(mat) > 0) * 1L,
# so returning weights does not break any caller. The fallback in
# adjacency_from_graph() already passes attr = "weight", confirming the intent.
# ---------------------------------------------------------------------------
test_that("L2: get_graph_adjacency returns weighted adjacency when graph has edge weights", {
  g <- make_test_graph(mod_type = "character", weighted = TRUE)
  # Edges: A-B=0.9, B-C=0.5, C-D=0.3, D-E=0.7

  adj <- get_graph_adjacency(g)

  expect_true(is.matrix(adj),
    label = "get_graph_adjacency must return a matrix")

  # Row/col names must be set
  expect_false(is.null(rownames(adj)),
    label = "adjacency matrix must have rownames")
  expect_equal(rownames(adj), colnames(adj),
    label = "rownames and colnames must match")

  # Edge A-B has weight 0.9; the matrix entry must reflect that, NOT just 1
  expect_equal(adj["A", "B"], 0.9,
    tolerance = 1e-9,
    label = paste0("adj[A,B] should be 0.9 (weight); got ", adj["A", "B"]))
  expect_equal(adj["B", "A"], 0.9,
    tolerance = 1e-9,
    label = "matrix must be symmetric for undirected graph")

  # Non-edge entries must be 0
  expect_equal(adj["A", "C"], 0,
    tolerance = 1e-9,
    label = "non-adjacent pair A-C must be 0")
})

test_that("L2-unweighted: get_graph_adjacency returns 0/1 when graph has no edge weights", {
  g <- make_test_graph(mod_type = "character", weighted = FALSE)

  adj <- get_graph_adjacency(g)

  expect_true(is.matrix(adj))
  # All non-zero entries should be exactly 1
  nonzero_vals <- adj[adj != 0]
  expect_true(all(nonzero_vals == 1L),
    label = paste0("unweighted graph: all non-zero adjacency entries must be 1; got ",
                   paste(unique(nonzero_vals), collapse = ", ")))
})
