## Tests for get_node_centrality correctness and robustness
## Covers audit findings H2 (weight direction), M2 (NA weights), L4 (NaN closeness)

if (!exists("get_node_centrality")) pkgload::load_all(".", quiet = TRUE)

library(igraph)
library(tidygraph)

# ---------------------------------------------------------------------------
# Helper: build a tbl_graph from an igraph object
# ---------------------------------------------------------------------------
ig_to_tbl <- function(ig) tidygraph::as_tbl_graph(ig)

# ---------------------------------------------------------------------------
# H2 — strength-semantic measures must rank strong connections higher
#
# Graph: a --(0.9)-- b --(0.9)-- c --(0.1)-- d
# 'b' and 'c' are the hub nodes; 'd' is weakly attached.
# With strength weights (raw |correlation|), b/c must have HIGHER
# Eigenvector and PageRank than the peripheral node d.
# The bug inverts this: 1/weight makes d look most central.
# ---------------------------------------------------------------------------
test_that("H2: weighted=TRUE uses strength weights for Eigenvector (strong node > weak node)", {
  ig <- igraph::graph_from_literal(a - b - c - d)
  igraph::E(ig)$weight <- c(0.9, 0.9, 0.1)   # a-b=0.9, b-c=0.9, c-d=0.1

  g <- ig_to_tbl(ig)
  result <- get_node_centrality(
    g,
    measures  = c("Eigenvector"),
    weighted  = TRUE,
    overwrite = TRUE
  )

  node_tbl <- result %>%
    tidygraph::activate(nodes) %>%
    tidygraph::as_tibble()

  eig_b <- node_tbl$Eigenvector[node_tbl$name == "b"]
  eig_d <- node_tbl$Eigenvector[node_tbl$name == "d"]

  # b is a hub (strong connections); d is peripheral (weak connection).
  # Strength semantics: Eigenvector of b > Eigenvector of d
  expect_gt(eig_b, eig_d,
    label = paste0("Eigenvector b=", round(eig_b, 4),
                   " must exceed Eigenvector d=", round(eig_d, 4)))
})

test_that("H2: weighted=TRUE uses strength weights for PageRank (strong node > weak node)", {
  ig <- igraph::graph_from_literal(a - b - c - d)
  igraph::E(ig)$weight <- c(0.9, 0.9, 0.1)

  g <- ig_to_tbl(ig)
  result <- get_node_centrality(
    g,
    measures  = c("PageRank"),
    weighted  = TRUE,
    overwrite = TRUE
  )

  node_tbl <- result %>%
    tidygraph::activate(nodes) %>%
    tidygraph::as_tibble()

  pr_b <- node_tbl$PageRank[node_tbl$name == "b"]
  pr_d <- node_tbl$PageRank[node_tbl$name == "d"]

  # b is a hub; d is weakly attached. PageRank of b > PageRank of d.
  expect_gt(pr_b, pr_d,
    label = paste0("PageRank b=", round(pr_b, 4),
                   " must exceed PageRank d=", round(pr_d, 4)))
})

# ---------------------------------------------------------------------------
# M2 — NA edge weights must produce a warning, not a crash
# ---------------------------------------------------------------------------
test_that("M2: NA edge weight with weighted=TRUE produces a warning (not an error)", {
  ig <- igraph::graph_from_literal(a - b - c)
  igraph::E(ig)$weight <- c(0.5, NA_real_)

  g <- ig_to_tbl(ig)

  # Should warn and fall back to unweighted — must NOT error
  expect_warning(
    result <- get_node_centrality(g, measures = "Eigenvector", weighted = TRUE),
    regexp = "non-finite|NA|falling back",
    ignore.case = TRUE
  )

  # Result must still be a tbl_graph with the Eigenvector column present
  expect_s3_class(result, "tbl_graph")
  node_tbl <- result %>%
    tidygraph::activate(nodes) %>%
    tidygraph::as_tibble()
  expect_true("Eigenvector" %in% colnames(node_tbl))
  expect_false(any(is.nan(node_tbl$Eigenvector)))
})

# ---------------------------------------------------------------------------
# L4 — Closeness for isolated / singleton-component nodes must be NA, not NaN
# ---------------------------------------------------------------------------
test_that("L4: Closeness returns NA (not NaN) for isolated nodes in disconnected graphs", {
  # Graph: a-b connected; c is isolated
  ig <- igraph::graph_from_literal(a - b)
  ig <- igraph::add_vertices(ig, 1, name = "c")   # c is isolated

  g <- ig_to_tbl(ig)
  result <- get_node_centrality(
    g,
    measures  = "Closeness",
    weighted  = FALSE,
    overwrite = TRUE
  )

  node_tbl <- result %>%
    tidygraph::activate(nodes) %>%
    tidygraph::as_tibble()

  closeness_c <- node_tbl$Closeness[node_tbl$name == "c"]

  # Must be NA, never NaN
  expect_true(is.na(closeness_c),
    label = paste0("Closeness for isolated node c must be NA; got ", closeness_c))
  expect_false(is.nan(closeness_c),
    label = "Closeness for isolated node c must not be NaN")
})
