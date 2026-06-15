## Tests for ggNetView_multi_link correctness (audit H3, L10, L11, L12)
## H3  — jitter uses unseeded RNG → non-deterministic output
## L10 — jitter block nested inside outer anchoring loop (runs N times)
## L11 — module-link group matching uses str_detect (regex, not exact)
## L12 — safe_multi_network_compare validates igraph but forwards as tbl_graph

if (!exists("ggNetView_multi_link")) pkgload::load_all(".", quiet = TRUE)

library(igraph)
library(tidygraph)

# ---------------------------------------------------------------------------
# Helper: build a minimal tbl_graph with required columns
#   (Modularity, Degree, Strength, name)
# ---------------------------------------------------------------------------
make_mini_tbl_graph <- function(n_nodes = 12, seed = 42) {
  set.seed(seed)
  # Erdos-Renyi graph with enough nodes to form >=2 modules
  ig <- igraph::sample_gnm(n = n_nodes, m = n_nodes * 2L, directed = FALSE, loops = FALSE)
  igraph::V(ig)$name <- paste0("N", seq_len(n_nodes))
  igraph::E(ig)$weight <- runif(igraph::ecount(ig), 0.5, 1)
  igraph::E(ig)$correlation <- igraph::E(ig)$weight
  igraph::E(ig)$corr_direction <- sample(c("Positive", "Negative"), igraph::ecount(ig), replace = TRUE)

  # Assign two balanced modules
  igraph::V(ig)$modularity2 <- ifelse(seq_len(n_nodes) <= n_nodes %/% 2L, "M1", "M2")
  igraph::V(ig)$modularity  <- igraph::V(ig)$modularity2
  igraph::V(ig)$modularity3 <- igraph::V(ig)$modularity2

  g <- tidygraph::as_tbl_graph(ig) %>%
    tidygraph::mutate(
      modularity  = factor(modularity2),
      modularity2 = factor(modularity2, levels = c("M1", "M2"), ordered = TRUE),
      modularity3 = as.character(modularity2),
      Modularity  = modularity2,
      Degree      = tidygraph::centrality_degree(mode = "out"),
      Strength    = tidygraph::centrality_degree(weights = weight)
    ) %>%
    tidygraph::arrange(Modularity, dplyr::desc(Degree))

  g
}

# Build two small graphs for comparison
g1 <- make_mini_tbl_graph(n_nodes = 12, seed = 1)
g2 <- make_mini_tbl_graph(n_nodes = 12, seed = 2)
graph_obj_list_test <- list(GroupA = g1, GroupB = g2)

# Shared minimal call params (no jitter to establish a baseline for L12;
# jitter = TRUE used in the determinism test)
base_params <- list(
  graph_obj_list = graph_obj_list_test,
  layout         = "fr",
  layout.module  = "random",
  comparisons    = FALSE,   # skip expensive module comparison for speed
  seed           = 123L
)

# ---------------------------------------------------------------------------
# H3 — determinism: same seed must produce identical node coordinates
# ---------------------------------------------------------------------------
test_that("H3: ggNetView_multi_link produces identical output with same seed (jitter=TRUE)", {

  params_jitter <- utils::modifyList(base_params, list(jitter = TRUE, jitter_sd = 0.05))

  res1 <- do.call(ggNetView_multi_link, params_jitter)
  res2 <- do.call(ggNetView_multi_link, params_jitter)

  # Extract node coordinates from each group's layout data
  coords1 <- lapply(seq_along(res1$graph), function(i) {
    grp <- names(res1$graph)[[i]]
    # The function returns graph_info embedded in the plot's data layers;
    # the most stable extraction is from the ggplot build data for geom_point.
    build1 <- ggplot2::ggplot_build(res1$p)
    build1$data
  })
  coords2 <- lapply(seq_along(res2$graph), function(i) {
    build2 <- ggplot2::ggplot_build(res2$p)
    build2$data
  })

  # All numeric columns in the built plot data must be identical
  for (layer_idx in seq_along(coords1[[1]])) {
    d1 <- coords1[[1]][[layer_idx]]
    d2 <- coords2[[1]][[layer_idx]]
    num_cols <- names(d1)[vapply(d1, is.numeric, logical(1))]
    for (col in num_cols) {
      expect_equal(
        d1[[col]], d2[[col]],
        label = paste0("layer ", layer_idx, " column '", col, "'"),
        info  = "H3: ggNetView_multi_link must be deterministic when seed is fixed"
      )
    }
  }
})

# ---------------------------------------------------------------------------
# H3 — determinism (simpler): same seed, different seed must DIFFER
# (anti-gaming guard: if jitter is disabled this test should be skipped)
# ---------------------------------------------------------------------------
test_that("H3: different seeds produce different output (jitter is active, not removed)", {

  params_s1 <- utils::modifyList(base_params, list(jitter = TRUE, jitter_sd = 0.05, seed = 111L))
  params_s2 <- utils::modifyList(base_params, list(jitter = TRUE, jitter_sd = 0.05, seed = 999L))

  res1 <- do.call(ggNetView_multi_link, params_s1)
  res2 <- do.call(ggNetView_multi_link, params_s2)

  b1 <- ggplot2::ggplot_build(res1$p)$data
  b2 <- ggplot2::ggplot_build(res2$p)$data

  # Find at least one numeric coordinate column that differs between seeds
  found_difference <- FALSE
  for (layer_idx in seq_along(b1)) {
    d1 <- b1[[layer_idx]]
    d2 <- b2[[layer_idx]]
    num_cols <- names(d1)[vapply(d1, is.numeric, logical(1))]
    for (col in c("x", "y")) {
      if (col %in% num_cols && !isTRUE(all.equal(d1[[col]], d2[[col]]))) {
        found_difference <- TRUE
        break
      }
    }
    if (found_difference) break
  }
  expect_true(found_difference,
    label = "H3 anti-gaming: different seeds must yield different coordinates (jitter must stay active)")
})

# ---------------------------------------------------------------------------
# L12 — adapter: safe_multi_network_compare accepts plain igraph inputs
#        (must coerce to tbl_graph internally rather than crashing)
# ---------------------------------------------------------------------------
test_that("L12: safe_multi_network_compare coerces igraph to tbl_graph without error", {

  if (!exists("safe_multi_network_compare")) pkgload::load_all(".", quiet = TRUE)

  # Build two plain igraph objects (NOT tbl_graph)
  ig1 <- igraph::graph_from_literal(a - b - c - d - e - a - c)
  igraph::V(ig1)$Modularity  <- c("M1", "M1", "M1", "M2", "M2")
  igraph::V(ig1)$modularity2 <- igraph::V(ig1)$Modularity
  igraph::V(ig1)$modularity3 <- igraph::V(ig1)$Modularity
  igraph::E(ig1)$weight <- rep(0.8, igraph::ecount(ig1))
  igraph::E(ig1)$correlation <- igraph::E(ig1)$weight
  igraph::E(ig1)$corr_direction <- "Positive"

  ig2 <- igraph::graph_from_literal(a - b - c - d - e - b - d)
  igraph::V(ig2)$Modularity  <- c("M1", "M1", "M2", "M2", "M2")
  igraph::V(ig2)$modularity2 <- igraph::V(ig2)$Modularity
  igraph::V(ig2)$modularity3 <- igraph::V(ig2)$Modularity
  igraph::E(ig2)$weight <- rep(0.7, igraph::ecount(ig2))
  igraph::E(ig2)$correlation <- igraph::E(ig2)$weight
  igraph::E(ig2)$corr_direction <- "Positive"

  graphs <- list(GroupA = ig1, GroupB = ig2)

  params <- list(
    layout        = "fr",
    layout.module = "random",
    comparisons   = FALSE,
    seed          = 123L
  )

  # Before fix: this fails with "must be a tbl_graph object"
  # After fix: coercion happens inside safe_multi_network_compare
  result <- safe_multi_network_compare(graphs, params = params)

  expect_true(result$ok,
    label = "L12: safe_multi_network_compare must succeed with plain igraph inputs")
})
