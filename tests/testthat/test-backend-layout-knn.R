# Test: H1 – k_nn must be clamped to n-1 before FNN::get.knn
# Reproduces the ANN error "k should be less than sample size!" that is emitted
# when k_nn >= nrow(layout) is passed to module_layout2 / module_layout3.
#
# We drive ggNetView() on a tiny graph (8 nodes) via the 'adjacent'
# layout.module path with default k_nn = 12 > n-1 = 7.  Before the fix, FNN
# emits the C-level ANN warning; after the fix it must not.

if (!exists("module_layout3")) pkgload::load_all(".", quiet = TRUE)

test_that("H1: no 'k should be less than sample size' warning on small graphs (adjacent layout)", {
  set.seed(42)

  # Build a small ring igraph with 8 nodes; default k_nn = 12 > n-1 = 7.
  # Named vertices are required so that get_location() can select the 'name' column.
  ig <- igraph::make_ring(8)
  igraph::V(ig)$name <- paste0("n", seq_len(8))
  igraph::E(ig)$weight <- 0.5

  # Convert to ggNetView tbl_graph (runs community detection -> adds modularity3)
  g <- build_graph_from_igraph(igraph = ig, module.method = "Fast_greedy", seed = 42)

  # Before the fix, FNN::get.knn(xy, k=12) on 8 points emits:
  #   Warning: k should be less than sample size!
  # After the fix the clamp k_nn <- min(k_nn, nrow(layout)-1) prevents it.
  expect_no_warning(
    {
      result <- ggNetView(
        graph_obj     = g,
        layout        = "fr",
        layout.module = "adjacent",
        k_nn          = 12,      # intentionally larger than n-1 = 7
        label         = FALSE
      )
    },
    message = "k should be less than sample size"
  )

  # Confirm the call produced a non-NULL result
  expect_true(!is.null(result))
})
