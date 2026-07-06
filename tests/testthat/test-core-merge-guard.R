# Source-level guard for the new-ggNetView core merge.
# These tests read source files only (no pkgload::load_all), so they run green
# even when heavy Imports (WGCNA, tidygraph, ggraph, ...) are not installed.

test_that("audit-H1 k_nn clamp survives the new-ggNetView merge", {
  gg_src <- readLines(testthat::test_path("../../R/ggnetview.R"), warn = FALSE)
  geo_src <- readLines(testthat::test_path("../../R/get_geo_neighbors.R"), warn = FALSE)

  expect_true(any(grepl("min(k_nn, k_nn_cap)", gg_src, fixed = TRUE)),
              info = "ggnetview.R must clamp k_nn_try to k_nn_cap (audit H1)")
  expect_equal(sum(grepl("audit H1", geo_src, fixed = TRUE)), 2L)
})

test_that("new ggNetView functions are vendored and exported", {
  expect_true(file.exists(testthat::test_path("../../R/gglink_heatmaps_2.R")))
  expect_true(file.exists(testthat::test_path("../../R/ggnetview_subgraph.R")))

  ns <- readLines(testthat::test_path("../../NAMESPACE"), warn = FALSE)
  expect_true(any(grepl("export(gglink_heatmaps_2)", ns, fixed = TRUE)))
  expect_true(any(grepl("export(ggnetview_subgraph)", ns, fixed = TRUE)))
  # The Shiny distribution keeps launch_ggNetView even though upstream dropped it.
  expect_true(any(grepl("export(launch_ggNetView)", ns, fixed = TRUE)))
})
