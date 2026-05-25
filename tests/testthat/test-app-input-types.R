test_that("phase2 example files exist and are readable", {
  paths <- testthat::test_path("../../inst/extdata", c(
    "phase2_example_matrix.csv",
    "phase2_example_matrix_b.csv",
    "phase2_example_edges.csv",
    "phase2_example_modules.csv",
    "phase2_example_adjacency.csv",
    "phase2_example_tom.csv"
  ))

  expect_true(all(file.exists(paths)))

  matrix_a <- utils::read.csv(paths[[1]], row.names = 1, check.names = FALSE)
  edges <- utils::read.csv(paths[[3]], check.names = FALSE)
  expect_equal(nrow(matrix_a), 6)
  expect_equal(ncol(matrix_a), 5)
  expect_true(all(c("source", "target", "weight") %in% names(edges)))
})

test_that("detect_upload_type recognizes phase2 table classes", {
  source(testthat::test_path("../../R/app_validation.R"))

  matrix_a <- utils::read.csv(testthat::test_path("../../inst/extdata/phase2_example_matrix.csv"), row.names = 1, check.names = FALSE)
  edges <- utils::read.csv(testthat::test_path("../../inst/extdata/phase2_example_edges.csv"), check.names = FALSE)
  modules <- utils::read.csv(testthat::test_path("../../inst/extdata/phase2_example_modules.csv"), check.names = FALSE)
  adjacency <- utils::read.csv(testthat::test_path("../../inst/extdata/phase2_example_adjacency.csv"), row.names = 1, check.names = FALSE)
  tom <- utils::read.csv(testthat::test_path("../../inst/extdata/phase2_example_tom.csv"), row.names = 1, check.names = FALSE)

  expect_equal(detect_upload_type(matrix_a), "matrix")
  expect_equal(detect_upload_type(edges), "edge_table")
  expect_equal(detect_upload_type(modules), "module_table")
  expect_equal(detect_upload_type(adjacency), "adjacency")
  expect_equal(detect_upload_type(tom), "wgcna_tom")
})
