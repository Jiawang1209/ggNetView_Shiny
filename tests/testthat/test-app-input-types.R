test_that("phase2 example files exist and are readable", {
  paths <- testthat::test_path("../../inst/extdata", c(
    "phase2_example_matrix.csv",
    "phase2_example_matrix_b.csv",
    "phase2_example_rmt_matrix.csv",
    "phase2_example_edges.csv",
    "phase2_example_modules.csv",
    "phase2_example_sample_metadata.csv",
    "phase2_example_adjacency.csv",
    "phase2_example_tom.csv"
  ))

  expect_true(all(file.exists(paths)))

  matrix_a <- utils::read.csv(paths[[1]], row.names = 1, check.names = FALSE)
  rmt_matrix <- utils::read.csv(paths[[3]], row.names = 1, check.names = FALSE)
  edges <- utils::read.csv(paths[[4]], check.names = FALSE)
  expect_equal(nrow(matrix_a), 6)
  expect_equal(ncol(matrix_a), 5)
  expect_equal(nrow(rmt_matrix), 120)
  expect_equal(ncol(rmt_matrix), 30)
  expect_true(all(c("source", "target", "weight") %in% names(edges)))
})

test_that("detect_upload_type recognizes phase2 table classes", {
  source(testthat::test_path("../../R/app_validation.R"))

  matrix_a <- utils::read.csv(testthat::test_path("../../inst/extdata/phase2_example_matrix.csv"), row.names = 1, check.names = FALSE)
  edges <- utils::read.csv(testthat::test_path("../../inst/extdata/phase2_example_edges.csv"), check.names = FALSE)
  modules <- utils::read.csv(testthat::test_path("../../inst/extdata/phase2_example_modules.csv"), check.names = FALSE)
  sample_metadata <- utils::read.csv(testthat::test_path("../../inst/extdata/phase2_example_sample_metadata.csv"), check.names = FALSE)
  adjacency <- utils::read.csv(testthat::test_path("../../inst/extdata/phase2_example_adjacency.csv"), row.names = 1, check.names = FALSE)
  tom <- utils::read.csv(testthat::test_path("../../inst/extdata/phase2_example_tom.csv"), row.names = 1, check.names = FALSE)

  expect_equal(detect_upload_type(matrix_a), "matrix")
  expect_equal(detect_upload_type(edges), "edge_table")
  expect_equal(detect_upload_type(modules), "module_table")
  expect_equal(detect_upload_type(sample_metadata), "sample_metadata")
  expect_equal(detect_upload_type(adjacency), "adjacency")
  expect_equal(detect_upload_type(tom), "wgcna_tom")
})

test_that("read_user_table preserves edge and module schema columns", {
  source(testthat::test_path("../../R/app_validation.R"))

  edge_path <- testthat::test_path("../../inst/extdata/phase2_example_edges.csv")
  module_path <- testthat::test_path("../../inst/extdata/phase2_example_modules.csv")

  edges <- read_user_table(edge_path)
  modules <- read_user_table(module_path)

  expect_true(all(c("source", "target", "weight") %in% names(edges)))
  expect_true(all(c("node", "module") %in% names(modules)))
  expect_equal(detect_upload_type(edges), "edge_table")
  expect_equal(detect_upload_type(modules), "module_table")
})

test_that("validated_upload_value honors manual type override", {
  source(testthat::test_path("../../R/app_validation.R"))
  source(testthat::test_path("../../inst/app/modules/mod_data_hub.R"))

  table <- data.frame(
    value = c("A", "B"),
    module = c("blue", "brown"),
    stringsAsFactors = FALSE
  )

  auto <- validated_upload_value(table, requested_type = "auto")
  override <- validated_upload_value(table, requested_type = "module_table")

  expect_equal(auto$type, "unknown")
  expect_equal(override$type, "module_table")
  expect_true(override$validation$ok)
})
