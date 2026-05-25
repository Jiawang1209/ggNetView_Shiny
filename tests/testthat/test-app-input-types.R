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

test_that("detect_upload_type recognizes node tables for node+edge workflows", {
  source(testthat::test_path("../../R/app_validation.R"))

  nodes <- data.frame(
    id = c("OTU1", "OTU2", "OTU3", "OTU7"),
    label = c("One", "Two", "Three", "Isolated"),
    type = c("A", "A", "B", "Z"),
    stringsAsFactors = FALSE
  )

  expect_equal(detect_upload_type(nodes), "node_table")
})

test_that("detect_upload_type recognizes STRINGDB/PPI tables", {
  source(testthat::test_path("../../R/app_validation.R"))

  stringdb <- data.frame(
    node1 = c("P1", "P1", "P2"),
    node2 = c("P2", "P3", "P4"),
    combined_score = c(0.92, 0.55, 0.81),
    coexpression = c(0.2, 0.1, 0.3),
    experimentally_determined_interaction = c(0.8, 0.4, 0.7),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  expect_equal(detect_upload_type(stringdb), "stringdb")
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

test_that("validated_upload_value honors node table override", {
  source(testthat::test_path("../../R/app_validation.R"))
  source(testthat::test_path("../../inst/app/modules/mod_data_hub.R"))

  table <- data.frame(
    name = c("OTU1", "OTU2", "OTU7"),
    label = c("Node 1", "Node 2", "Isolated"),
    type = c("core", "core", "extra"),
    stringsAsFactors = FALSE
  )

  override <- validated_upload_value(table, requested_type = "node_table")

  expect_equal(override$type, "node_table")
  expect_true(override$validation$ok)
})

test_that("validated_upload_value honors STRINGDB/PPI override", {
  source(testthat::test_path("../../R/app_validation.R"))
  source(testthat::test_path("../../inst/app/modules/mod_data_hub.R"))

  table <- data.frame(
    node1 = c("P1", "P2"),
    node2 = c("P2", "P3"),
    combined_score = c(0.8, 0.9),
    stringsAsFactors = FALSE
  )

  override <- validated_upload_value(table, requested_type = "stringdb")

  expect_equal(override$type, "stringdb")
  expect_true(override$validation$ok)
})
