source(testthat::test_path("../../R/app_validation.R"))
source(testthat::test_path("../../R/app_adapters.R"))
source(testthat::test_path("../../R/app_graph_builders.R"))

read_phase2_fixture <- function(name, row_names = TRUE) {
  path <- testthat::test_path("../../inst/extdata", name)
  if (row_names) {
    utils::read.csv(path, row.names = 1, check.names = FALSE)
  } else {
    utils::read.csv(path, check.names = FALSE)
  }
}

test_that("builder modes are discoverable", {
  modes <- graph_builder_modes()
  expect_true(all(c(
    "matrix",
    "matrix_rmt",
    "edge_table",
    "adjacency",
    "double_matrix",
    "multi_matrix",
    "wgcna_tom",
    "consensus"
  ) %in% unname(modes)))
})

test_that("matrix graph builder returns app_result", {
  mat <- read_phase2_fixture("phase2_example_matrix.csv")
  result <- safe_graph_builder(
    mode = "matrix",
    inputs = list(matrix = mat),
    params = list(method = "cor", cor.method = "pearson", r.threshold = 0.2, p.threshold = 1)
  )

  expect_true(is.list(result))
  expect_true(isTRUE(result$ok), info = result$trace %||% result$message)
  expect_s3_class(result$value, "igraph")
})

test_that("edge table graph builder returns app_result", {
  edges <- read_phase2_fixture("phase2_example_edges.csv", row_names = FALSE)
  result <- safe_graph_builder(
    mode = "edge_table",
    inputs = list(edge_table = edges),
    params = list()
  )

  expect_true(isTRUE(result$ok), info = result$trace %||% result$message)
  expect_s3_class(result$value, "igraph")
})

test_that("adjacency graph builder returns app_result", {
  adjacency <- read_phase2_fixture("phase2_example_adjacency.csv")
  result <- safe_graph_builder(
    mode = "adjacency",
    inputs = list(adjacency = adjacency),
    params = list()
  )

  expect_true(isTRUE(result$ok), info = result$trace %||% result$message)
  expect_s3_class(result$value, "igraph")
})
