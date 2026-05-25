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

test_that("edge table graph builder accepts module metadata", {
  edges <- read_phase2_fixture("phase2_example_edges.csv", row_names = FALSE)
  modules <- read_phase2_fixture("phase2_example_modules.csv", row_names = FALSE)
  result <- safe_graph_builder(
    mode = "edge_table",
    inputs = list(edge_table = edges, module_table = modules),
    params = list()
  )

  expect_true(isTRUE(result$ok), info = result$trace %||% result$message)
  expect_s3_class(result$value, "igraph")
  expect_true("Modularity" %in% igraph::vertex_attr_names(result$value))
})

test_that("adjacency graph builder accepts module metadata", {
  adjacency <- read_phase2_fixture("phase2_example_adjacency.csv")
  modules <- read_phase2_fixture("phase2_example_modules.csv", row_names = FALSE)
  result <- safe_graph_builder(
    mode = "adjacency",
    inputs = list(adjacency = adjacency, module_table = modules),
    params = list()
  )

  expect_true(isTRUE(result$ok), info = result$trace %||% result$message)
  expect_s3_class(result$value, "igraph")
  expect_true("Modularity" %in% igraph::vertex_attr_names(result$value))
})

test_that("double matrix, multi matrix, WGCNA/TOM, and consensus builders return graphs", {
  mat_a <- read_phase2_fixture("phase2_example_matrix.csv")
  mat_b <- read_phase2_fixture("phase2_example_matrix_b.csv")
  adjacency <- read_phase2_fixture("phase2_example_adjacency.csv")
  tom <- read_phase2_fixture("phase2_example_tom.csv")
  modules <- read_phase2_fixture("phase2_example_modules.csv", row_names = FALSE)

  double_result <- safe_graph_builder(
    mode = "double_matrix",
    inputs = list(matrix_a = mat_a, matrix_b = mat_b),
    params = list()
  )
  expect_true(isTRUE(double_result$ok), info = double_result$trace %||% double_result$message)
  expect_s3_class(double_result$value, "igraph")

  multi_result <- safe_graph_builder(
    mode = "multi_matrix",
    inputs = list(matrices = list(otu = mat_a, gene = mat_b)),
    params = list()
  )
  expect_true(isTRUE(multi_result$ok), info = multi_result$trace %||% multi_result$message)
  expect_s3_class(multi_result$value, "igraph")

  wgcna_result <- safe_graph_builder(
    mode = "wgcna_tom",
    inputs = list(tom = tom, module_table = modules),
    params = list(threshold = 0.2)
  )
  expect_true(isTRUE(wgcna_result$ok), info = wgcna_result$trace %||% wgcna_result$message)
  expect_s3_class(wgcna_result$value, "igraph")

  consensus_result <- safe_graph_builder(
    mode = "consensus",
    inputs = list(graphs_or_adjacency = list(a = adjacency, b = adjacency)),
    params = list(method = "intersection", binarize = "threshold", binarize_threshold = 0.1)
  )
  expect_true(isTRUE(consensus_result$ok), info = consensus_result$trace %||% consensus_result$message)
  expect_s3_class(consensus_result$value, "igraph")
})
