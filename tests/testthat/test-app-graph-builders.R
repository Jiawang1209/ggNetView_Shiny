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
    "node_edge",
    "igraph",
    "stringdb",
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

test_that("RMT threshold workflow returns a threshold result for gallery RMT fixture", {
  mat <- read_phase2_fixture("phase2_example_rmt_matrix.csv")
  result <- safe_rmt_threshold(
    mat,
    params = list(transfrom.method = "none", method = "cor", cor.method = "pearson", min.mat.dim = 20, verbose = FALSE)
  )

  expect_true(isTRUE(result$ok), info = result$trace %||% result$message)
  expect_true(is.numeric(result$value$chosen_threshold))
  expect_s3_class(result$value$scores, "data.frame")
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

test_that("node+edge graph builder preserves isolated nodes", {
  edges <- read_phase2_fixture("phase2_example_edges.csv", row_names = FALSE)
  nodes <- data.frame(
    id = c(paste0("OTU", 1:6), "OTU7"),
    label = c(paste("OTU", 1:6), "Isolated OTU"),
    type = c(rep("observed", 6), "isolated"),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  result <- safe_graph_builder(
    mode = "node_edge",
    inputs = list(edge_table = edges, node_table = nodes),
    params = list(module.method = "Walktrap")
  )

  expect_true(isTRUE(result$ok), info = result$trace %||% result$message)
  expect_s3_class(result$value, "igraph")
  expect_true("OTU7" %in% igraph::V(result$value)$name)
  expect_equal(unname(igraph::degree(result$value)["OTU7"]), 0)
  expect_equal(igraph::V(result$value)$type[igraph::V(result$value)$name == "OTU7"], "isolated")
})

test_that("igraph graph builder standardizes existing graph objects", {
  mat <- read_phase2_fixture("phase2_example_matrix.csv")
  graph <- safe_graph_builder(
    mode = "matrix",
    inputs = list(matrix = mat),
    params = list(method = "cor", cor.method = "pearson", r.threshold = 0.2, p.threshold = 1)
  )$value

  result <- safe_graph_builder(
    mode = "igraph",
    inputs = list(graph = graph),
    params = list(use_existing_modules = TRUE, module.method = "Walktrap")
  )

  expect_true(isTRUE(result$ok), info = result$trace %||% result$message)
  expect_s3_class(result$value, "igraph")
  expect_true(all(c("Modularity", "Degree", "Strength") %in% igraph::vertex_attr_names(result$value)))
})

test_that("STRINGDB graph builder preserves evidence channel attributes", {
  stringdb <- data.frame(
    node1 = c("P1", "P1", "P2", "P3"),
    node2 = c("P2", "P3", "P4", "P4"),
    combined_score = c(0.92, 0.55, 0.81, 0.73),
    coexpression = c(0.2, 0.1, 0.3, 0.4),
    experimentally_determined_interaction = c(0.8, 0.4, 0.7, 0.6),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  result <- safe_graph_builder(
    mode = "stringdb",
    inputs = list(stringdb = stringdb),
    params = list(score_threshold = 0.7, module.method = "Walktrap")
  )

  expect_true(isTRUE(result$ok), info = result$trace %||% result$message)
  expect_s3_class(result$value, "igraph")
  expect_equal(igraph::ecount(result$value), 3)
  expect_true(all(c("combined_score", "coexpression", "experimentally_determined_interaction") %in% igraph::edge_attr_names(result$value)))
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
