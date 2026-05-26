source(testthat::test_path("../../R/app_validation.R"))
source(testthat::test_path("../../R/app_adapters.R"))
source(testthat::test_path("../../R/app_graph_builders.R"))
source(testthat::test_path("../../R/app_graph_inspect.R"))

read_phase2_fixture <- function(name, row_names = TRUE) {
  path <- testthat::test_path("../../inst/extdata", name)
  if (row_names) {
    utils::read.csv(path, row.names = 1, check.names = FALSE)
  } else {
    utils::read.csv(path, check.names = FALSE)
  }
}

phase2_module_graph <- function() {
  edges <- read_phase2_fixture("phase2_example_edges.csv", row_names = FALSE)
  modules <- read_phase2_fixture("phase2_example_modules.csv", row_names = FALSE)
  result <- safe_graph_builder(
    mode = "edge_table",
    inputs = list(edge_table = edges, module_table = modules),
    params = list()
  )
  testthat::expect_true(isTRUE(result$ok), info = result$trace %||% result$message)
  result$value
}

test_that("safe_graph_info returns node and edge info", {
  graph <- phase2_module_graph()
  result <- safe_graph_info(graph)

  expect_true(isTRUE(result$ok), info = result$trace %||% result$message)
  expect_true(all(c("node_info", "edge_info") %in% names(result$value)))
  expect_gt(nrow(result$value$node_info), 0)
  expect_gt(nrow(result$value$edge_info), 0)
})

test_that("module choices and module subgraph use ggNetView helpers", {
  graph <- phase2_module_graph()
  choices <- graph_module_choices(graph)

  expect_true(all(c("A", "B", "C") %in% unname(choices)))

  result <- safe_module_subgraph(graph, select_module = "A")
  expect_true(isTRUE(result$ok), info = result$trace %||% result$message)
  expect_s3_class(subgraph_selected_graph(result$value), "igraph")
  expect_true(all(c("Module", "Number") %in% names(subgraph_stat_table(result$value, "stat_module"))))
})

test_that("sample subgraph uses matrix sample selection", {
  graph <- phase2_module_graph()
  matrix <- read_phase2_fixture("phase2_example_matrix.csv")

  result <- safe_sample_subgraph(
    graph,
    matrix = matrix,
    select_sample = c("S1", "S2"),
    min_abundance = 0,
    combine = "union"
  )

  expect_true(isTRUE(result$ok), info = result$trace %||% result$message)
  expect_s3_class(subgraph_selected_graph(result$value), "igraph")
  expect_true(all(c("Sample", "Node", "Edge", "Status") %in% names(subgraph_stat_table(result$value, "stat_sample"))))
})
