source(testthat::test_path("../../R/app_validation.R"))
source(testthat::test_path("../../R/app_adapters.R"))
source(testthat::test_path("../../R/app_graph_builders.R"))
source(testthat::test_path("../../R/app_topology_adapters.R"))

read_phase2_fixture <- function(name, row_names = TRUE) {
  path <- testthat::test_path("../../inst/extdata", name)
  if (row_names) {
    utils::read.csv(path, row.names = 1, check.names = FALSE)
  } else {
    utils::read.csv(path, check.names = FALSE)
  }
}

phase2_topology_graph <- function() {
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

test_that("node centrality returns node result table", {
  result <- safe_node_centrality(
    phase2_topology_graph(),
    measures = c("Betweenness", "Closeness", "PageRank"),
    weighted = FALSE
  )

  expect_true(isTRUE(result$ok), info = result$trace %||% result$message)
  expect_true(all(c("Betweenness", "Closeness", "PageRank") %in% names(result$value)))
  expect_gt(nrow(result$value), 0)
})

test_that("Zi-Pi returns keystone classification table", {
  result <- safe_zipi(phase2_topology_graph())

  expect_true(isTRUE(result$ok), info = result$trace %||% result$message)
  expect_true(all(c("within_module_connectivities", "among_module_connectivities", "type") %in% names(result$value)))
  expect_gt(nrow(result$value), 0)
})

test_that("IVI reports dependency result clearly", {
  result <- safe_node_ivi(phase2_topology_graph(), ncores = 1L)

  if (requireNamespace("influential", quietly = TRUE)) {
    expect_true(isTRUE(result$ok), info = result$trace %||% result$message)
    expect_true("IVI" %in% names(result$value))
  } else {
    expect_false(result$ok)
    expect_match(result$trace, "influential")
  }
})
