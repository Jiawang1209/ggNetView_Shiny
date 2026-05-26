source(test_path("../../R/app_validation.R"))
source(test_path("../../R/app_adapters.R"))
source(test_path("../../R/app_graph_inspect.R"))
source(test_path("../../R/app_topology_adapters.R"))
source(test_path("../../R/app_compare_environment.R"))
source(test_path("../../inst/app/modules/mod_graph_explorer.R"))
source(test_path("../../inst/app/modules/mod_topology_results.R"))
source(test_path("../../inst/app/modules/mod_compare_environment.R"))

test_that("graph explorer extracts node and edge tables", {
  graph <- igraph::make_ring(3)
  graph <- igraph::set_vertex_attr(graph, "name", value = c("A", "B", "C"))
  graph <- igraph::set_edge_attr(graph, "weight", value = c(0.1, 0.2, 0.3))

  nodes <- graph_nodes_table(graph)
  edges <- graph_edges_table(graph)

  expect_equal(nodes$name, c("A", "B", "C"))
  expect_equal(nrow(edges), 3L)
  expect_true(all(c("from", "to", "weight") %in% names(edges)))
})

test_that("topology result table extracts ggNetView topology payload", {
  topology <- data.frame(metric = c("nodes", "edges"), value = c(3, 2))
  robustness <- data.frame(step = 1:3, score = c(1, 0.5, 0))

  table <- topology_result_table(list(topology = topology, Robustness = robustness))

  expect_equal(table, topology)
})
