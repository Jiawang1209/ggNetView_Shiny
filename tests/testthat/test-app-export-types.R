source(testthat::test_path("../../R/app_exports.R"))

test_that("graph export formats include graph-specific artifacts", {
  formats <- export_formats_for_type("graph")
  expect_true(all(c("rds", "nodes_csv", "edges_csv", "adjacency_csv", "params_json") %in% formats))
  expect_false(any(c("png", "pdf") %in% formats))
})

test_that("result export formats include table artifacts", {
  formats <- export_formats_for_type("result")
  expect_true(all(c("csv", "rds", "params_json") %in% formats))
})

test_that("plot export formats remain plot-only for images", {
  formats <- export_formats_for_type("plot")
  expect_true(all(c("png", "pdf") %in% formats))
})

test_that("graph CSV writers export nodes, edges, and adjacency", {
  graph <- igraph::make_ring(3)
  graph <- igraph::set_vertex_attr(graph, "name", value = c("A", "B", "C"))
  graph <- igraph::set_edge_attr(graph, "weight", value = c(0.2, 0.4, 0.6))

  nodes_path <- tempfile(fileext = ".csv")
  edges_path <- tempfile(fileext = ".csv")
  adjacency_path <- tempfile(fileext = ".csv")

  write_graph_nodes_csv(graph, nodes_path)
  write_graph_edges_csv(graph, edges_path)
  write_graph_adjacency_csv(graph, adjacency_path)

  expect_equal(nrow(utils::read.csv(nodes_path)), 3L)
  expect_equal(nrow(utils::read.csv(edges_path)), 3L)
  expect_equal(nrow(utils::read.csv(adjacency_path)), 3L)
})
