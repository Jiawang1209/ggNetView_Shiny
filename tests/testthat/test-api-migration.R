source(test_path("../../R/build_graph_from_mat.R"))
source(test_path("../../R/build_graph_from_adj_mat.R"))
source(test_path("../../R/build_graph_from_df.R"))
source(test_path("../../R/ggnetview.R"))
source(test_path("../../R/get_network_topology.R"))

test_that("new ggNetView API files are present in root R directory", {
  expect_true(file.exists(test_path("../../R/build_graph_from_consensus.R")))
  expect_true(file.exists(test_path("../../R/get_node_centrality.R")))
  expect_true(file.exists(test_path("../../R/get_node_ivi.R")))
  expect_true(file.exists(test_path("../../R/get_sample_subgraph.R")))
})

test_that("first milestone API functions are available", {
  expect_true(exists("build_graph_from_mat", mode = "function"))
  expect_true(exists("build_graph_from_adj_mat", mode = "function"))
  expect_true(exists("build_graph_from_df", mode = "function"))
  expect_true(exists("ggNetView", mode = "function"))
  expect_true(exists("get_network_topology", mode = "function"))
})
