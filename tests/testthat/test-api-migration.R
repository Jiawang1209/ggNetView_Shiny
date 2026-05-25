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

test_that("required API functions are exported by NAMESPACE", {
  namespace_lines <- readLines(test_path("../../NAMESPACE"), warn = FALSE)
  required_exports <- c(
    "build_graph_from_mat",
    "build_graph_from_adj_mat",
    "build_graph_from_df",
    "ggNetView",
    "get_network_topology",
    "build_graph_from_consensus",
    "build_graph_from_node_edge",
    "build_graph_from_stringdb",
    "get_node_centrality",
    "get_node_ivi",
    "get_sample_subgraph"
  )
  for (fn in required_exports) {
    expect_true(
      any(namespace_lines == paste0("export(", fn, ")")),
      info = paste(fn, "is exported")
    )
  }
})
