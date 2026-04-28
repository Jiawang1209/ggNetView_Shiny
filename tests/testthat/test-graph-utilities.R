test_that("get_graph_nodes returns a data frame with expected columns", {
  data(ppi_example, package = "ggNetView")
  g <- build_graph_from_df(
    df = ppi_example$ppi,
    node_annotation = ppi_example$annotation
  )

  nodes <- get_graph_nodes(g)

  expect_s3_class(nodes, "data.frame")
  expect_true("name" %in% colnames(nodes))
  expect_true("Modularity" %in% colnames(nodes))
  expect_true("Degree" %in% colnames(nodes))
  expect_gt(nrow(nodes), 0)
})

test_that("get_graph_adjacency returns a square numeric matrix", {
  data(ppi_example, package = "ggNetView")
  g <- build_graph_from_df(
    df = ppi_example$ppi,
    node_annotation = ppi_example$annotation
  )

  adj <- get_graph_adjacency(g)

  expect_true(is.matrix(adj))
  expect_equal(nrow(adj), ncol(adj))
  expect_equal(nrow(adj), igraph::gorder(g))
  expect_true(is.numeric(adj))
})

test_that("get_info_from_graph returns a list with node and edge info", {
  data(ppi_example, package = "ggNetView")
  g <- build_graph_from_df(
    df = ppi_example$ppi,
    node_annotation = ppi_example$annotation
  )

  info <- get_info_from_graph(g)

  expect_true(is.list(info))
  expect_true(length(info) >= 2)
})

test_that("get_subgraph returns a subgraph for a selected module", {
  data(ppi_example, package = "ggNetView")
  g <- build_graph_from_df(
    df = ppi_example$ppi,
    node_annotation = ppi_example$annotation
  )

  nodes <- get_graph_nodes(g)
  first_mod <- as.character(nodes$Modularity[1])

  sg <- get_subgraph(g, select_module = first_mod)

  expect_true(!is.null(sg))
})

test_that("order_graph reorders modules by supplied order", {
  data(ppi_example, package = "ggNetView")
  g <- build_graph_from_df(
    df = ppi_example$ppi,
    node_annotation = ppi_example$annotation
  )

  nodes <- get_graph_nodes(g)
  mod_levels <- unique(as.character(nodes$Modularity))

  ordered <- order_graph(g, order = mod_levels)

  expect_s3_class(ordered, "tbl_graph")
  expect_equal(igraph::gorder(ordered), igraph::gorder(g))
})

test_that("trans_adjacency_matrix_to_df converts matrix to edge data frame", {
  mat <- matrix(c(0, 0.5, 0.3, 0.5, 0, 0.7, 0.3, 0.7, 0), nrow = 3)
  rownames(mat) <- colnames(mat) <- c("A", "B", "C")

  df <- trans_adjacency_matrix_to_df(mat)

  expect_s3_class(df, "data.frame")
  expect_true(all(c("from", "to") %in% colnames(df)))
  expect_gt(nrow(df), 0)
})
