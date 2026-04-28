test_that("ggnetview_zipi returns data and plot", {
  data(ppi_example, package = "ggNetView")
  g <- build_graph_from_df(
    df = ppi_example$ppi,
    node_annotation = ppi_example$annotation
  )

  nodes_tbl <- get_graph_nodes(g)
  adj_mat   <- get_graph_adjacency(g)

  zipi <- ggnetview_zipi(
    nodes_bulk     = nodes_tbl,
    z_bulk_mat     = adj_mat,
    modularity_col = "Modularity",
    degree_col     = "Degree"
  )

  expect_true(is.list(zipi))
  expect_true("data" %in% names(zipi))
  expect_true("plot" %in% names(zipi))

  expect_s3_class(zipi$data, "data.frame")
  expect_s3_class(zipi$plot, "ggplot")

  expect_true("within_module_connectivities" %in% colnames(zipi$data))
  expect_true("among_module_connectivities" %in% colnames(zipi$data))
  expect_true("type" %in% colnames(zipi$data))
})

test_that("ggnetview_zipi classifies nodes into valid roles", {
  data(ppi_example, package = "ggNetView")
  g <- build_graph_from_df(
    df = ppi_example$ppi,
    node_annotation = ppi_example$annotation
  )

  nodes_tbl <- get_graph_nodes(g)
  adj_mat   <- get_graph_adjacency(g)

  zipi <- ggnetview_zipi(
    nodes_bulk     = nodes_tbl,
    z_bulk_mat     = adj_mat,
    modularity_col = "Modularity",
    degree_col     = "Degree"
  )

  valid_roles <- c("Peripherals", "Connectors", "Module hubs", "Network hubs")
  non_na_types <- zipi$data$type[!is.na(zipi$data$type)]
  expect_true(all(non_na_types %in% valid_roles))
})

test_that("ggnetview_zipi respects custom thresholds", {
  data(ppi_example, package = "ggNetView")
  g <- build_graph_from_df(
    df = ppi_example$ppi,
    node_annotation = ppi_example$annotation
  )

  nodes_tbl <- get_graph_nodes(g)
  adj_mat   <- get_graph_adjacency(g)

  zipi_default <- ggnetview_zipi(
    nodes_tbl, adj_mat, "Modularity", "Degree"
  )
  zipi_loose <- ggnetview_zipi(
    nodes_tbl, adj_mat, "Modularity", "Degree",
    zi_threshold = 1.0, pi_threshold = 0.3
  )

  n_peripheral_default <- sum(zipi_default$data$type == "Peripherals", na.rm = TRUE)
  n_peripheral_loose   <- sum(zipi_loose$data$type == "Peripherals", na.rm = TRUE)

  expect_lte(n_peripheral_loose, n_peripheral_default)
})
