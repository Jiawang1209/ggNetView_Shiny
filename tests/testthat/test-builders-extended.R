test_that("build_graph_from_df creates a graph from edge data frame", {
  data(ppi_example, package = "ggNetView")

  g <- build_graph_from_df(
    df = ppi_example$ppi,
    node_annotation = ppi_example$annotation
  )

  expect_s3_class(g, "tbl_graph")
  expect_gt(igraph::gorder(g), 0)
  expect_gt(igraph::gsize(g), 0)

  node_df <- g %>%
    tidygraph::activate(nodes) %>%
    tidygraph::as_tibble()

  expect_true("Modularity" %in% colnames(node_df))
  expect_true("Degree" %in% colnames(node_df))
})

test_that("build_graph_from_adj_mat creates a graph from adjacency matrix", {
  mat <- matrix(
    c(0, 0.8, 0.3, 0,
      0.8, 0, 0.5, 0.2,
      0.3, 0.5, 0, 0.9,
      0, 0.2, 0.9, 0),
    nrow = 4
  )
  rownames(mat) <- colnames(mat) <- paste0("N", 1:4)

  g <- build_graph_from_adj_mat(mat, seed = 1)

  expect_s3_class(g, "tbl_graph")
  expect_equal(igraph::gorder(g), 4)
  expect_gt(igraph::gsize(g), 0)
})

test_that("build_graph_from_double_mat creates a cross-block network", {
  set.seed(1)
  mat1 <- matrix(rnorm(40), nrow = 4)
  mat2 <- matrix(rnorm(40), nrow = 4)
  rownames(mat1) <- paste0("A", 1:4)
  rownames(mat2) <- paste0("B", 1:4)
  colnames(mat1) <- colnames(mat2) <- paste0("S", 1:10)

  g <- build_graph_from_double_mat(
    mat1 = mat1, mat2 = mat2,
    module.method = "Fast_greedy",
    seed = 1
  )

  expect_s3_class(g, "tbl_graph")

  node_df <- g %>%
    tidygraph::activate(nodes) %>%
    tidygraph::as_tibble()

  expect_true("Modularity" %in% colnames(node_df))
})

test_that("build_graph_from_multi_mat handles three matrices", {
  set.seed(1)
  n <- 20
  mat_a <- matrix(rnorm(5 * n), nrow = 5)
  mat_b <- matrix(rnorm(5 * n), nrow = 5)
  mat_c <- matrix(rnorm(5 * n), nrow = 5)
  rownames(mat_a) <- paste0("A", 1:5)
  rownames(mat_b) <- paste0("B", 1:5)
  rownames(mat_c) <- paste0("C", 1:5)
  colnames(mat_a) <- colnames(mat_b) <- colnames(mat_c) <- paste0("S", 1:n)

  g <- build_graph_from_multi_mat(
    mat_a, mat_b, mat_c,
    module.method = "Fast_greedy",
    seed = 1
  )

  expect_s3_class(g, "tbl_graph")
  expect_gt(igraph::gorder(g), 0)
})

test_that("build_graph_from_module builds graph with user-supplied modules", {
  data(ppi_module, package = "ggNetView")

  g <- build_graph_from_module(
    df              = ppi_module$ppi,
    node_annotation = ppi_module$annotation,
    seed            = 1
  )

  expect_s3_class(g, "tbl_graph")
  node_df <- g %>%
    tidygraph::activate(nodes) %>%
    tidygraph::as_tibble()

  expect_true("Modularity" %in% colnames(node_df))
})

test_that("build_graph_from_mat with node_annotation attaches metadata", {
  mat <- rbind(
    A = c(1, 2, 3, 4, 5, 6, 7, 8),
    B = c(2, 3, 4, 5, 6, 7, 8, 9),
    C = c(8, 7, 6, 5, 4, 3, 2, 1),
    D = c(3, 4, 5, 6, 7, 8, 9, 10)
  )
  colnames(mat) <- paste0("S", 1:8)

  annot <- data.frame(
    name  = c("A", "B", "C", "D"),
    Group = c("G1", "G1", "G2", "G2")
  )

  g <- build_graph_from_mat(
    mat = mat,
    method = "cor",
    cor.method = "pearson",
    r.threshold = 0.5,
    p.threshold = 1,
    node_annotation = annot,
    seed = 1
  )

  node_df <- g %>%
    tidygraph::activate(nodes) %>%
    tidygraph::as_tibble()

  expect_true("Group" %in% colnames(node_df))
})

test_that("update_graph_modules renames module labels", {
  data(ppi_example, package = "ggNetView")
  g <- build_graph_from_df(
    df = ppi_example$ppi,
    node_annotation = ppi_example$annotation
  )

  nodes <- get_graph_nodes(g)
  old_mods <- unique(as.character(nodes$Modularity))
  rename_vec <- setNames(
    paste0("Renamed_", old_mods),
    old_mods
  )

  g2 <- update_graph_modules(g, modules = rename_vec)

  expect_s3_class(g2, "tbl_graph")
  expect_equal(igraph::gorder(g2), igraph::gorder(g))

  new_nodes <- get_graph_nodes(g2)
  expect_true(all(grepl("^Renamed_", as.character(new_nodes$Modularity))))
})
