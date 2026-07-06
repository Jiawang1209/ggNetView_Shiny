source(testthat::test_path("../../R/app_validation.R"))
source(testthat::test_path("../../R/app_adapters.R"))
source(testthat::test_path("../../R/app_compare_environment.R"))

test_that("safe_link_heatmap_adaptive resolves gglink_heatmaps_2 and returns a result object", {
  set.seed(1)
  spec <- as.data.frame(matrix(runif(6 * 8), nrow = 6,
                               dimnames = list(paste0("OTU", 1:6), paste0("S", 1:8))))
  env <- as.data.frame(matrix(runif(2 * 8), nrow = 2,
                              dimnames = list(c("pH", "temp"), paste0("S", 1:8))))

  result <- safe_link_heatmap_adaptive(env = env, spec = spec, params = list())

  expect_s3_class(result, "ggnetview_app_result")
  if (result$ok) {
    expect_true(is.list(result$value))
  } else {
    expect_true(is.character(result$message) && nzchar(result$message))
  }
})

test_that("safe_magnified_subgraph fails gracefully on non-igraph input", {
  result <- safe_magnified_subgraph(list(), select_module = "1")
  expect_false(result$ok)
  expect_match(result$message, "igraph", ignore.case = TRUE)
})

test_that("safe_magnified_subgraph fails gracefully when select_module is not a module level", {
  library(igraph)
  nodes <- data.frame(name = c("A","B","C","D"),
                      Modularity = factor(c("1","1","2","2")),
                      stringsAsFactors = FALSE)
  edges <- data.frame(from = c("A","B","C"), to = c("B","C","D"), stringsAsFactors = FALSE)
  g <- igraph::graph_from_data_frame(edges, directed = FALSE, vertices = nodes)

  result <- safe_magnified_subgraph(g, select_module = "999")
  expect_false(result$ok)
  expect_match(result$message, "module", ignore.case = TRUE)
})
