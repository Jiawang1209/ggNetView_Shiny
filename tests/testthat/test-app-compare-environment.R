source(testthat::test_path("../../R/app_validation.R"))
source(testthat::test_path("../../R/app_adapters.R"))
source(testthat::test_path("../../R/app_graph_builders.R"))
source(testthat::test_path("../../R/app_compare_environment.R"))

read_phase2_fixture <- function(name, row_names = TRUE) {
  path <- testthat::test_path("../../inst/extdata", name)
  if (row_names) {
    utils::read.csv(path, row.names = 1, check.names = FALSE)
  } else {
    utils::read.csv(path, check.names = FALSE)
  }
}

phase2_graph_pair <- function() {
  mat_a <- read_phase2_fixture("phase2_example_matrix.csv")
  mat_b <- read_phase2_fixture("phase2_example_matrix_b.csv")
  graph_a <- safe_graph_builder(
    "matrix",
    inputs = list(matrix = mat_a),
    params = list(method = "cor", r.threshold = 0.2, p.threshold = 1)
  )
  graph_b <- safe_graph_builder(
    "matrix",
    inputs = list(matrix = mat_b),
    params = list(method = "cor", r.threshold = 0.2, p.threshold = 1)
  )
  testthat::expect_true(isTRUE(graph_a$ok), info = graph_a$trace %||% graph_a$message)
  testthat::expect_true(isTRUE(graph_b$ok), info = graph_b$trace %||% graph_b$message)
  list(A = graph_a$value, B = graph_b$value)
}

phase2_env_spec <- function() {
  spec <- t(as.matrix(read_phase2_fixture("phase2_example_matrix.csv")))
  env <- data.frame(
    temperature = c(12, 13, 14, 16, 18),
    pH = c(6.8, 6.9, 7.1, 7.2, 7.4),
    moisture = c(30, 31, 35, 36, 40),
    row.names = rownames(spec),
    check.names = FALSE
  )
  list(spec = as.data.frame(spec, check.names = FALSE), env = env)
}

test_that("multi-network comparison returns a plot payload", {
  result <- safe_multi_network_compare(phase2_graph_pair())

  expect_true(isTRUE(result$ok), info = result$trace %||% result$message)
  expect_s3_class(result$value$plot, "ggplot")
  expect_true(is.null(result$value$link_info) || is.data.frame(result$value$link_info) || is.list(result$value$link_info))
})

test_that("environment link returns plot and statistics", {
  data <- phase2_env_spec()
  result <- safe_environment_link(
    env = data$env,
    spec = data$spec,
    env_select = list(Environment = seq_len(ncol(data$env))),
    spec_select = list(Species = seq_len(ncol(data$spec)))
  )

  expect_true(isTRUE(result$ok), info = result$trace %||% result$message)
  expect_s3_class(result$value$plot, "ggplot")
  expect_s3_class(result$value$curved_plot, "ggplot")
  expect_true(all(c("ID", "Type", "Correlation", "Pvalue") %in% names(result$value$stats)))
})

test_that("Mantel pairwise returns a statistics table or dependency error", {
  data <- phase2_env_spec()
  result <- safe_mantel_pairwise(data$spec[, 1:2], data$env[, 1:2], params = list(permutations = 9L))

  if (requireNamespace("vegan", quietly = TRUE)) {
    expect_true(isTRUE(result$ok), info = result$trace %||% result$message)
    expect_true(all(c("ID", "Type", "Correlation", "Pvalue") %in% names(result$value)))
  } else {
    expect_false(result$ok)
    expect_match(result$trace, "vegan")
  }
})
