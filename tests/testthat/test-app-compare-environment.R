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

test_that("multi-network comparison exposes link and topology tables", {
  result <- safe_multi_network_compare(
    phase2_graph_pair(),
    params = list(include_topology_summary = TRUE)
  )

  expect_true(isTRUE(result$ok), info = result$trace %||% result$message)
  expect_true(is.data.frame(result$value$link_table))
  expect_true(is.data.frame(result$value$topology_table))
  expect_true(all(c("graph", "Topology", "Value") %in% names(result$value$topology_table)))
  expect_true(all(c("A", "B") %in% unique(result$value$topology_table$graph)))
  expect_true(nrow(result$value$topology_table) > 0)
})

test_that("grouped matrix workflow returns a multi-network plot", {
  mat <- read_phase2_fixture("phase2_example_matrix.csv")
  group_info <- default_group_info_for_matrix(mat)
  result <- safe_multi_group_network(mat, group_info = group_info)

  expect_true(isTRUE(result$ok), info = result$trace %||% result$message)
  expect_true(inherits(result$value$plot, "ggplot") || inherits(result$value$plot, "patchwork"))
  expect_true(all(c("Sample", "Group") %in% names(result$value$group_info)))
  expect_equal(nrow(result$value$group_info), ncol(mat))
})

test_that("grouped matrix workflow aligns custom sample metadata", {
  mat <- read_phase2_fixture("phase2_example_matrix.csv")
  group_info <- data.frame(
    Sample = c("S4", "S2", "S5", "S1", "S3", "S_unused"),
    Group = c("Late", "Early", "Late", "Early", "Late", "Ignore"),
    stringsAsFactors = FALSE
  )
  result <- safe_multi_group_network(mat, group_info = group_info)

  expect_true(isTRUE(result$ok), info = result$trace %||% result$message)
  expect_equal(result$value$group_info$Sample, colnames(mat))
  expect_equal(result$value$group_info$Group, c("Early", "Early", "Late", "Late", "Late"))
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

test_that("manual environment heatmap returns plot and statistics", {
  data <- phase2_env_spec()
  result <- safe_environment_heatmap(
    env = data$env,
    spec = data$spec,
    env_select = list(Environment = seq_len(ncol(data$env))),
    spec_select = list(Species = seq_len(ncol(data$spec)))
  )

  expect_true(isTRUE(result$ok), info = result$trace %||% result$message)
  expect_s3_class(result$value$plot, "ggplot")
  expect_s3_class(result$value$curved_plot, "ggplot")
  expect_true(is.data.frame(result$value$stats))
  expect_true(all(c("ID", "Type", "Correlation", "Pvalue") %in% names(result$value$stats)))
})

test_that("manual environment heatmap supports block Mantel links or dependency error", {
  data <- phase2_env_spec()
  result <- safe_environment_heatmap(
    env = data$env,
    spec = data$spec[, 1:3],
    env_select = list(Environment = seq_len(ncol(data$env))),
    spec_select = list(Species = 1:3),
    params = list(
      relation_method = "mantel",
      mantel_kind = "block_vs_col",
      permutations = 9L,
      spec_collapse = TRUE
    )
  )

  if (requireNamespace("vegan", quietly = TRUE)) {
    expect_true(isTRUE(result$ok), info = result$trace %||% result$message)
    expect_s3_class(result$value$plot, "ggplot")
    expect_true(is.data.frame(result$value$stats))
  } else {
    expect_false(result$ok)
    expect_match(result$trace, "vegan")
  }
})

test_that("triple environment heatmap returns a plot from graph-backed tables", {
  data <- phase2_env_spec()
  graph <- phase2_graph_pair()[[1]]
  result <- safe_environment_triple_heatmap(
    env = data$env,
    experiment = data$spec,
    graph = graph,
    params = list(feature_count = 3L, r = 6)
  )

  expect_true(isTRUE(result$ok), info = result$trace %||% result$message)
  expect_s3_class(result$value$plot, "ggplot")
  expect_true(is.data.frame(result$value$nodes))
  expect_true(is.data.frame(result$value$edges))
  expect_true(ncol(result$value$experiment) <= ncol(data$spec))
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
