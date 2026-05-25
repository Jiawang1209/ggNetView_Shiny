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
  expect_true(is.data.frame(result$value$link_summary))
  expect_true(is.data.frame(result$value$topology_table))
  expect_true(all(c("graph", "Topology", "Value") %in% names(result$value$topology_table)))
  expect_true(all(c("A", "B") %in% unique(result$value$topology_table$graph)))
  expect_true(nrow(result$value$topology_table) > 0)
})

test_that("multi-network comparison parses and applies selected comparison pairs", {
  pairs <- parse_comparison_pairs("A,B\nB,A\nA,Missing", c("A", "B", "C"))

  expect_equal(length(pairs$pairs), 1L)
  expect_equal(pairs$pairs[[1]], c("A", "B"))
  expect_true(any(grepl("duplicate", pairs$warnings)))
  expect_true(any(grepl("not available", pairs$warnings)))

  result <- safe_multi_network_compare(
    phase2_graph_pair(),
    params = list(comparison_pairs = "A,B")
  )

  expect_true(isTRUE(result$ok), info = result$trace %||% result$message)
  expect_equal(result$value$comparison_pairs[[1]], c("A", "B"))
})

test_that("multi-network link interpretation summarizes pair-level links", {
  link_info <- data.frame(
    link_level = c("module", "module", "node"),
    group_a = c("A", "A", "A"),
    group_b = c("B", "B", "B"),
    source = c("1", "2", "OTU1"),
    target = c("1", "3", "OTU1"),
    x = c(0, 0, 1),
    y = c(0, 2, 1),
    xend = c(3, 4, 1),
    yend = c(4, 2, 3),
    stringsAsFactors = FALSE
  )

  interpreted <- interpret_multi_network_links(link_info)

  expect_true(is.data.frame(interpreted$details))
  expect_true(is.data.frame(interpreted$summary))
  expect_true(all(c("pair", "link_label", "distance") %in% names(interpreted$details)))
  expect_equal(interpreted$details$pair, rep("A vs B", 3))
  expect_equal(interpreted$summary$link_count[interpreted$summary$link_level == "module"], 2L)
  expect_equal(interpreted$summary$link_count[interpreted$summary$link_level == "node"], 1L)
  expect_equal(interpreted$summary$unique_sources[interpreted$summary$link_level == "module"], 2L)
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

test_that("environment block selectors parse and pass through to environment link", {
  data <- phase2_env_spec()
  env_blocks <- parse_table_blocks("Climate: temperature,pH\nWater: moisture", names(data$env), "Env")
  spec_blocks <- parse_table_blocks("Early: OTU1,OTU2,OTU3\nLate: OTU4,OTU5,OTU6", names(data$spec), "Spec")

  expect_equal(names(env_blocks$blocks), c("Climate", "Water"))
  expect_equal(env_blocks$blocks$Climate, c("temperature", "pH"))
  expect_equal(names(spec_blocks$blocks), c("Early", "Late"))
  expect_length(env_blocks$warnings, 0L)
  expect_length(spec_blocks$warnings, 0L)

  result <- safe_environment_link(
    env = data$env,
    spec = data$spec,
    env_blocks = "Climate: temperature,pH\nWater: moisture",
    spec_blocks = "Early: OTU1,OTU2,OTU3\nLate: OTU4,OTU5,OTU6"
  )

  expect_true(isTRUE(result$ok), info = result$trace %||% result$message)
  expect_true(all(c("Climate", "Water") %in% unique(result$value$stats$env_block)))
  expect_true(all(c("Early", "Late") %in% unique(result$value$stats$spec_block)))
  expect_equal(names(result$value$env_select), c("Climate", "Water"))
  expect_equal(names(result$value$spec_select), c("Early", "Late"))
})

test_that("environment block pairs restrict computed environment links", {
  data <- phase2_env_spec()
  env_blocks <- "Climate: temperature,pH\nWater: moisture"
  spec_blocks <- "Early: OTU1,OTU2,OTU3\nLate: OTU4,OTU5,OTU6"

  parsed <- parse_environment_block_pairs(
    "Climate,Early\nWater,Late",
    env_names = c("Climate", "Water"),
    spec_names = c("Early", "Late")
  )
  expect_equal(parsed$pairs, list(c("Climate", "Early"), c("Water", "Late")))
  expect_length(parsed$warnings, 0L)

  result <- safe_environment_link(
    env = data$env,
    spec = data$spec,
    env_blocks = env_blocks,
    spec_blocks = spec_blocks,
    env_spec_pairs = "Climate,Early\nWater,Late"
  )

  expect_true(isTRUE(result$ok), info = result$trace %||% result$message)
  actual_pairs <- unique(paste(result$value$stats$env_block, result$value$stats$spec_block, sep = ","))
  expect_setequal(actual_pairs, c("Climate,Early", "Water,Late"))
  expect_equal(result$value$comparison_pairs, list(c("Climate", "Early"), c("Water", "Late")))
})

test_that("environment multi-core geometry params parse and pass through", {
  data <- phase2_env_spec()
  data$env$conductivity <- c(100, 105, 112, 119, 126)
  params <- environment_geometry_params(
    orientation_text = "top_right,bottom_right",
    spec_layout_text = "circle_outline,square_outline",
    group_layout = "row",
    anchor_dist = 4,
    distance = 2,
    nrow = 1,
    scale_networks = FALSE,
    core_point_size = 6
  )

  expect_equal(params$orientation, c("top_right", "bottom_right"))
  expect_equal(params$spec_layout, c("circle_outline", "square_outline"))
  expect_equal(params$group_layout, "row")
  expect_equal(params$anchor_dist, 4)
  expect_equal(params$distance, 2)
  expect_equal(params$nrow, 1L)
  expect_false(params$scale_networks)
  expect_equal(params$CorePointSize, 6)

  result <- safe_environment_heatmap(
    env = data$env,
    spec = data$spec,
    env_blocks = "Climate: temperature,pH\nWater: moisture,conductivity",
    spec_blocks = "Early: OTU1,OTU2,OTU3\nLate: OTU4,OTU5,OTU6",
    env_spec_pairs = "Climate,Early\nWater,Late",
    params = params
  )

  expect_true(isTRUE(result$ok), info = result$trace %||% result$message)
  expect_equal(result$value$call_params$orientation, c("top_right", "bottom_right"))
  expect_equal(result$value$call_params$spec_layout, c("circle_outline", "square_outline"))
  expect_equal(result$value$call_params$group_layout, "row")
  expect_equal(result$value$call_params$nrow, 1L)
  expect_false(result$value$call_params$scale_networks)
  expect_equal(result$value$call_params$CorePointSize, 6)
})

test_that("environment style geometry params parse and pass through", {
  data <- phase2_env_spec()
  data$env$conductivity <- c(100, 105, 112, 119, 126)
  params <- environment_geometry_params(
    orientation_text = "top_right,bottom_right",
    spec_layout_text = "circle_outline,square_outline",
    group_layout = "row",
    anchor_dist = 4,
    distance = 2,
    nrow = 1,
    ncol = 2,
    heatmap_label_size = 4,
    heatmap_sig_size = 3,
    heatmap_point_size = 4.5,
    core_point_size = 6,
    sig_line_width_min = 0.25,
    sig_line_width_max = 1.75,
    sig_line_color_low = "#2166ac",
    sig_line_color_high = "#b2182b",
    sig_line_alpha = 0.8
  )

  expect_equal(params$ncol, 2L)
  expect_equal(params$HeatmapLabelSize, 4)
  expect_equal(params$HeatmapSigSize, 3)
  expect_equal(params$HeatmapPointSize, 4.5)
  expect_equal(params$CorePointSize, 6)
  expect_equal(params$SigLineWidth, c(0.25, 1.75))
  expect_equal(params$SigLineColor, c("#2166ac", "#b2182b"))
  expect_equal(params$SigLineAlpha, 0.8)

  result <- safe_environment_link(
    env = data$env,
    spec = data$spec,
    env_blocks = "Climate: temperature,pH\nWater: moisture,conductivity",
    spec_blocks = "Early: OTU1,OTU2,OTU3\nLate: OTU4,OTU5,OTU6",
    env_spec_pairs = "Climate,Early\nWater,Late",
    params = params
  )

  expect_true(isTRUE(result$ok), info = result$trace %||% result$message)
  expect_equal(result$value$call_params$ncol, 2L)
  expect_equal(result$value$call_params$HeatmapLabelSize, 4)
  expect_equal(result$value$call_params$HeatmapSigSize, 3)
  expect_equal(result$value$call_params$HeatmapPointSize, 4.5)
  expect_equal(result$value$call_params$CorePointSize, 6)
  expect_equal(result$value$call_params$SigLineWidth, c(0.25, 1.75))
  expect_equal(result$value$call_params$SigLineColor, c("#2166ac", "#b2182b"))
  expect_equal(result$value$call_params$SigLineAlpha, 0.8)
})

test_that("environment geometry supports arc rotation and inward heatmap distance", {
  data <- phase2_env_spec()
  data$env$conductivity <- c(100, 105, 112, 119, 126)
  params <- environment_geometry_params(
    orientation_text = "top_right,bottom_right",
    spec_layout_text = "circle_outline,square_outline",
    group_layout = "arc",
    group_angle = 45,
    group_arc_angle = 120,
    anchor_dist = 4,
    distance = -1,
    nrow = 1,
    scale_networks = TRUE,
    core_point_size = 10
  )

  expect_equal(params$group_layout, "arc")
  expect_equal(params$group_angle, 45)
  expect_equal(params$group_arc_angle, 120)
  expect_equal(params$distance, -1)

  result <- safe_environment_heatmap(
    env = data$env,
    spec = data$spec,
    env_blocks = "Climate: temperature,pH\nWater: moisture,conductivity",
    spec_blocks = "Early: OTU1,OTU2,OTU3\nLate: OTU4,OTU5,OTU6",
    env_spec_pairs = "Climate,Early\nWater,Late",
    params = c(
      list(
        relation_method = "correlation",
        spec_collapse = TRUE
      ),
      params
    )
  )

  expect_true(isTRUE(result$ok), info = result$trace %||% result$message)
  expect_s3_class(result$value$plot, "ggplot")
  expect_equal(result$value$call_params$group_layout, "arc")
  expect_equal(result$value$call_params$group_angle, 45)
  expect_equal(result$value$call_params$group_arc_angle, 120)
  expect_equal(result$value$call_params$distance, -1)
})

test_that("environment link ignores heatmap-only arc geometry controls", {
  data <- phase2_env_spec()
  data$env$conductivity <- c(100, 105, 112, 119, 126)
  params <- environment_geometry_params(
    orientation_text = "top_right,bottom_right",
    spec_layout_text = "circle_outline,square_outline",
    group_layout = "arc",
    group_angle = 45,
    group_arc_angle = 120,
    anchor_dist = 4,
    distance = 1,
    nrow = 1,
    scale_networks = TRUE,
    core_point_size = 10
  )

  result <- safe_environment_link(
    env = data$env,
    spec = data$spec,
    env_blocks = "Climate: temperature,pH\nWater: moisture,conductivity",
    spec_blocks = "Early: OTU1,OTU2,OTU3\nLate: OTU4,OTU5,OTU6",
    env_spec_pairs = "Climate,Early\nWater,Late",
    params = c(
      list(
        relation_method = "correlation",
        cor.method = "pearson"
      ),
      params
    )
  )

  expect_true(isTRUE(result$ok), info = result$trace %||% result$message)
  expect_equal(result$value$call_params$group_layout, "circle")
  expect_null(result$value$call_params$group_angle)
  expect_null(result$value$call_params$group_arc_angle)
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

test_that("module environment heatmap returns plot and module-level statistics", {
  data <- phase2_env_spec()
  graph <- phase2_graph_pair()[[1]]
  otu_mat <- read_phase2_fixture("phase2_example_matrix.csv")

  result <- safe_module_environment_heatmap(
    graph = graph,
    env = data$env,
    otu_mat = otu_mat,
    env_blocks = "Climate: temperature,pH\nWater: moisture",
    params = list(
      relation_method = "correlation",
      cor.method = "pearson",
      orientation = c("top_right", "bottom_right"),
      layout = "circle",
      layout.module = "adjacent",
      distance = 2
    )
  )

  expect_true(isTRUE(result$ok), info = result$trace %||% result$message)
  expect_s3_class(result$value$plot, "ggplot")
  expect_s3_class(result$value$curved_plot, "ggplot")
  expect_true(is.data.frame(result$value$stats))
  expect_true(all(c("ID", "Type", "Correlation", "Pvalue", "env_block") %in% names(result$value$stats)))
  expect_equal(names(result$value$env_select), c("Climate", "Water"))
  expect_equal(result$value$call_params$orientation, c("top_right", "bottom_right"))
})

test_that("module environment heatmap explains env block and orientation mismatches", {
  data <- phase2_env_spec()
  graph <- phase2_graph_pair()[[1]]
  otu_mat <- read_phase2_fixture("phase2_example_matrix.csv")

  result <- safe_module_environment_heatmap(
    graph = graph,
    env = data$env,
    otu_mat = otu_mat,
    env_blocks = "Climate: temperature,pH\nWater: moisture",
    params = list(orientation = "top_right")
  )

  expect_false(isTRUE(result$ok))
  expect_match(result$message, "same number of environment blocks and orientations")
})

test_that("manual environment heatmap supports block Mantel links or dependency error", {
  data <- phase2_env_spec()
  mantel_params <- environment_mantel_params(
    method = "spearman",
    alternative = "greater",
    spec_dist_method = "bray",
    env_dist_method = "euclidean",
    permutations = 9L
  )

  expect_equal(mantel_params$method, "spearman")
  expect_equal(mantel_params$mantel.method2, "spearman")
  expect_equal(mantel_params$mantel.alternative, "greater")
  expect_equal(mantel_params$spec_dist_method, "bray")
  expect_equal(mantel_params$env_dist_method, "euclidean")
  expect_equal(mantel_params$permutations, 9L)

  result <- safe_environment_heatmap(
    env = data$env,
    spec = data$spec[, 1:3],
    env_select = list(Environment = seq_len(ncol(data$env))),
    spec_select = list(Species = 1:3),
    params = c(list(
      relation_method = "mantel",
      mantel_kind = "block_vs_col",
      spec_collapse = TRUE
    ), mantel_params)
  )

  if (requireNamespace("vegan", quietly = TRUE)) {
    expect_true(isTRUE(result$ok), info = result$trace %||% result$message)
    expect_s3_class(result$value$plot, "ggplot")
    expect_true(is.data.frame(result$value$stats))
    expect_equal(result$value$call_params$mantel.method2, "spearman")
    expect_equal(result$value$call_params$mantel.alternative, "greater")
    expect_equal(result$value$call_params$spec_dist_method, "bray")
    expect_equal(result$value$call_params$env_dist_method, "euclidean")
    expect_equal(result$value$call_params$permutations, 9L)
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

test_that("direct Mantel table routes block and pairwise helpers", {
  data <- phase2_env_spec()
  block <- safe_mantel_table(
    data$spec[, 1:3],
    data$env[, 1:2],
    params = list(
      mantel_kind = "block_vs_col",
      method = "spearman",
      spec_dist_method = "bray",
      env_dist_method = "euclidean",
      permutations = 9L
    )
  )
  pairwise <- safe_mantel_table(
    data$spec[, 1:2],
    data$env[, 1:2],
    params = list(
      mantel_kind = "col_vs_col",
      method = "kendall",
      alternative = "less",
      permutations = 9L
    )
  )

  if (requireNamespace("vegan", quietly = TRUE)) {
    expect_true(isTRUE(block$ok), info = block$trace %||% block$message)
    expect_true(isTRUE(pairwise$ok), info = pairwise$trace %||% pairwise$message)
    expect_true(all(c("ID", "Type", "Correlation", "Pvalue") %in% names(block$value)))
    expect_true(all(c("ID", "Type", "Correlation", "Pvalue") %in% names(pairwise$value)))
    expect_true(all(block$value$ID == "Species"))
    expect_equal(nrow(pairwise$value), 4L)
  } else {
    expect_false(block$ok)
    expect_false(pairwise$ok)
    expect_match(block$trace, "vegan")
    expect_match(pairwise$trace, "vegan")
  }
})
