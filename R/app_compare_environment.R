safe_multi_network_compare <- function(graphs, params = list()) {
  if (!is.list(graphs) || length(graphs) < 2L) {
    return(app_failure("Multi-network comparison requires at least two graph objects."))
  }
  if (!all(vapply(graphs, inherits, logical(1), what = "igraph"))) {
    return(app_failure("All comparison inputs must be graph objects."))
  }

  fn <- resolve_ggnetview_function("ggNetView_multi_link")
  if (is.null(fn)) {
    return(app_failure("Cannot find ggNetView function: ggNetView_multi_link"))
  }

  defaults <- list(
    graph_obj_list = graphs,
    layout = "fr",
    layout.module = "adjacent",
    comparisons = TRUE,
    k_nn = 2
  )
  call_args <- utils::modifyList(defaults, params, keep.null = TRUE)

  result <- safe_call(
    do.call(fn, call_args),
    "Failed to compare multiple networks."
  )
  if (!result$ok) {
    return(result)
  }

  value <- result$value
  plot <- if (is.list(value) && !is.null(value$p)) value$p else value
  info <- if (is.list(value)) value$info else NULL
  link_info <- if (is.list(value)) value$link_info else NULL

  app_success(list(
    plot = plot,
    info = info,
    link_info = link_info,
    raw = value
  ))
}

default_group_info_for_matrix <- function(mat, split = c("halves", "alternating")) {
  split <- match.arg(split)
  mat <- as.data.frame(mat, check.names = FALSE)
  samples <- colnames(mat)
  if (is.null(samples) || length(samples) < 2L) {
    stop("Grouped network workflow requires a matrix with at least two sample columns.", call. = FALSE)
  }

  if (split == "alternating") {
    groups <- rep(c("Group_A", "Group_B"), length.out = length(samples))
  } else {
    midpoint <- ceiling(length(samples) / 2)
    groups <- ifelse(seq_along(samples) <= midpoint, "Group_A", "Group_B")
  }

  data.frame(Sample = samples, Group = groups, stringsAsFactors = FALSE)
}

align_group_info_for_matrix <- function(mat, group_info) {
  mat <- as.data.frame(mat, check.names = FALSE)
  samples <- colnames(mat)
  if (is.null(samples) || length(samples) < 2L) {
    stop("Grouped network workflow requires a matrix with at least two sample columns.", call. = FALSE)
  }

  group_info <- as.data.frame(group_info, check.names = FALSE)
  if (!all(c("Sample", "Group") %in% names(group_info))) {
    stop("Group metadata must contain Sample and Group columns.", call. = FALSE)
  }
  group_info$Sample <- as.character(group_info$Sample)
  group_info$Group <- as.character(group_info$Group)

  matching <- group_info[group_info$Sample %in% samples, , drop = FALSE]
  if (anyDuplicated(matching$Sample)) {
    stop("Group metadata contains duplicate Sample values for selected matrix columns.", call. = FALSE)
  }

  missing_samples <- setdiff(samples, matching$Sample)
  if (length(missing_samples)) {
    stop(
      "Group metadata is missing samples: ",
      paste(missing_samples, collapse = ", "),
      call. = FALSE
    )
  }

  aligned <- matching[match(samples, matching$Sample), , drop = FALSE]
  aligned[, c("Sample", "Group"), drop = FALSE]
}

safe_multi_group_network <- function(mat, group_info = NULL, params = list()) {
  fn <- resolve_ggnetview_function("ggNetView_multi")
  if (is.null(fn)) {
    return(app_failure("Cannot find ggNetView function: ggNetView_multi"))
  }

  mat <- as.data.frame(mat, check.names = FALSE)
  if (is.null(group_info)) {
    group_info <- default_group_info_for_matrix(mat)
  } else {
    group_info <- tryCatch(align_group_info_for_matrix(mat, group_info), error = function(e) e)
    if (inherits(group_info, "error")) {
      return(app_failure(conditionMessage(group_info)))
    }
  }

  defaults <- list(
    mat = mat,
    group_info = group_info,
    method = "cor",
    cor.method = "pearson",
    r.threshold = 0.2,
    p.threshold = 1,
    layout = "circle",
    layout.module = "adjacent"
  )
  call_args <- utils::modifyList(defaults, params, keep.null = TRUE)

  result <- safe_call(
    do.call(fn, call_args),
    "Failed to build grouped multi-network plot."
  )
  if (!result$ok) {
    return(result)
  }

  app_success(list(
    plot = result$value,
    group_info = group_info,
    raw = result$value
  ))
}

safe_environment_link <- function(env, spec, env_select = NULL, spec_select = NULL, params = list()) {
  fn <- resolve_ggnetview_function("gglink_heatmaps_2")
  if (is.null(fn)) {
    return(app_failure("Cannot find ggNetView function: gglink_heatmaps_2"))
  }

  env <- as.data.frame(env, check.names = FALSE)
  spec <- as.data.frame(spec, check.names = FALSE)
  if (is.null(env_select)) {
    env_select <- list(Environment = seq_len(ncol(env)))
  }
  if (is.null(spec_select)) {
    spec_select <- list(Species = seq_len(ncol(spec)))
  }

  defaults <- list(
    env = env,
    spec = spec,
    env_select = env_select,
    spec_select = spec_select,
    relation_method = "correlation",
    cor.method = "pearson",
    orientation = "top_right",
    group_layout = "circle"
  )
  call_args <- utils::modifyList(defaults, params, keep.null = TRUE)

  result <- safe_call(
    do.call(fn, call_args),
    "Failed to calculate environment links."
  )
  if (!result$ok) {
    return(result)
  }

  value <- result$value
  app_success(list(
    plot = value[[1]],
    curved_plot = value[[2]],
    stats = value[[3]],
    raw = value
  ))
}

safe_environment_heatmap <- function(env, spec, env_select = NULL, spec_select = NULL, params = list()) {
  fn <- resolve_ggnetview_function("gglink_heatmaps")
  if (is.null(fn)) {
    return(app_failure("Cannot find ggNetView function: gglink_heatmaps"))
  }

  env <- as.data.frame(env, check.names = FALSE)
  spec <- as.data.frame(spec, check.names = FALSE)
  if (is.null(env_select)) {
    env_select <- list(Environment = seq_len(ncol(env)))
  }
  if (is.null(spec_select)) {
    spec_select <- list(Species = seq_len(ncol(spec)))
  }

  defaults <- list(
    env = env,
    spec = spec,
    env_select = env_select,
    spec_select = spec_select,
    relation_method = "correlation",
    cor.method = "pearson",
    cor.use = "pairwise",
    mantel_kind = "block_vs_col",
    permutations = 99L,
    spec_collapse = FALSE,
    drop_nonsig = FALSE,
    orientation = "top_right",
    group_layout = "circle",
    spec_layout = "circle_outline"
  )
  call_args <- utils::modifyList(defaults, params, keep.null = TRUE)

  result <- safe_call(
    do.call(fn, call_args),
    "Failed to calculate manual environment heatmap."
  )
  if (!result$ok) {
    return(result)
  }

  value <- result$value
  plot <- if (is.list(value) && length(value) >= 1L) value[[1]] else value
  curved_plot <- if (is.list(value) && length(value) >= 2L) value[[2]] else NULL
  stats <- if (is.list(value) && length(value) >= 3L) value[[3]] else data.frame()

  app_success(list(
    plot = plot,
    curved_plot = curved_plot,
    stats = stats,
    raw = value
  ))
}

graph_to_triple_tables <- function(graph) {
  if (!inherits(graph, "igraph")) {
    stop("Triple heatmap requires a graph object for edge/node tables.", call. = FALSE)
  }

  edges <- igraph::as_data_frame(graph, what = "edges")
  if (!all(c("from", "to") %in% names(edges))) {
    stop("Graph edge table must contain from/to columns.", call. = FALSE)
  }
  if (!"weight" %in% names(edges)) {
    edges$weight <- 1
  }
  edges <- edges[, c("from", "to", "weight"), drop = FALSE]

  nodes <- igraph::as_data_frame(graph, what = "vertices")
  if (!"name" %in% names(nodes)) {
    vertex_names <- igraph::V(graph)$name
    if (is.null(vertex_names)) {
      vertex_names <- as.character(seq_len(igraph::vcount(graph)))
    }
    nodes$name <- vertex_names
  }
  annotation_col <- intersect(c("Modularity", "modularity2", "modularity", "module", "annotation"), names(nodes))
  annotation <- if (length(annotation_col)) {
    as.character(nodes[[annotation_col[[1]]]])
  } else {
    rep("Feature", nrow(nodes))
  }
  node_table <- data.frame(
    node = as.character(nodes$name),
    annotation = annotation,
    stringsAsFactors = FALSE
  )

  list(edges = edges, nodes = node_table)
}

sample_table_for_triple <- function(x, sample_col = "Sample") {
  x <- as.data.frame(x, check.names = FALSE)
  if (sample_col %in% names(x)) {
    return(x)
  }
  samples <- rownames(x)
  if (is.null(samples) || any(!nzchar(samples))) {
    samples <- paste0("S", seq_len(nrow(x)))
  }
  data.frame(Sample = samples, x, check.names = FALSE)
}

safe_environment_triple_heatmap <- function(env, experiment, graph, params = list()) {
  fn <- resolve_ggnetview_function("gglink_heatmap_triple")
  if (is.null(fn)) {
    return(app_failure("Cannot find ggNetView function: gglink_heatmap_triple"))
  }

  tables <- tryCatch(graph_to_triple_tables(graph), error = function(e) e)
  if (inherits(tables, "error")) {
    return(app_failure(conditionMessage(tables)))
  }

  env <- as.data.frame(env, check.names = FALSE)
  experiment <- as.data.frame(experiment, check.names = FALSE)
  common_samples <- intersect(rownames(env), rownames(experiment))
  if (length(common_samples) >= 3L) {
    env <- env[common_samples, , drop = FALSE]
    experiment <- experiment[common_samples, , drop = FALSE]
  }

  graph_nodes <- tables$nodes$node
  feature_count_param <- params$feature_count
  if (is.null(feature_count_param)) {
    feature_count_param <- min(3L, ncol(experiment))
  }
  feature_count <- as.integer(feature_count_param)
  feature_count <- max(1L, min(feature_count, ncol(experiment), max(1L, length(graph_nodes) - 1L)))
  preferred_features <- intersect(graph_nodes, colnames(experiment))
  if (length(preferred_features) >= feature_count) {
    feature_names <- preferred_features[seq_len(feature_count)]
  } else {
    feature_names <- unique(c(preferred_features, colnames(experiment)))[seq_len(feature_count)]
  }
  experiment <- experiment[, feature_names, drop = FALSE]

  defaults <- list(
    Environment = sample_table_for_triple(env),
    Experiment = sample_table_for_triple(experiment),
    edge = tables$edges,
    node = tables$nodes,
    sample_col = "Sample",
    hub_n = ncol(experiment),
    r = 6
  )
  call_args <- utils::modifyList(defaults, params[names(params) != "feature_count"], keep.null = TRUE)

  result <- safe_call(
    do.call(fn, call_args),
    "Failed to calculate triple environment heatmap."
  )
  if (!result$ok) {
    return(result)
  }

  app_success(list(
    plot = result$value,
    nodes = tables$nodes,
    edges = tables$edges,
    experiment = experiment,
    raw = result$value
  ))
}

safe_mantel_pairwise <- function(spec, env, params = list()) {
  fn <- resolve_ggnetview_function("mantel_pairwise")
  if (is.null(fn)) {
    return(app_failure("Cannot find ggNetView function: mantel_pairwise"))
  }

  defaults <- list(
    spec_df = as.data.frame(spec, check.names = FALSE),
    env_df = as.data.frame(env, check.names = FALSE),
    method = "pearson",
    permutations = 99L
  )
  call_args <- utils::modifyList(defaults, params, keep.null = TRUE)

  safe_call(
    do.call(fn, call_args),
    "Failed to run Mantel pairwise test."
  )
}
