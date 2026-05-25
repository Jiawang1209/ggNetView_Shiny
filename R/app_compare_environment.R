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

safe_multi_group_network <- function(mat, group_info = NULL, params = list()) {
  fn <- resolve_ggnetview_function("ggNetView_multi")
  if (is.null(fn)) {
    return(app_failure("Cannot find ggNetView function: ggNetView_multi"))
  }

  mat <- as.data.frame(mat, check.names = FALSE)
  if (is.null(group_info)) {
    group_info <- default_group_info_for_matrix(mat)
  } else {
    group_info <- as.data.frame(group_info, check.names = FALSE)
  }
  if (!all(c("Sample", "Group") %in% names(group_info))) {
    return(app_failure("Group metadata must contain Sample and Group columns."))
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
