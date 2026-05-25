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
