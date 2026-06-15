# Adapters for the network perturbation / robustness workflow.
# Wrap the package functions get_network_perturbation(), get_node_influence(),
# and press_perturbation() with the shared app_success/app_failure contract.

# Ensure a graph object is a tidygraph tbl_graph, as required by the
# perturbation functions. Registry graphs from the builders are already
# tbl_graph, but coerce defensively for plain igraph inputs.
coerce_tbl_graph <- function(graph) {
  if (inherits(graph, "tbl_graph")) {
    return(graph)
  }
  if (inherits(graph, "igraph") && requireNamespace("tidygraph", quietly = TRUE)) {
    return(tidygraph::as_tbl_graph(graph))
  }
  graph
}

# Node names for populating source-node selectors.
perturbation_node_names <- function(graph) {
  if (inherits(graph, "igraph")) {
    nm <- igraph::V(graph)$name
    if (is.null(nm)) {
      nm <- as.character(seq_len(igraph::vcount(graph)))
    }
    return(nm)
  }
  fn <- resolve_ggnetview_function("get_graph_nodes")
  if (!is.null(fn)) {
    nodes <- tryCatch(as.data.frame(fn(graph), check.names = FALSE), error = function(e) NULL)
    if (!is.null(nodes) && "name" %in% names(nodes)) {
      return(as.character(nodes$name))
    }
  }
  character()
}

# Module labels available on a graph, for the "module knockout" strategy.
perturbation_module_values <- function(graph, module_col = "Modularity") {
  fn <- resolve_ggnetview_function("get_graph_nodes")
  if (is.null(fn)) {
    return(character())
  }
  nodes <- tryCatch(as.data.frame(fn(graph), check.names = FALSE), error = function(e) NULL)
  if (is.null(nodes) || !module_col %in% names(nodes)) {
    return(character())
  }
  vals <- unique(as.character(nodes[[module_col]]))
  vals[!is.na(vals) & nzchar(vals)]
}

normalize_fraction_step <- function(value, default = 0.05) {
  value <- suppressWarnings(as.numeric(value))
  if (length(value) != 1L || is.na(value) || !is.finite(value) || value <= 0 || value >= 1) {
    value <- default
  }
  value
}

safe_network_perturbation <- function(graph, params = list()) {
  if (!inherits(graph, "igraph") && !inherits(graph, "tbl_graph")) {
    return(app_failure("Network perturbation requires a graph object."))
  }
  fn <- resolve_ggnetview_function("get_network_perturbation")
  if (is.null(fn)) {
    return(app_failure("Cannot find ggNetView function: get_network_perturbation"))
  }

  graph <- coerce_tbl_graph(graph)

  strategy <- params$strategy %||% "random"
  centrality <- params$centrality %||% "degree"
  target <- params$target
  if (is.character(target) && length(target) == 0L) {
    target <- NULL
  }
  step <- normalize_fraction_step(params$fraction_step, 0.05)
  bootstrap <- suppressWarnings(as.integer(params$bootstrap %||% 100))
  if (is.na(bootstrap) || bootstrap < 1L) {
    bootstrap <- 100L
  }
  seed <- suppressWarnings(as.integer(params$seed %||% 123))
  if (is.na(seed)) {
    seed <- 123L
  }

  # Ensure fraction grid always terminates at exactly 1.0 regardless of
  # whether `step` divides 1 evenly (e.g. step=0.3 → 0.3,0.6,0.9 would miss
  # 1.0 without this guard).  L9 fix.
  fractions <- unique(c(seq(step, 1, by = step), 1))

  result <- safe_call(
    fn(
      graph,
      strategy = strategy,
      centrality = centrality,
      target = target,
      module_col = params$module_col %||% "Modularity",
      fractions = fractions,
      bootstrap = bootstrap,
      seed = seed,
      plot = TRUE
    ),
    "Failed to run network perturbation."
  )
  if (!result$ok) {
    return(result)
  }

  value <- result$value
  app_success(list(
    curve = as.data.frame(value$curve, check.names = FALSE),
    robustness_index = as.data.frame(value$robustness_index, check.names = FALSE),
    plot = value$plot
  ))
}

safe_perturbation_curve_plot <- function(curve, metric = "LCC_fraction") {
  fn <- resolve_ggnetview_function("ggnetview_perturbation_curve")
  if (is.null(fn)) {
    return(app_failure("Cannot find ggNetView function: ggnetview_perturbation_curve"))
  }
  if (!is.data.frame(curve) || !nrow(curve)) {
    return(app_failure("No perturbation curve available to plot."))
  }
  safe_call(fn(curve, metric = metric), "Failed to draw perturbation curve.")
}

safe_node_influence <- function(graph, source, params = list()) {
  if (!inherits(graph, "igraph") && !inherits(graph, "tbl_graph")) {
    return(app_failure("Node influence requires a graph object."))
  }
  if (is.null(source) || !length(source) || !any(nzchar(source))) {
    return(app_failure("Select at least one source node."))
  }
  fn <- resolve_ggnetview_function("get_node_influence")
  if (is.null(fn)) {
    return(app_failure("Cannot find ggNetView function: get_node_influence"))
  }

  graph <- coerce_tbl_graph(graph)
  alpha <- suppressWarnings(as.numeric(params$alpha %||% 0.5))
  if (is.na(alpha) || alpha <= 0 || alpha >= 1) {
    alpha <- 0.5
  }
  delta <- suppressWarnings(as.numeric(params$delta %||% 1))
  if (is.na(delta) || !is.finite(delta)) {
    delta <- 1
  }

  result <- safe_call(
    fn(
      graph,
      source = source,
      delta = delta,
      alpha = alpha,
      signed = isTRUE(params$signed %||% TRUE),
      drop_source = isTRUE(params$drop_source %||% TRUE)
    ),
    "Failed to compute node influence."
  )
  if (!result$ok) {
    return(result)
  }

  nodes_fn <- resolve_ggnetview_function("get_graph_nodes")
  table <- if (!is.null(nodes_fn)) {
    tryCatch(as.data.frame(nodes_fn(result$value), check.names = FALSE), error = function(e) NULL)
  } else {
    NULL
  }
  if (is.null(table)) {
    table <- as.data.frame(
      igraph::as_data_frame(tidygraph::as.igraph(result$value), what = "vertices"),
      check.names = FALSE
    )
  }
  if ("Influence" %in% names(table)) {
    table <- table[order(-table$Influence), , drop = FALSE]
    rownames(table) <- NULL
  }
  app_success(table)
}

safe_press_perturbation <- function(graph = NULL, cor_mat = NULL, params = list()) {
  fn <- resolve_ggnetview_function("press_perturbation")
  if (is.null(fn)) {
    return(app_failure("Cannot find ggNetView function: press_perturbation"))
  }
  if (is.null(graph) && is.null(cor_mat)) {
    return(app_failure("Press perturbation requires a graph object or a correlation matrix."))
  }

  if (!is.null(cor_mat)) {
    cor_mat <- as.matrix(cor_mat)
    storage.mode(cor_mat) <- "numeric"
  }
  graph_arg <- if (!is.null(graph)) coerce_tbl_graph(graph) else NULL

  source <- params$source
  if (is.character(source) && length(source) == 0L) {
    source <- NULL
  }
  self_regulation <- params$self_regulation
  if (!is.null(self_regulation)) {
    self_regulation <- suppressWarnings(as.numeric(self_regulation))
    if (length(self_regulation) != 1L || is.na(self_regulation)) {
      self_regulation <- NULL
    }
  }

  result <- safe_call(
    fn(
      graph_obj = graph_arg,
      cor_mat = cor_mat,
      self_regulation = self_regulation,
      source = source
    ),
    "Failed to run press perturbation."
  )
  if (!result$ok) {
    return(result)
  }

  value <- result$value
  net_mat <- value$net_effect
  net_effect <- data.frame(
    node = rownames(net_mat) %||% as.character(seq_len(nrow(net_mat))),
    as.data.frame(net_mat, check.names = FALSE),
    check.names = FALSE,
    row.names = NULL
  )
  response <- if (!is.null(value$response)) {
    as.data.frame(value$response, check.names = FALSE)
  } else {
    data.frame()
  }
  meta <- data.frame(
    stable = isTRUE(value$stable),
    eigen_real_max = value$eigen_real_max %||% NA_real_,
    self_regulation = value$self_regulation %||% NA_real_,
    stringsAsFactors = FALSE
  )
  app_success(list(net_effect = net_effect, response = response, meta = meta))
}
