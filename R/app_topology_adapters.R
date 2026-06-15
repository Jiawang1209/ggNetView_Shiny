node_table_from_graph <- function(graph) {
  if (!inherits(graph, "igraph")) {
    return(data.frame())
  }

  fn <- resolve_ggnetview_function("get_graph_nodes")
  if (!is.null(fn)) {
    result <- tryCatch(
      as.data.frame(fn(graph), check.names = FALSE),
      error = function(e) NULL
    )
    if (!is.null(result)) {
      return(result)
    }
  }

  igraph::as_data_frame(graph, what = "vertices")
}

adjacency_from_graph <- function(graph) {
  fn <- resolve_ggnetview_function("get_graph_adjacency")
  if (!is.null(fn)) {
    result <- tryCatch(fn(graph), error = function(e) NULL)
    if (!is.null(result)) {
      return(result)
    }
  }
  attr <- if ("weight" %in% igraph::edge_attr_names(graph)) "weight" else NULL
  as.matrix(igraph::as_adjacency_matrix(graph, attr = attr, sparse = FALSE))
}

safe_node_centrality <- function(graph, measures = "all", weighted = FALSE) {
  if (!inherits(graph, "igraph")) {
    return(app_failure("Node centrality requires an igraph graph object."))
  }

  fn <- resolve_ggnetview_function("get_node_centrality")
  if (is.null(fn)) {
    return(app_failure("Cannot find ggNetView function: get_node_centrality"))
  }

  result <- safe_call(
    fn(graph, measures = measures, weighted = weighted),
    "Failed to calculate node centrality."
  )
  if (!result$ok) {
    return(result)
  }

  app_success(node_table_from_graph(result$value))
}

safe_sample_topology <- function(graph, matrix, params = list()) {
  if (!inherits(graph, "igraph")) {
    return(app_failure("Sample topology requires an igraph graph object."))
  }
  if (is.null(matrix)) {
    return(app_failure("Sample topology requires the matrix used to build or interpret the graph."))
  }

  matrix <- as.matrix(matrix)
  storage.mode(matrix) <- "numeric"
  use_parallel_api <- isTRUE(params$parallel_api)
  fn_name <- if (use_parallel_api) {
    "get_sample_subgraph_topology_parallel"
  } else {
    "get_sample_subgraph_topology"
  }
  fn <- resolve_ggnetview_function(fn_name)
  if (is.null(fn)) {
    return(app_failure(paste("Cannot find ggNetView function:", fn_name)))
  }

  defaults <- list(
    graph_obj = graph,
    mat = matrix,
    transfrom.method = "none",
    method = "cor",
    cor.method = "pearson",
    proc = "none",
    r.threshold = 0.2,
    p.threshold = 1,
    bootstrap = 0
  )
  if (identical(fn_name, "get_sample_subgraph_topology_parallel")) {
    defaults$parallel <- FALSE
    defaults$n_workers <- 1L
  }
  params$parallel_api <- NULL
  call_args <- utils::modifyList(defaults, params, keep.null = TRUE)
  allowed <- names(formals(fn))
  call_args <- call_args[names(call_args) %in% allowed]

  result <- safe_call(
    do.call(fn, call_args),
    "Failed to calculate sample-level topology."
  )
  if (!result$ok) {
    return(result)
  }

  value <- result$value
  if (is.list(value)) {
    value$topology <- if (is.data.frame(value$topology)) value$topology else data.frame()
    value$Robustness <- if (is.data.frame(value$Robustness)) value$Robustness else data.frame()
    value$sample_stat <- if (is.data.frame(value$sample_stat)) value$sample_stat else data.frame()
  }
  app_success(value)
}

safe_node_ivi <- function(graph, scale = "range", ncores = 1L) {
  if (!inherits(graph, "igraph")) {
    return(app_failure("Node IVI requires an igraph graph object."))
  }

  fn <- resolve_ggnetview_function("get_node_ivi")
  if (is.null(fn)) {
    return(app_failure("Cannot find ggNetView function: get_node_ivi"))
  }

  result <- safe_call(
    fn(graph, scale = scale, ncores = ncores),
    "Failed to calculate node IVI."
  )
  if (!result$ok) {
    return(result)
  }

  app_success(node_table_from_graph(result$value))
}

safe_zipi <- function(graph, zi_threshold = 2.5, pi_threshold = 0.62) {
  if (!inherits(graph, "igraph")) {
    return(app_failure("Zi-Pi requires an igraph graph object."))
  }
  if (!is.finite(zi_threshold)) {
    return(app_failure(
      paste0("zi_threshold must be a finite number (got: ", zi_threshold, ").")
    ))
  }
  if (!is.finite(pi_threshold) || pi_threshold < 0 || pi_threshold > 1) {
    return(app_failure(
      paste0("pi_threshold must be a finite number in [0, 1] (got: ", pi_threshold, ").")
    ))
  }

  fn <- resolve_ggnetview_function("ggnetview_zipi")
  if (is.null(fn)) {
    return(app_failure("Cannot find ggNetView function: ggnetview_zipi"))
  }

  nodes <- node_table_from_graph(graph)
  adjacency <- adjacency_from_graph(graph)
  module_col <- intersect(c("Modularity", "modularity2", "modularity", "module"), names(nodes))
  degree_col <- intersect(c("Degree", "degree"), names(nodes))

  if (!length(module_col)) {
    return(app_failure("Zi-Pi requires a module column on graph nodes."))
  }
  if (!length(degree_col)) {
    nodes$Degree <- igraph::degree(graph)
    degree_col <- "Degree"
  }

  result <- safe_call(
    fn(
      nodes_bulk = nodes,
      z_bulk_mat = adjacency,
      modularity_col = module_col[[1]],
      degree_col = degree_col[[1]],
      zi_threshold = zi_threshold,
      pi_threshold = pi_threshold
    ),
    "Failed to calculate Zi-Pi keystone classification."
  )
  if (!result$ok) {
    return(result)
  }
  if (is.list(result$value) && is.data.frame(result$value$data)) {
    return(app_success(result$value$data))
  }
  result
}
