node_table_from_graph <- function(graph) {
  if (!inherits(graph, "igraph")) {
    return(data.frame())
  }

  fn <- resolve_ggnetview_function("get_graph_nodes")
  if (!is.null(fn)) {
    return(as.data.frame(fn(graph), check.names = FALSE))
  }

  igraph::as_data_frame(graph, what = "vertices")
}

adjacency_from_graph <- function(graph) {
  fn <- resolve_ggnetview_function("get_graph_adjacency")
  if (!is.null(fn)) {
    return(fn(graph))
  }
  as.matrix(igraph::as_adjacency_matrix(graph, attr = "weight", sparse = FALSE))
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
