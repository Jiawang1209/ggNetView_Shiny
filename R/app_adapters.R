safe_call <- function(expr, user_message) {
  tryCatch(
    app_success(force(expr)),
    error = function(e) app_failure(user_message, trace = conditionMessage(e))
  )
}

safe_build_graph <- function(data, builder, params = list()) {
  builder_map <- list(
    matrix = "build_graph_from_mat",
    adjacency = "build_graph_from_adj_mat",
    edge_table = "build_graph_from_df"
  )

  fn_name <- builder_map[[builder]]
  if (is.null(fn_name) || !exists(fn_name, mode = "function")) {
    return(app_failure(paste("Unsupported graph builder:", builder)))
  }

  fn <- get(fn_name, mode = "function")
  safe_call(
    do.call(fn, c(list(data), params)),
    paste("Failed to build graph with", fn_name)
  )
}

safe_plot_ggnetview <- function(graph, params = list()) {
  if (!inherits(graph, "igraph")) {
    return(app_failure("Visual Lab requires an igraph graph object."))
  }

  safe_call(
    do.call(ggNetView, c(list(graph), params)),
    "Failed to generate ggNetView plot."
  )
}

safe_topology <- function(graph, params = list()) {
  if (!inherits(graph, "igraph")) {
    return(app_failure("Topology Results requires an igraph graph object."))
  }

  safe_call(
    do.call(get_network_topology, c(list(graph), params)),
    "Failed to calculate network topology."
  )
}
