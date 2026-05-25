safe_graph_info <- function(graph) {
  if (!inherits(graph, "igraph")) {
    return(app_failure("Graph info requires an igraph graph object."))
  }

  fn <- resolve_ggnetview_function("get_info_from_graph")
  if (is.null(fn)) {
    return(app_failure("Cannot find ggNetView function: get_info_from_graph"))
  }

  safe_call(
    fn(graph),
    "Failed to extract graph information."
  )
}

normalize_graph_modularity_factor <- function(graph) {
  if (!inherits(graph, "igraph")) {
    return(graph)
  }

  modularity <- igraph::vertex_attr(graph, "Modularity")
  if (is.null(modularity)) {
    modularity <- igraph::vertex_attr(graph, "modularity2")
  }
  if (is.null(modularity)) {
    modularity <- igraph::vertex_attr(graph, "modularity")
  }
  if (is.null(modularity)) {
    return(graph)
  }

  igraph::set_vertex_attr(graph, "Modularity", value = factor(as.character(modularity)))
}

graph_module_choices <- function(graph) {
  if (!inherits(graph, "igraph")) {
    return(stats::setNames(character(), character()))
  }

  nodes <- tryCatch(igraph::as_data_frame(graph, what = "vertices"), error = function(e) data.frame())
  if (!nrow(nodes)) {
    return(stats::setNames(character(), character()))
  }

  module_col <- intersect(c("Modularity", "modularity2", "modularity", "module"), names(nodes))
  if (!length(module_col)) {
    return(stats::setNames(character(), character()))
  }

  modules <- sort(unique(as.character(nodes[[module_col[[1]]]])))
  modules <- modules[!is.na(modules) & nzchar(modules)]
  stats::setNames(modules, modules)
}

safe_module_subgraph <- function(graph, select_module = NULL) {
  if (!inherits(graph, "igraph")) {
    return(app_failure("Subgraph extraction requires an igraph graph object."))
  }

  fn <- resolve_ggnetview_function("get_subgraph")
  if (is.null(fn)) {
    return(app_failure("Cannot find ggNetView function: get_subgraph"))
  }

  graph <- normalize_graph_modularity_factor(graph)

  safe_call(
    fn(graph, select_module = select_module),
    "Failed to extract module subgraph."
  )
}

safe_sample_subgraph <- function(graph, matrix, select_sample = NULL, min_abundance = 0, combine = "union") {
  if (!inherits(graph, "igraph")) {
    return(app_failure("Sample subgraph extraction requires an igraph graph object."))
  }

  fn <- resolve_ggnetview_function("get_sample_subgraph")
  if (is.null(fn)) {
    return(app_failure("Cannot find ggNetView function: get_sample_subgraph"))
  }

  safe_call(
    fn(
      graph_obj = graph,
      mat = matrix,
      select_sample = select_sample,
      min_abundance = min_abundance,
      combine = combine
    ),
    "Failed to extract sample subgraph."
  )
}

subgraph_selected_graph <- function(result) {
  if (!is.list(result)) {
    return(NULL)
  }
  selected <- result$sub_graph_select
  if (inherits(selected, "igraph")) {
    return(selected)
  }
  NULL
}

subgraph_stat_table <- function(result, stat_name = c("stat_module", "stat_sample")) {
  stat_name <- match.arg(stat_name)
  if (!is.list(result) || is.null(result[[stat_name]])) {
    return(data.frame())
  }
  as.data.frame(result[[stat_name]], check.names = FALSE)
}
