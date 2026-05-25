write_registry_table <- function(data, path) {
  if (is.matrix(data)) {
    ids <- rownames(data)
    if (is.null(ids)) {
      ids <- seq_len(nrow(data))
    }
    data <- data.frame(id = ids, data, check.names = FALSE)
  }
  utils::write.csv(data, path, row.names = FALSE)
  invisible(path)
}

write_registry_object <- function(data, path) {
  saveRDS(data, path)
  invisible(path)
}

write_registry_params <- function(params, path) {
  jsonlite::write_json(params, path, pretty = TRUE, auto_unbox = TRUE, null = "null")
  invisible(path)
}

export_formats_for_type <- function(type) {
  switch(type,
    graph = c("rds", "nodes_csv", "edges_csv", "adjacency_csv", "params_json"),
    plot = c("rds", "png", "pdf", "params_json"),
    result = c("csv", "rds", "params_json"),
    matrix = c("csv", "rds", "params_json"),
    adjacency = c("csv", "rds", "params_json"),
    edge_table = c("csv", "rds", "params_json"),
    module_table = c("csv", "rds", "params_json"),
    annotation = c("csv", "rds", "params_json"),
    wgcna_tom = c("csv", "rds", "params_json"),
    c("rds", "params_json")
  )
}

write_graph_nodes_csv <- function(graph, path) {
  nodes <- igraph::as_data_frame(graph, what = "vertices")
  utils::write.csv(nodes, path, row.names = FALSE)
  invisible(path)
}

write_graph_edges_csv <- function(graph, path) {
  edges <- igraph::as_data_frame(graph, what = "edges")
  utils::write.csv(edges, path, row.names = FALSE)
  invisible(path)
}

write_graph_adjacency_csv <- function(graph, path) {
  adjacency <- as.matrix(igraph::as_adjacency_matrix(graph, attr = "weight", sparse = FALSE))
  utils::write.csv(adjacency, path)
  invisible(path)
}

write_plot_png <- function(plot, path, width = 8, height = 6, dpi = 300) {
  ggplot2::ggsave(filename = path, plot = plot, width = width, height = height, dpi = dpi, device = "png")
  invisible(path)
}

write_plot_pdf <- function(plot, path, width = 8, height = 6) {
  ggplot2::ggsave(filename = path, plot = plot, width = width, height = height, device = "pdf")
  invisible(path)
}
