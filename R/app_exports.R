if (!exists("%||%", mode = "function")) {
  `%||%` <- function(x, y) {
    if (is.null(x)) y else x
  }
}

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

workflow_manifest <- function(registry) {
  items <- shiny::isolate(registry$items)
  item_records <- if (length(items)) {
    data.frame(
      id = vapply(items, `[[`, character(1), "id"),
      name = vapply(items, `[[`, character(1), "name"),
      type = vapply(items, `[[`, character(1), "type"),
      source = vapply(items, function(item) item$source %||% "", character(1)),
      created_at = vapply(items, function(item) format(item$created_at, "%Y-%m-%dT%H:%M:%OS%z"), character(1)),
      stringsAsFactors = FALSE
    )
  } else {
    data.frame(
      id = character(),
      name = character(),
      type = character(),
      source = character(),
      created_at = character(),
      stringsAsFactors = FALSE
    )
  }
  item_records$summary <- I(lapply(items, function(item) item$summary %||% list()))
  item_records$params <- I(lapply(items, function(item) item$params %||% list()))
  item_records$warnings <- I(lapply(items, function(item) item$warnings %||% character()))

  list(
    app = "ggNetView Shiny",
    generated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS%z"),
    item_count = nrow(item_records),
    items = item_records
  )
}

write_workflow_manifest <- function(registry, path) {
  jsonlite::write_json(
    workflow_manifest(registry),
    path,
    pretty = TRUE,
    auto_unbox = TRUE,
    null = "null"
  )
  invisible(path)
}

read_workflow_manifest <- function(path) {
  manifest <- jsonlite::read_json(path, simplifyVector = FALSE)
  if (!is.list(manifest) || !identical(manifest$app, "ggNetView Shiny")) {
    stop("Workflow manifest is not a ggNetView Shiny manifest.", call. = FALSE)
  }
  if (is.null(manifest$items) || !is.list(manifest$items)) {
    stop("Workflow manifest is missing item records.", call. = FALSE)
  }
  manifest
}

manifest_param_value <- function(item, key, default = "") {
  params <- item$params
  if (is.null(params) || is.null(params[[key]])) {
    return(default)
  }
  value <- params[[key]]
  if (length(value) == 0) {
    return(default)
  }
  paste(as.character(value), collapse = ",")
}

workflow_replay_plan <- function(manifest) {
  items <- manifest$items %||% list()
  if (!length(items)) {
    return(data.frame(
      step = integer(),
      id = character(),
      name = character(),
      type = character(),
      source = character(),
      recipe = character(),
      status = character(),
      stringsAsFactors = FALSE
    ))
  }

  records <- lapply(seq_along(items), function(i) {
    item <- items[[i]]
    recipe <- manifest_param_value(item, "recipe")
    has_recipe <- nzchar(recipe) && !identical(recipe, "manual_starter")
    data.frame(
      step = i,
      id = item$id %||% "",
      name = item$name %||% "",
      type = item$type %||% "",
      source = item$source %||% "",
      recipe = recipe,
      status = if (has_recipe) "recipe-output-needs-rerun" else "input-or-existing-object",
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, records)
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
