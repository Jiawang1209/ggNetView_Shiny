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

workflow_snapshot_types <- function() {
  c(
    "matrix",
    "adjacency",
    "edge_table",
    "module_table",
    "annotation",
    "wgcna_tom",
    "sample_metadata",
    "env_matrix",
    "result"
  )
}

workflow_item_data_snapshot <- function(item) {
  if (!item$type %in% workflow_snapshot_types()) {
    return(NULL)
  }

  data <- item$data
  if (!is.matrix(data) && !is.data.frame(data)) {
    return(NULL)
  }

  list(
    format = "rds-base64",
    type = item$type,
    class = class(data),
    value = jsonlite::base64_enc(serialize(data, NULL, version = 3))
  )
}

workflow_decode_data_snapshot <- function(snapshot) {
  if (is.null(snapshot) || is.null(snapshot$value)) {
    return(NULL)
  }
  if (!identical(snapshot$format %||% "", "rds-base64")) {
    stop("Unsupported workflow data snapshot format.", call. = FALSE)
  }
  unserialize(jsonlite::base64_dec(snapshot$value))
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
  item_records$data_snapshot <- I(lapply(items, workflow_item_data_snapshot))

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

manifest_param_raw <- function(item, key, default = NULL) {
  params <- item$params
  if (is.null(params) || is.null(params[[key]])) {
    return(default)
  }
  params[[key]]
}

manifest_param_vector <- function(item, key, default = character()) {
  value <- manifest_param_raw(item, key, default = default)
  if (is.null(value)) {
    return(default)
  }
  value <- unlist(value, use.names = FALSE)
  value <- as.character(value)
  value[nzchar(value)]
}

workflow_replay_builder <- function(item) {
  builder <- manifest_param_value(item, "builder")
  if (!nzchar(builder)) {
    return("")
  }
  builder
}

workflow_replay_source_ids <- function(item) {
  source_ids <- manifest_param_vector(item, "source_ids")
  if (length(source_ids)) {
    return(unique(source_ids))
  }

  source <- item$source %||% ""
  source <- unlist(strsplit(as.character(source), ",", fixed = TRUE), use.names = FALSE)
  source <- trimws(source)
  unique(source[nzchar(source)])
}

workflow_replay_builder_modes <- function() {
  if (exists("graph_builder_modes", mode = "function", inherits = TRUE)) {
    return(unname(graph_builder_modes()))
  }
  c("matrix", "matrix_rmt", "edge_table", "adjacency", "double_matrix", "multi_matrix", "wgcna_tom", "consensus")
}

is_workflow_replay_builder_item <- function(item) {
  identical(item$type %||% "", "graph") &&
    workflow_replay_builder(item) %in% workflow_replay_builder_modes()
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
      builder = character(),
      status = character(),
      replay_reason = character(),
      stringsAsFactors = FALSE
    ))
  }

  records <- lapply(seq_along(items), function(i) {
    item <- items[[i]]
    recipe <- manifest_param_value(item, "recipe")
    has_recipe <- nzchar(recipe) && !identical(recipe, "manual_starter")
    builder <- workflow_replay_builder(item)
    has_builder <- is_workflow_replay_builder_item(item)
    status <- if (has_recipe) {
      "recipe-output-needs-rerun"
    } else if (has_builder) {
      "builder-output-needs-rerun"
    } else {
      "input-or-existing-object"
    }
    replay_reason <- if (has_recipe) {
      "gallery recipe can be rerun"
    } else if (has_builder) {
      "graph builder params and source IDs are present"
    } else {
      "manifest does not include a supported replay recipe or graph builder"
    }
    data.frame(
      step = i,
      id = item$id %||% "",
      name = item$name %||% "",
      type = item$type %||% "",
      source = item$source %||% "",
      recipe = recipe,
      builder = builder,
      status = status,
      replay_reason = replay_reason,
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, records)
}

workflow_replay_builder_items <- function(manifest) {
  items <- manifest$items %||% manifest
  if (!length(items)) {
    return(list())
  }
  Filter(is_workflow_replay_builder_item, items)
}

workflow_restore_manifest_inputs <- function(registry, manifest) {
  items <- manifest$items %||% list()
  if (!length(items)) {
    return(app_success(list(restored = 0L, skipped = 0L), message = "No workflow items to restore."))
  }

  restored <- 0L
  skipped <- 0L
  restored_ids <- character()
  for (item in items) {
    snapshot <- item$data_snapshot
    if (is.null(snapshot) || !item$type %in% workflow_snapshot_types()) {
      skipped <- skipped + 1L
      next
    }

    data <- tryCatch(
      workflow_decode_data_snapshot(snapshot),
      error = function(e) e
    )
    if (inherits(data, "error")) {
      return(app_failure(
        sprintf("Could not restore workflow object '%s'.", item$name %||% item$id),
        trace = conditionMessage(data)
      ))
    }

    params <- item$params %||% list()
    params$restored_from_manifest_id <- item$id %||% ""
    warnings <- c(
      item$warnings %||% character(),
      "Restored from workflow manifest data snapshot."
    )
    restored_item <- registry_add_with_id(
      registry,
      id = item$id %||% "",
      name = item$name %||% item$id %||% "restored_object",
      type = item$type,
      data = data,
      source = item$source %||% "workflow_manifest",
      params = params,
      warnings = warnings
    )
    restored <- restored + 1L
    restored_ids <- c(restored_ids, restored_item$id)
  }

  app_success(
    list(restored = restored, skipped = skipped, restored_ids = restored_ids),
    message = sprintf("Restored %s workflow input object(s).", restored)
  )
}

workflow_replay_recipes <- function(plan, known_recipes) {
  if (is.null(plan) || !nrow(plan) || !"recipe" %in% names(plan)) {
    return(character())
  }
  recipes <- unique(as.character(plan$recipe))
  recipes <- recipes[nzchar(recipes)]
  recipes <- setdiff(recipes, "manual_starter")
  recipes[recipes %in% known_recipes]
}

workflow_replay_graph_builder_params <- function(item) {
  params <- item$params %||% list()
  params$builder <- NULL
  params$source_ids <- NULL
  params$module_id <- NULL
  params$recipe <- NULL
  params
}

workflow_replay_graph_builder_inputs <- function(builder, source_items, module_item = NULL) {
  source_data <- lapply(source_items, `[[`, "data")
  source_names <- vapply(source_items, function(item) item$name %||% item$id, character(1))

  inputs <- switch(builder,
    matrix = list(matrix = source_data[[1]]),
    matrix_rmt = list(matrix = source_data[[1]]),
    edge_table = list(edge_table = source_data[[1]]),
    adjacency = list(adjacency = source_data[[1]]),
    double_matrix = list(matrix_a = source_data[[1]], matrix_b = source_data[[2]]),
    multi_matrix = {
      names(source_data) <- source_names
      list(matrices = source_data)
    },
    wgcna_tom = list(tom = source_data[[1]]),
    consensus = {
      values <- lapply(source_data, function(value) {
        if (inherits(value, "igraph")) {
          return(as.matrix(igraph::as_adjacency_matrix(value, attr = "weight", sparse = FALSE)))
        }
        value
      })
      names(values) <- source_names
      list(graphs_or_adjacency = values)
    },
    list()
  )

  if (!is.null(module_item)) {
    inputs$module_table <- module_item$data
  }
  inputs
}

workflow_replay_registry_get <- function(registry, id) {
  shiny::isolate(registry_get(registry, id))
}

workflow_replay_graph_builder <- function(registry, item) {
  builder <- workflow_replay_builder(item)
  source_ids <- workflow_replay_source_ids(item)
  if (!length(source_ids)) {
    return(app_failure(sprintf("Graph builder replay for '%s' has no source IDs.", item$name %||% item$id)))
  }

  source_items <- lapply(source_ids, function(id) workflow_replay_registry_get(registry, id))
  missing <- source_ids[vapply(source_items, is.null, logical(1))]
  if (length(missing)) {
    return(app_failure(sprintf(
      "Graph builder replay for '%s' cannot run because source object is not available: %s",
      item$name %||% item$id,
      paste(missing, collapse = ", ")
    )))
  }

  module_id <- manifest_param_value(item, "module_id")
  module_item <- if (nzchar(module_id)) workflow_replay_registry_get(registry, module_id) else NULL
  if (nzchar(module_id) && is.null(module_item)) {
    return(app_failure(sprintf(
      "Graph builder replay for '%s' cannot run because module object is not available: %s",
      item$name %||% item$id,
      module_id
    )))
  }

  inputs <- workflow_replay_graph_builder_inputs(builder, source_items, module_item)
  params <- workflow_replay_graph_builder_params(item)
  result <- safe_graph_builder(builder, inputs = inputs, params = params)
  if (!isTRUE(result$ok)) {
    return(app_failure(
      sprintf("Graph builder replay failed for '%s'.", item$name %||% item$id),
      trace = result$trace %||% result$message
    ))
  }
  if (!inherits(result$value, "igraph")) {
    return(app_failure(sprintf("Graph builder replay for '%s' did not return an igraph object.", item$name %||% item$id)))
  }

  replay_params <- item$params %||% list()
  replay_params$builder <- builder
  replay_params$source_ids <- source_ids
  if (nzchar(module_id)) {
    replay_params$module_id <- module_id
  }
  replayed <- registry_add(
    registry,
    name = paste0(item$name %||% item$id, "_replay"),
    type = "graph",
    data = result$value,
    source = paste(source_ids, collapse = ","),
    params = replay_params,
    warnings = c(item$warnings %||% character(), "Replayed from workflow manifest using current registry sources.")
  )
  app_success(replayed, message = paste("Replayed graph builder output:", replayed$name))
}

workflow_replay_graph_builders <- function(registry, manifest_or_items) {
  items <- workflow_replay_builder_items(manifest_or_items)
  lapply(items, function(item) workflow_replay_graph_builder(registry, item))
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
