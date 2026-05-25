safe_call <- function(expr, user_message) {
  tryCatch(
    app_success(force(expr)),
    error = function(e) app_failure(user_message, trace = conditionMessage(e))
  )
}

.ggnetview_source_cache <- new.env(parent = emptyenv())

local_source_env <- function(root) {
  root <- normalizePath(root, mustWork = FALSE)
  if (exists(root, envir = .ggnetview_source_cache, inherits = FALSE)) {
    return(get(root, envir = .ggnetview_source_cache, inherits = FALSE))
  }

  r_dir <- file.path(root, "R")
  if (!dir.exists(r_dir)) {
    return(NULL)
  }

  env <- new.env(parent = .GlobalEnv)
  if (requireNamespace("magrittr", quietly = TRUE)) {
    assign("%>%", magrittr::`%>%`, envir = env)
  }
  files <- sort(list.files(r_dir, pattern = "[.]R$", full.names = TRUE))
  for (file in files) {
    sys.source(file, envir = env)
  }
  assign(root, env, envir = .ggnetview_source_cache)
  env
}

resolve_ggnetview_function <- function(name) {
  source_roots <- unique(normalizePath(c(
    getwd(),
    file.path(getwd(), "..", ".."),
    getOption("ggnetview.app_root", NA_character_)
  ), mustWork = FALSE))
  source_roots <- source_roots[!is.na(source_roots)]

  for (root in source_roots) {
    source_path <- file.path(root, "R", paste0(name, ".R"))
    if (file.exists(source_path)) {
      env <- local_source_env(root)
      if (exists(name, envir = env, mode = "function", inherits = FALSE)) {
        return(get(name, envir = env, mode = "function", inherits = FALSE))
      }
    }
  }

  if (exists(name, mode = "function", inherits = TRUE)) {
    return(get(name, mode = "function", inherits = TRUE))
  }

  ns <- tryCatch(asNamespace("ggNetView"), error = function(e) NULL)
  if (!is.null(ns) && exists(name, envir = ns, mode = "function", inherits = FALSE)) {
    return(get(name, envir = ns, mode = "function", inherits = FALSE))
  }

  NULL
}

safe_build_graph <- function(data, builder, params = list()) {
  if (!is.character(builder) || length(builder) != 1L || is.na(builder) || !nzchar(builder)) {
    return(app_failure("Unsupported graph builder: <invalid>"))
  }

  mode <- switch(builder,
    matrix = "matrix",
    adjacency = "adjacency",
    edge_table = "edge_table",
    NULL
  )
  if (is.null(mode)) {
    return(app_failure(paste("Unsupported graph builder:", builder)))
  }

  input_name <- switch(mode,
    matrix = "matrix",
    adjacency = "adjacency",
    edge_table = "edge_table"
  )

  if (!exists("safe_graph_builder", mode = "function", inherits = TRUE)) {
    source_roots <- unique(normalizePath(c(
      getwd(),
      file.path(getwd(), "..", ".."),
      getOption("ggnetview.app_root", NA_character_)
    ), mustWork = FALSE))
    source_roots <- source_roots[!is.na(source_roots)]

    for (root in source_roots) {
      graph_builder_path <- file.path(root, "R", "app_graph_builders.R")
      if (file.exists(graph_builder_path)) {
        sys.source(graph_builder_path, envir = .GlobalEnv)
        break
      }
    }
  }

  if (!exists("safe_graph_builder", mode = "function", inherits = TRUE)) {
    fn_name <- switch(mode,
      matrix = "build_graph_from_mat",
      adjacency = "build_graph_from_adj_mat",
      edge_table = "build_graph_from_df"
    )
    fn <- resolve_ggnetview_function(fn_name)
    if (is.null(fn)) {
      return(app_failure(paste("Cannot find ggNetView function:", fn_name)))
    }
    return(safe_call(
      do.call(fn, c(list(data), params)),
      paste("Failed to build graph with", fn_name)
    ))
  }

  safe_graph_builder(
    mode = mode,
    inputs = stats::setNames(list(data), input_name),
    params = params
  )
}

safe_plot_ggnetview <- function(graph, params = list()) {
  if (!inherits(graph, "igraph")) {
    return(app_failure("Visual Lab requires an igraph graph object."))
  }

  fn <- resolve_ggnetview_function("ggNetView")
  if (is.null(fn)) {
    return(app_failure("Cannot find ggNetView function: ggNetView"))
  }

  safe_call(
    do.call(fn, c(list(graph), params)),
    "Failed to generate ggNetView plot."
  )
}

safe_topology <- function(graph, params = list()) {
  if (!inherits(graph, "igraph")) {
    return(app_failure("Topology Results requires an igraph graph object."))
  }

  fn <- resolve_ggnetview_function("get_network_topology")
  if (is.null(fn)) {
    return(app_failure("Cannot find ggNetView function: get_network_topology"))
  }

  safe_call(
    do.call(fn, c(list(graph), params)),
    "Failed to calculate network topology."
  )
}
