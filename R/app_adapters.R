safe_call <- function(expr, user_message) {
  tryCatch(
    app_success(force(expr)),
    error = function(e) app_failure(user_message, trace = conditionMessage(e))
  )
}

resolve_ggnetview_function <- function(name) {
  if (exists(name, mode = "function", inherits = TRUE)) {
    return(get(name, mode = "function", inherits = TRUE))
  }

  source_roots <- unique(normalizePath(c(
    getwd(),
    file.path(getwd(), "..", ".."),
    getOption("ggnetview.app_root", NA_character_)
  ), mustWork = FALSE))
  source_roots <- source_roots[!is.na(source_roots)]

  for (root in source_roots) {
    source_path <- file.path(root, "R", paste0(name, ".R"))
    if (file.exists(source_path)) {
      env <- new.env(parent = parent.frame())
      sys.source(source_path, envir = env)
      if (exists(name, envir = env, mode = "function", inherits = FALSE)) {
        return(get(name, envir = env, mode = "function", inherits = FALSE))
      }
    }
  }

  ns <- tryCatch(asNamespace("ggNetView"), error = function(e) NULL)
  if (!is.null(ns) && exists(name, envir = ns, mode = "function", inherits = FALSE)) {
    return(get(name, envir = ns, mode = "function", inherits = FALSE))
  }

  NULL
}

safe_build_graph <- function(data, builder, params = list()) {
  builder_map <- list(
    matrix = "build_graph_from_mat",
    adjacency = "build_graph_from_adj_mat",
    edge_table = "build_graph_from_df"
  )

  if (!is.character(builder) || length(builder) != 1L || is.na(builder) || !nzchar(builder)) {
    return(app_failure("Unsupported graph builder: <invalid>"))
  }

  fn_name <- builder_map[[builder]]
  if (is.null(fn_name)) {
    return(app_failure(paste("Unsupported graph builder:", builder)))
  }

  fn <- resolve_ggnetview_function(fn_name)
  if (is.null(fn)) {
    return(app_failure(paste("Cannot find ggNetView function:", fn_name)))
  }

  safe_call(
    do.call(fn, c(list(data), params)),
    paste("Failed to build graph with", fn_name)
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
