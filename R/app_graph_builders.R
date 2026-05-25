`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

graph_builder_modes <- function() {
  c(
    "Matrix" = "matrix",
    "Matrix + RMT" = "matrix_rmt",
    "Edge table" = "edge_table",
    "Adjacency matrix" = "adjacency",
    "Double matrix" = "double_matrix",
    "Multi matrix" = "multi_matrix",
    "WGCNA/TOM" = "wgcna_tom",
    "Consensus" = "consensus"
  )
}

normalize_module_table <- function(module_table) {
  if (is.null(module_table)) {
    return(NULL)
  }

  module_table <- as.data.frame(module_table, stringsAsFactors = FALSE)
  names_lower <- tolower(names(module_table))
  node_col <- match(TRUE, names_lower %in% c("node", "name", "id"))
  module_col <- match(TRUE, names_lower %in% c("module", "modularity", "group", "class"))

  if (is.na(node_col) || is.na(module_col)) {
    stop("Module table must contain node/name/id and module/modularity/group columns.", call. = FALSE)
  }

  data.frame(
    name = module_table[[node_col]],
    Modularity = module_table[[module_col]],
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

normalize_wgcna_module_table <- function(module_table) {
  module_table <- normalize_module_table(module_table)
  if (is.null(module_table)) {
    return(NULL)
  }
  data.frame(
    ID = module_table$name,
    Module = module_table$Modularity,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

normalize_graph_builder_params <- function(mode, params = list()) {
  params <- params %||% list()
  mode <- as.character(mode)

  if (mode %in% c("matrix", "matrix_rmt")) {
    defaults <- list(
      transfrom.method = "none",
      method = "cor",
      cor.method = "pearson",
      proc = "none",
      r.threshold = 0.1,
      p.threshold = 1,
      module.method = "Fast_greedy"
    )
    return(utils::modifyList(defaults, params, keep.null = TRUE))
  }

  if (mode %in% c("edge_table", "adjacency", "double_matrix", "multi_matrix", "wgcna_tom", "consensus")) {
    return(params)
  }

  params
}

required_builder_inputs <- function(mode) {
  switch(mode,
    matrix = c("matrix"),
    matrix_rmt = c("matrix"),
    edge_table = c("edge_table"),
    adjacency = c("adjacency"),
    double_matrix = c("matrix_a", "matrix_b"),
    multi_matrix = c("matrices"),
    wgcna_tom = c("tom"),
    consensus = c("graphs_or_adjacency"),
    character()
  )
}

validate_graph_builder_inputs <- function(mode, inputs) {
  missing <- setdiff(required_builder_inputs(mode), names(inputs))
  if (length(missing) > 0) {
    return(app_failure(sprintf("Missing required builder input: %s", paste(missing, collapse = ", "))))
  }
  app_success(TRUE)
}

graph_builder_function_name <- function(mode, inputs) {
  switch(mode,
    matrix = "build_graph_from_mat",
    matrix_rmt = "build_graph_from_mat",
    edge_table = if (!is.null(inputs$module_table)) "build_graph_from_module" else "build_graph_from_df",
    adjacency = if (!is.null(inputs$module_table)) "build_graph_from_adj_mat_module" else "build_graph_from_adj_mat",
    double_matrix = if (!is.null(inputs$module_table)) "build_graph_from_double_mat_with_module" else "build_graph_from_double_mat",
    multi_matrix = "build_graph_from_multi_mat",
    wgcna_tom = "build_graph_from_wgcna",
    consensus = "build_graph_from_consensus",
    NULL
  )
}

graph_builder_call_args <- function(mode, inputs, params) {
  module_annotation <- normalize_module_table(inputs$module_table)

  if (identical(mode, "wgcna_tom")) {
    module <- normalize_wgcna_module_table(inputs$module_table)
    tom <- inputs$tom
    if (is.data.frame(tom)) {
      tom <- as.matrix(tom)
    }
    if (is.matrix(tom) && nrow(tom) == ncol(tom)) {
      trans_fn <- resolve_ggnetview_function("trans_TOM_in_WGCNA")
      if (is.null(trans_fn)) {
        stop("Cannot find ggNetView function: trans_TOM_in_WGCNA", call. = FALSE)
      }
      mat_for_names <- inputs$matrix
      if (is.null(mat_for_names)) {
        node_names <- rownames(tom) %||% colnames(tom) %||% paste0("node_", seq_len(ncol(tom)))
        mat_for_names <- matrix(0, nrow = 1, ncol = length(node_names), dimnames = list("sample_1", node_names))
      } else if (is.data.frame(mat_for_names) || is.matrix(mat_for_names)) {
        mat_for_names <- t(as.matrix(mat_for_names))
      }
      edge_df <- trans_fn(TOM = tom, mat = mat_for_names, threshold = params$threshold %||% NULL, top_k = params$top_k %||% NULL)
      params$threshold <- NULL
      params$top_k <- NULL
      return(c(list(edge_df, module = module), params))
    }
    return(c(list(tom, module = module), params))
  }

  switch(mode,
    matrix = c(list(inputs$matrix), params),
    matrix_rmt = c(list(inputs$matrix), params),
    edge_table = if (!is.null(module_annotation)) {
      c(list(inputs$edge_table, node_annotation = module_annotation), params)
    } else {
      c(list(inputs$edge_table), params)
    },
    adjacency = if (!is.null(module_annotation)) {
      c(list(inputs$adjacency, node_annotation = module_annotation), params)
    } else {
      c(list(inputs$adjacency), params)
    },
    double_matrix = if (!is.null(module_annotation)) {
      c(list(inputs$matrix_a, inputs$matrix_b, node_annotation = module_annotation), params)
    } else {
      c(list(inputs$matrix_a, inputs$matrix_b), params)
    },
    multi_matrix = c(unname(inputs$matrices), params),
    wgcna_tom = c(list(inputs$tom), params),
    consensus = c(list(inputs$graphs_or_adjacency), params)
  )
}

safe_graph_builder <- function(mode, inputs, params = list()) {
  mode <- as.character(mode)
  inputs <- inputs %||% list()
  params <- normalize_graph_builder_params(mode, params)

  validation <- validate_graph_builder_inputs(mode, inputs)
  if (!isTRUE(validation$ok)) {
    return(validation)
  }

  fn_name <- graph_builder_function_name(mode, inputs)
  if (is.null(fn_name)) {
    return(app_failure(paste("Unsupported graph builder mode:", mode)))
  }

  fn <- resolve_ggnetview_function(fn_name)
  if (is.null(fn)) {
    return(app_failure(paste("Cannot find ggNetView function:", fn_name)))
  }

  call_args <- tryCatch(
    graph_builder_call_args(mode, inputs, params),
    error = function(e) e
  )
  if (inherits(call_args, "error")) {
    return(app_failure("Failed to prepare graph builder inputs.", trace = conditionMessage(call_args)))
  }

  safe_call(
    do.call(fn, call_args),
    paste("Failed to build graph with", fn_name)
  )
}

safe_rmt_threshold <- function(matrix, params = list()) {
  fn <- resolve_ggnetview_function("ggNetView_RMT")
  if (is.null(fn)) {
    return(app_failure("Cannot find ggNetView function: ggNetView_RMT"))
  }

  safe_call(
    do.call(fn, c(list(matrix), params)),
    "Failed to calculate RMT threshold."
  )
}
