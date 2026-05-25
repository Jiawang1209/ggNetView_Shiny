app_result <- function(ok, value = NULL, message = NULL, warnings = character(), trace = NULL) {
  structure(
    list(
      ok = isTRUE(ok),
      value = value,
      message = message,
      warnings = warnings,
      trace = trace
    ),
    class = "ggnetview_app_result"
  )
}

app_success <- function(value = NULL, message = NULL, warnings = character()) {
  app_result(TRUE, value = value, message = message, warnings = warnings)
}

app_failure <- function(message, trace = NULL, warnings = character()) {
  app_result(FALSE, value = NULL, message = message, warnings = warnings, trace = trace)
}

read_user_table <- function(path, filename = path) {
  ext <- tolower(tools::file_ext(filename))
  if (!ext %in% c("csv", "tsv", "txt")) {
    stop("Unsupported file type. Please upload a CSV, TSV, or TXT file.", call. = FALSE)
  }

  delim <- if (identical(ext, "csv")) "," else "\t"
  data <- utils::read.table(
    path,
    header = TRUE,
    sep = delim,
    check.names = FALSE,
    stringsAsFactors = FALSE,
    comment.char = "",
    quote = "\""
  )

  if (ncol(data) < 2L) {
    stop("Uploaded table must contain an ID column and at least one data column.", call. = FALSE)
  }

  if (is.data.frame(data) && all(c("from", "to") %in% names(data))) {
    return(data)
  }

  ids <- data[[1]]
  if (anyDuplicated(ids)) {
    stop("The first column contains duplicate IDs. Please make row IDs unique.", call. = FALSE)
  }

  rownames(data) <- ids
  data[[1]] <- NULL
  data
}

detect_upload_type <- function(data) {
  if (!is.matrix(data) && !is.data.frame(data)) {
    return("unknown")
  }

  data_frame <- as.data.frame(data)
  names_lower <- tolower(names(data_frame))
  has_cols <- function(cols) all(cols %in% names_lower)

  if (has_cols(c("source", "target")) || has_cols(c("from", "to"))) {
    return("edge_table")
  }

  if (has_cols(c("node", "module")) || has_cols(c("name", "module"))) {
    return("module_table")
  }

  if (has_cols(c("node", "label")) || has_cols(c("name", "class")) || has_cols(c("id", "group"))) {
    return("annotation")
  }

  numeric_data <- suppressWarnings(data.matrix(data_frame))
  if (anyNA(numeric_data)) {
    return("unknown")
  }

  is_square <- nrow(numeric_data) == ncol(numeric_data)
  has_matching_names <- !is.null(rownames(numeric_data)) &&
    !is.null(colnames(numeric_data)) &&
    identical(rownames(numeric_data), colnames(numeric_data))

  if (is_square && has_matching_names) {
    diagonal <- diag(numeric_data)
    if (all(abs(diagonal - 1) < 1e-8)) {
      return("wgcna_tom")
    }
    return("adjacency")
  }

  if (all(vapply(data_frame, is.numeric, logical(1)))) {
    return("matrix")
  }

  "unknown"
}

validate_matrix_like <- function(data) {
  if (!is.matrix(data) && !is.data.frame(data)) {
    return(app_failure("Input must be a matrix or data frame."))
  }

  converted <- suppressWarnings(as.matrix(data))
  suppressWarnings(storage.mode(converted) <- "double")

  if (anyNA(converted) || any(!is.finite(converted))) {
    return(app_failure("Matrix input must be numeric and contain only finite values."))
  }

  if (is.null(rownames(converted)) || anyDuplicated(rownames(converted))) {
    return(app_failure("Matrix input must have unique row names."))
  }

  app_success(converted)
}
