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

read_user_table <- function(path) {
  ext <- tolower(tools::file_ext(path))
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

  ids <- data[[1]]
  if (anyDuplicated(ids)) {
    stop("The first column contains duplicate IDs. Please make row IDs unique.", call. = FALSE)
  }

  rownames(data) <- ids
  data[[1]] <- NULL
  data
}

detect_upload_type <- function(data) {
  if (is.matrix(data) || is.data.frame(data)) {
    numeric_cols <- vapply(data, function(x) all(!is.na(suppressWarnings(as.numeric(x)))), logical(1))
    if (all(numeric_cols)) {
      return("matrix")
    }
  }

  if (is.data.frame(data) && all(c("from", "to") %in% names(data))) {
    return("edge_table")
  }

  "table"
}

validate_matrix_like <- function(data) {
  if (!is.matrix(data) && !is.data.frame(data)) {
    return(app_failure("Input must be a matrix or data frame."))
  }

  converted <- suppressWarnings(as.matrix(data))
  suppressWarnings(storage.mode(converted) <- "double")

  if (anyNA(converted)) {
    return(app_failure("Matrix input must be numeric and cannot contain non-numeric cells."))
  }

  if (is.null(rownames(converted)) || anyDuplicated(rownames(converted))) {
    return(app_failure("Matrix input must have unique row names."))
  }

  app_success(converted)
}
