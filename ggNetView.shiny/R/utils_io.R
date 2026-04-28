#' Read a user-uploaded data file (CSV / TSV / RDS)
#'
#' Internal helper used by the Shiny modules. Returns a `data.frame`
#' (or whatever object is stored in the RDS).
#'
#' @param path Path to the uploaded file.
#' @param has_header Logical, is the first row a header?
#' @param row_names_col Optional column number to use as row names.
#' @keywords internal
read_user_table <- function(path, has_header = TRUE, row_names_col = NULL) {
  ext <- tolower(tools::file_ext(path))
  out <- switch(
    ext,
    "csv"  = utils::read.csv(path, header = has_header,
                             stringsAsFactors = FALSE,
                             check.names = FALSE),
    "tsv"  = utils::read.delim(path, header = has_header,
                               stringsAsFactors = FALSE,
                               check.names = FALSE),
    "txt"  = utils::read.delim(path, header = has_header,
                               stringsAsFactors = FALSE,
                               check.names = FALSE),
    "rds"  = readRDS(path),
    "rda"  = {
      e <- new.env(); load(path, envir = e)
      get(ls(e)[1], envir = e)
    },
    "rdata" = {
      e <- new.env(); load(path, envir = e)
      get(ls(e)[1], envir = e)
    },
    stop("Unsupported file extension: ", ext, call. = FALSE)
  )
  if (is.data.frame(out) && !is.null(row_names_col) &&
      row_names_col >= 1 && row_names_col <= ncol(out)) {
    rownames(out) <- as.character(out[[row_names_col]])
    out <- out[, -row_names_col, drop = FALSE]
  }
  out
}

#' Safely list available built-in datasets in ggNetView
#'
#' @keywords internal
list_ggNetView_data <- function() {
  if (!requireNamespace("ggNetView", quietly = TRUE)) {
    return(character(0))
  }
  out <- tryCatch(
    utils::data(package = "ggNetView")$results[, "Item"],
    error = function(e) character(0)
  )
  as.character(out)
}

#' Safely fetch a built-in dataset from ggNetView by name
#'
#' @keywords internal
get_ggNetView_data <- function(name) {
  e <- new.env()
  utils::data(list = name, package = "ggNetView", envir = e)
  if (!exists(name, envir = e, inherits = FALSE)) {
    stop("Dataset '", name, "' not found in ggNetView.", call. = FALSE)
  }
  get(name, envir = e, inherits = FALSE)
}

#' Detect whether x is a `tbl_graph` / `igraph` object
#' @keywords internal
is_graph_obj <- function(x) {
  inherits(x, "tbl_graph") || inherits(x, "igraph")
}

#' Pretty-format an object summary for the UI
#' @keywords internal
describe_object <- function(x) {
  if (is.null(x)) return("NULL")
  if (is.data.frame(x)) {
    return(paste0("data.frame  ", nrow(x), " rows x ", ncol(x), " cols"))
  }
  if (is.matrix(x)) {
    return(paste0("matrix  ", nrow(x), " rows x ", ncol(x), " cols"))
  }
  if (is_graph_obj(x)) {
    return(paste0("graph  ",
                  igraph::vcount(x), " nodes / ",
                  igraph::ecount(x), " edges"))
  }
  paste0(class(x)[1], "  length=", length(x))
}
