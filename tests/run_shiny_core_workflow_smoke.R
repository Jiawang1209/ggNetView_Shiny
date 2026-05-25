# Run with: R_PROFILE_USER=/dev/null Rscript tests/run_shiny_core_workflow_smoke.R
# or: Rscript --vanilla tests/run_shiny_core_workflow_smoke.R

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
script_path <- if (length(file_arg)) {
  normalizePath(sub("^--file=", "", file_arg[[1]]), mustWork = TRUE)
} else {
  normalizePath(file.path("tests", "run_shiny_core_workflow_smoke.R"), mustWork = TRUE)
}
repo_root <- normalizePath(file.path(dirname(script_path), ".."), mustWork = TRUE)
setwd(repo_root)

source_repo_file <- function(...) {
  path <- file.path(repo_root, ...)
  if (!file.exists(path)) {
    stop("Cannot source missing file: ", path, call. = FALSE)
  }
  sys.source(path, envir = globalenv())
}

description_imports <- function() {
  desc <- read.dcf(file.path(repo_root, "DESCRIPTION"))[1, , drop = TRUE]
  import_text <- desc[["Imports"]]
  if (is.null(import_text) || is.na(import_text) || !nzchar(import_text)) {
    return(character())
  }

  imports <- unlist(strsplit(import_text, ",", fixed = TRUE), use.names = FALSE)
  imports <- trimws(gsub("\\s*\\([^)]*\\)", "", imports))
  imports[nzchar(imports)]
}

recommended_or_base_packages <- function() {
  installed <- utils::installed.packages()
  rownames(installed)[installed[, "Priority"] %in% c("base", "recommended")]
}

report_missing_imports <- function() {
  imports <- setdiff(description_imports(), recommended_or_base_packages())
  missing <- imports[!vapply(imports, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing)) {
    message(
      "Missing non-base/non-recommended DESCRIPTION Imports: ",
      paste(missing, collapse = ", ")
    )
  } else {
    message("All non-base/non-recommended DESCRIPTION Imports are installed.")
  }
  invisible(missing)
}

try_load_namespace <- function() {
  if (requireNamespace("pkgload", quietly = TRUE)) {
    return(tryCatch(
      {
        pkgload::load_all(repo_root, quiet = TRUE)
        TRUE
      },
      error = function(e) {
        message("Could not load ggNetView namespace with pkgload: ", conditionMessage(e))
        FALSE
      }
    ))
  }

  message("pkgload is not installed; cannot fully load ggNetView namespace for smoke workflow.")
  FALSE
}

assert_app_ok <- function(result, step) {
  if (isTRUE(result$ok)) {
    return(invisible(result$value))
  }

  details <- c(
    paste0(step, " failed"),
    if (!is.null(result$message)) paste0("message: ", result$message),
    if (!is.null(result$trace)) paste0("trace: ", result$trace)
  )
  stop(paste(details, collapse = "\n"), call. = FALSE)
}

report_missing_imports()
namespace_loaded <- try_load_namespace()
if (!namespace_loaded) {
  message("Continuing through app adapters to expose the first real workflow failure.")
}

source_repo_file("R", "app_validation.R")
source_repo_file("R", "app_adapters.R")
source_repo_file("R", "app_exports.R")
source_repo_file("R", "apply_transform_method.R")
source_repo_file("inst", "app", "modules", "mod_graph_builder.R")

if (requireNamespace("magrittr", quietly = TRUE)) {
  `%>%` <- magrittr::`%>%`
}

matrix_path <- file.path("inst", "extdata", "example_matrix.csv")
mat <- read_user_table(matrix_path)
valid <- validate_matrix_like(mat)
assert_app_ok(valid, "matrix validation")

graph_params <- graph_builder_params(
  builder = "matrix",
  method = "cor",
  cor_method = "pearson",
  proc = "none",
  r_threshold = 0.1,
  p_threshold = 1,
  module_method = "Fast_greedy"
)
graph_result <- safe_build_graph(valid$value, builder = "matrix", params = graph_params)
assert_app_ok(graph_result, "graph build")

topology_result <- safe_topology(graph_result$value)
assert_app_ok(topology_result, "topology calculation")

plot_result <- safe_plot_ggnetview(graph_result$value, params = list(layout = "nicely"))
assert_app_ok(plot_result, "plot generation")

tmpdir <- tempfile("ggnetview-smoke-")
dir.create(tmpdir)
write_registry_object(graph_result$value, file.path(tmpdir, "graph.rds"))
write_registry_params(list(builder = "matrix"), file.path(tmpdir, "params.json"))
stopifnot(file.exists(file.path(tmpdir, "graph.rds")))
stopifnot(file.exists(file.path(tmpdir, "params.json")))

round_trip_graph <- readRDS(file.path(tmpdir, "graph.rds"))
if (requireNamespace("igraph", quietly = TRUE) && inherits(graph_result$value, "igraph")) {
  stopifnot(igraph::vcount(round_trip_graph) == igraph::vcount(graph_result$value))
  stopifnot(igraph::ecount(round_trip_graph) == igraph::ecount(graph_result$value))
} else {
  stopifnot(inherits(round_trip_graph, class(graph_result$value)[1]))
}

params <- jsonlite::read_json(file.path(tmpdir, "params.json"), simplifyVector = TRUE)
stopifnot(identical(params$builder, "matrix"))

cat("core workflow smoke passed\n")
