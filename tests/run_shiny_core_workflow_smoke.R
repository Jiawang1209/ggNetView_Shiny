Sys.setenv(R_PROFILE_USER = "/dev/null")

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

warn_missing_packages <- function(packages) {
  missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing)) {
    warning(
      "Missing packages needed by the real ggNetView workflow: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  invisible(missing)
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

warn_missing_packages(c(
  "jsonlite",
  "magrittr",
  "tibble",
  "dplyr",
  "tidyr",
  "purrr",
  "igraph",
  "tidygraph",
  "psych",
  "ggplot2",
  "ggrepel",
  "ggforce"
))

source_repo_file("R", "app_validation.R")
source_repo_file("R", "app_adapters.R")
source_repo_file("R", "app_exports.R")
source_repo_file("R", "apply_transform_method.R")
source_repo_file("R", "stat_graph.R")
source_repo_file("R", "ggnetview_palette.R")
source_repo_file("R", "theme_ggnetview.R")
source_repo_file("R", "create_layout_nicely.R")
source_repo_file("R", "build_graph_from_mat.R")
source_repo_file("R", "get_network_topology.R")
source_repo_file("R", "ggnetview.R")

if (requireNamespace("magrittr", quietly = TRUE)) {
  `%>%` <- magrittr::`%>%`
}

if (!requireNamespace("ggNetView", quietly = TRUE) &&
    requireNamespace("pkgload", quietly = TRUE)) {
  tryCatch(
    pkgload::load_all(repo_root, quiet = TRUE),
    error = function(e) warning(
      "Could not load ggNetView namespace with pkgload: ",
      conditionMessage(e),
      call. = FALSE
    )
  )
}

matrix_path <- file.path("inst", "extdata", "example_matrix.csv")
mat <- read_user_table(matrix_path)
valid <- validate_matrix_like(mat)
assert_app_ok(valid, "matrix validation")

graph_params <- list(
  method = "cor",
  cor.method = "pearson",
  proc = "none",
  r.threshold = 0.1,
  p.threshold = 1,
  module.method = "Fast_greedy"
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

cat("core workflow smoke passed\n")
