# Run with: NOT_CRAN=true /usr/local/bin/Rscript tests/run_shiny_phase2_workflow_smoke.R
# or: /usr/local/bin/Rscript --vanilla tests/run_shiny_phase2_workflow_smoke.R

Sys.setenv(NOT_CRAN = Sys.getenv("NOT_CRAN", "true"))

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
script_path <- if (length(file_arg)) {
  normalizePath(sub("^--file=", "", file_arg[[1]]), mustWork = TRUE)
} else {
  normalizePath(file.path("tests", "run_shiny_phase2_workflow_smoke.R"), mustWork = TRUE)
}
repo_root <- normalizePath(file.path(dirname(script_path), ".."), mustWork = TRUE)
setwd(repo_root)

if (!requireNamespace("shinytest2", quietly = TRUE)) {
  stop("shinytest2 is required for browser workflow smoke.", call. = FALSE)
}

message("Starting ggNetView Shiny browser workflow smoke")

app <- shinytest2::AppDriver$new(
  app_dir = repo_root,
  name = "phase2_workflow",
  seed = 1115,
  height = 900,
  width = 1400,
  load_timeout = 60000,
  timeout = 120000
)
on.exit(app$stop(), add = TRUE)

set_input <- function(id, value) {
  args <- c(stats::setNames(list(value), id), list(wait_ = TRUE))
  do.call(app$set_inputs, args)
}

click <- function(selector) {
  app$click(selector = selector)
  app$wait_for_idle(timeout = 30000)
}

click_tab <- function(label) {
  click(sprintf("a[data-value='%s']", label))
}

wait_for_text <- function(text, timeout = 60000) {
  script <- sprintf(
    "document.body && document.body.innerText.includes(%s)",
    jsonlite::toJSON(text, auto_unbox = TRUE)
  )
  app$wait_for_js(script, timeout = timeout)
}

wait_for_element <- function(id, timeout = 60000) {
  script <- sprintf(
    "document.getElementById(%s) !== null",
    jsonlite::toJSON(id, auto_unbox = TRUE)
  )
  app$wait_for_js(script, timeout = timeout)
}

assert_download_nonempty <- function(output_id) {
  path <- NULL
  deadline <- Sys.time() + 30
  last_error <- NULL
  while (Sys.time() < deadline) {
    path <- tryCatch(
      app$get_download(output_id),
      error = function(e) {
        last_error <<- e
        NULL
      }
    )
    if (!is.null(path) && file.exists(path) && file.info(path)$size > 0) {
      return(invisible(path))
    }
    Sys.sleep(1)
  }
  if (!is.null(last_error)) {
    stop(conditionMessage(last_error), call. = FALSE)
  }
  if (!file.exists(path) || file.info(path)$size <= 0) {
    stop("Expected non-empty download for output: ", output_id, call. = FALSE)
  }
  invisible(path)
}

click("#data_hub-load_gallery")
wait_for_text("gallery_matrix_graph")
wait_for_text("gallery_edge_module_graph")

click_tab("Compare & Environment")
set_input("compare_environment-compare_graph_ids", c("obj_0008", "obj_0009"))
click("#compare_environment-run_compare")
wait_for_text("Registered comparison plot:", timeout = 120000)
click("#compare_environment-run_multi_group")
wait_for_text("Registered grouped network plot:", timeout = 120000)
click("#compare_environment-run_environment")
wait_for_text("Registered environment link plot:", timeout = 120000)
click("#compare_environment-run_environment_manual")
wait_for_text("Registered manual environment heatmap:", timeout = 120000)

set_input("graph_builder-graph_name", "browser_matrix_graph")
click_tab("Graph Builder")
click("#graph_builder-build")
wait_for_text("Built graph: browser_matrix_graph")

click_tab("Graph Explorer")
click("#graph_explorer-register_info")
wait_for_text("Registered graph info")
click("#graph_explorer-register_module_subgraph")
wait_for_text("Registered module subgraph")

click_tab("Visual Lab")
click("#visual_lab-draw")
wait_for_text("Registered plot:")

click_tab("Topology")
click("#topology_results-calculate")
wait_for_text("Registered topology:")
click("#topology_results-calculate_zipi")
wait_for_text("Registered zipi")

click_tab("Export")
set_input("export_center-object_id", "obj_0008")
wait_for_element("export_center-download_nodes_csv")
wait_for_text("Download Nodes CSV")
app$wait_for_idle(timeout = 30000)
assert_download_nonempty("export_center-download_nodes_csv")

cat("phase2 browser workflow smoke passed\n")
