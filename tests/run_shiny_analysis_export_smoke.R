# Run with: NOT_CRAN=true /usr/local/bin/Rscript tests/run_shiny_analysis_export_smoke.R
# or: /usr/local/bin/Rscript --vanilla tests/run_shiny_analysis_export_smoke.R

Sys.setenv(NOT_CRAN = Sys.getenv("NOT_CRAN", "true"))

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
script_path <- if (length(file_arg)) {
  normalizePath(sub("^--file=", "", file_arg[[1]]), mustWork = TRUE)
} else {
  normalizePath(file.path("tests", "run_shiny_analysis_export_smoke.R"), mustWork = TRUE)
}
repo_root <- normalizePath(file.path(dirname(script_path), ".."), mustWork = TRUE)
setwd(repo_root)

if (!requireNamespace("shinytest2", quietly = TRUE)) {
  stop("shinytest2 is required for analysis/export browser smoke.", call. = FALSE)
}

message("Starting ggNetView Shiny analysis/export smoke")

app <- shinytest2::AppDriver$new(
  app_dir = repo_root,
  name = "analysis_export",
  seed = 1115,
  height = 900,
  width = 1400,
  load_timeout = 60000,
  timeout = 120000
)
on.exit(app$stop(), add = TRUE)

set_input <- function(id, value, wait = FALSE) {
  args <- c(stats::setNames(list(value), id), list(wait_ = wait))
  do.call(app$set_inputs, args)
}

click <- function(selector) {
  app$click(selector = selector)
  app$wait_for_idle(timeout = 30000)
}

click_tab <- function(label) {
  click(sprintf("a[data-value='%s']", label))
}

wait_for_text <- function(text, timeout = 120000) {
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

select_registry_object_by_type <- function(type) {
  script <- sprintf(
    paste(
      "(() => {",
      "const el = document.getElementById('export_center-object_id');",
      "if (!el) return null;",
      "const options = el.selectize ? Object.values(el.selectize.options) : Array.from(el.options);",
      "const matching = options.filter(o => (o.label || o.text || '').includes(%s));",
      "const option = matching.length ? matching[matching.length - 1] : null;",
      "return option ? option.value : null;",
      "})()"
    ),
    jsonlite::toJSON(sprintf("[%s]", type), auto_unbox = TRUE)
  )
  value <- app$get_js(script)
  if (is.null(value) || !nzchar(value)) {
    stop("Could not find registry object of type: ", type, call. = FALSE)
  }
  set_input("export_center-object_id", value)
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
  stop("Expected non-empty download for output: ", output_id, call. = FALSE)
}

click_tab("Data Hub")
click("#data_hub-load_gallery")
wait_for_text("gallery_sample_metadata")
wait_for_text("gallery_matrix_graph")

click_tab("Graph Explorer")
set_input("graph_explorer-sample_ids", "S1")
click("#graph_explorer-register_sample_subgraph")
wait_for_text("Registered sample subgraph")

click_tab("Topology")
set_input("topology_results-graph_id", "obj_0009")
set_input("topology_results-matrix_id", "obj_0001")
set_input("topology_results-topology_parallel_api", TRUE)
set_input("topology_results-topology_parallel", FALSE)
set_input("topology_results-topology_bootstrap", 0)
set_input("topology_results-topology_workers", 1)
click("#topology_results-calculate")
wait_for_text("Registered topology:")
wait_for_element("topology_results-download_topology")
assert_download_nonempty("topology_results-download_topology")
click("#topology_results-calculate_centrality")
wait_for_text("Registered node_centrality")
wait_for_element("topology_results-download_node_metrics")
assert_download_nonempty("topology_results-download_node_metrics")
click("#topology_results-calculate_sample_topology")
wait_for_text("Registered sample topology:", timeout = 120000)
wait_for_element("topology_results-download_sample_topology")
assert_download_nonempty("topology_results-download_sample_topology")
click("#topology_results-calculate_ivi")
wait_for_text("Registered node_ivi")

click_tab("Compare & Environment")
click("#compare_environment-run_environment")
wait_for_text("Registered environment link plot:", timeout = 120000)
wait_for_text("strongest_link", timeout = 60000)
set_input("compare_environment-mantel_kind", "block_vs_col")
set_input("compare_environment-mantel_method", "spearman")
set_input("compare_environment-mantel_alternative", "greater")
set_input("compare_environment-spec_dist_method", "bray")
set_input("compare_environment-env_dist_method", "euclidean")
set_input("compare_environment-mantel_permutations", 9)
click("#compare_environment-run_mantel")
wait_for_text("Registered Mantel result")

click_tab("Visual Lab")
wait_for_element("visual_lab-plot_width")
wait_for_element("visual_lab-plot_height")
set_input("visual_lab-plot_width", 7)
set_input("visual_lab-plot_height", 5)
click("#visual_lab-draw")
wait_for_text("Registered plot:")
wait_for_element("visual_lab-download_png")
wait_for_element("visual_lab-download_pdf")
assert_download_nonempty("visual_lab-download_png")
assert_download_nonempty("visual_lab-download_pdf")

click_tab("Export")
select_registry_object_by_type("plot")
wait_for_element("export_center-download_png")
wait_for_element("export_center-download_pdf")
assert_download_nonempty("export_center-download_png")
assert_download_nonempty("export_center-download_pdf")

cat("analysis/export browser smoke passed\n")
