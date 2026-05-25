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

upload_file <- function(id, path) {
  args <- c(stats::setNames(list(path), id), list(wait_ = TRUE))
  do.call(app$upload_file, args)
  app$wait_for_idle(timeout = 30000)
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
wait_for_text("gallery_sample_metadata")
wait_for_text("gallery_matrix_graph")
wait_for_text("gallery_edge_module_graph")

click_tab("Compare & Environment")
set_input("compare_environment-compare_graph_ids", c("obj_0009", "obj_0010"))
wait_for_element("compare_environment-comparison_pairs")
wait_for_element("compare_environment-env_blocks")
wait_for_element("compare_environment-spec_blocks")
wait_for_element("compare_environment-env_spec_pairs")
wait_for_element("compare_environment-compare_topology")
click("#compare_environment-run_compare")
wait_for_text("Registered comparison plot:", timeout = 120000)
wait_for_text("Topology Comparison", timeout = 120000)
set_input("compare_environment-multi_group_id", "obj_0005")
click("#compare_environment-run_multi_group")
wait_for_text("Registered grouped network plot:", timeout = 120000)
click("#compare_environment-run_environment")
wait_for_text("Registered environment link plot:", timeout = 120000)
click("#compare_environment-run_environment_manual")
wait_for_text("Registered manual environment heatmap:", timeout = 120000)
click("#compare_environment-run_environment_triple")
wait_for_text("Registered triple environment heatmap:", timeout = 120000)

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
set_input("visual_lab-layout", "circular_modules_equal_petal_layout")
set_input("visual_lab-layout_module", "order")
click("#visual_lab-draw")
wait_for_text("Registered plot:")

click_tab("Topology")
click("#topology_results-calculate")
wait_for_text("Registered topology:")
click("#topology_results-calculate_zipi")
wait_for_text("Registered zipi")

click_tab("Export")
set_input("export_center-object_id", "obj_0009")
wait_for_element("export_center-download_nodes_csv")
wait_for_text("Formats")
wait_for_text("Source")
wait_for_text("Download Nodes CSV")
app$wait_for_idle(timeout = 30000)
assert_download_nonempty("export_center-download_nodes_csv")
wait_for_element("export_center-download_workflow_manifest")
assert_download_nonempty("export_center-download_workflow_manifest")

click_tab("Data Hub")
click("#data_hub-run_gallery_recipe")
wait_for_text("gallery_recipe_circle_plot", timeout = 120000)
set_input("data_hub-gallery_recipe", "grouped_network_plot")
click("#data_hub-run_gallery_recipe")
wait_for_text("gallery_recipe_grouped_network_plot", timeout = 120000)
set_input("data_hub-gallery_recipe", "graph_info_topology")
click("#data_hub-run_gallery_recipe")
wait_for_text("gallery_recipe_network_topology", timeout = 120000)
set_input("data_hub-gallery_recipe", "environment_heatmap")
click("#data_hub-run_gallery_recipe")
wait_for_text("gallery_recipe_environment_heatmap", timeout = 120000)
set_input("data_hub-gallery_recipe", "mantel_pairwise")
click("#data_hub-run_gallery_recipe")
wait_for_text("gallery_recipe_mantel_pairwise", timeout = 120000)
set_input("data_hub-gallery_recipe", "multi_network_compare")
click("#data_hub-run_gallery_recipe")
wait_for_text("gallery_recipe_multi_network_compare", timeout = 120000)
set_input("data_hub-gallery_recipe", "triple_environment_heatmap")
click("#data_hub-run_gallery_recipe")
wait_for_text("gallery_recipe_triple_environment_heatmap", timeout = 120000)
set_input("data_hub-gallery_recipe", "multi_omics_network")
click("#data_hub-run_gallery_recipe")
wait_for_text("gallery_recipe_multi_omics_graph", timeout = 120000)
set_input("data_hub-gallery_recipe", "multi_omics_double_matrix")
click("#data_hub-run_gallery_recipe")
wait_for_text("gallery_recipe_multi_omics_double_graph", timeout = 120000)
set_input("data_hub-gallery_recipe", "multi_omics_environment_blocks")
click("#data_hub-run_gallery_recipe")
wait_for_text("gallery_recipe_multi_omics_environment_heatmap", timeout = 120000)

click_tab("Export")
replay_manifest <- assert_download_nonempty("export_center-download_workflow_manifest")
upload_file("export_center-workflow_manifest", replay_manifest)
wait_for_text("recipe-output-needs-rerun", timeout = 120000)

cat("phase2 browser workflow smoke passed\n")
