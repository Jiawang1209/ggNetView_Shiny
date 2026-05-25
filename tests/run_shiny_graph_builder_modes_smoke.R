# Run with: NOT_CRAN=true /usr/local/bin/Rscript tests/run_shiny_graph_builder_modes_smoke.R
# or: /usr/local/bin/Rscript --vanilla tests/run_shiny_graph_builder_modes_smoke.R

Sys.setenv(NOT_CRAN = Sys.getenv("NOT_CRAN", "true"))

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
script_path <- if (length(file_arg)) {
  normalizePath(sub("^--file=", "", file_arg[[1]]), mustWork = TRUE)
} else {
  normalizePath(file.path("tests", "run_shiny_graph_builder_modes_smoke.R"), mustWork = TRUE)
}
repo_root <- normalizePath(file.path(dirname(script_path), ".."), mustWork = TRUE)
setwd(repo_root)

if (!requireNamespace("shinytest2", quietly = TRUE)) {
  stop("shinytest2 is required for graph builder browser smoke.", call. = FALSE)
}

message("Starting ggNetView Shiny graph builder mode smoke")

app <- shinytest2::AppDriver$new(
  app_dir = repo_root,
  name = "graph_builder_modes",
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

wait_for_text <- function(text, timeout = 120000) {
  script <- sprintf(
    "document.body && document.body.innerText.includes(%s)",
    jsonlite::toJSON(text, auto_unbox = TRUE)
  )
  app$wait_for_js(script, timeout = timeout)
}

build_graph <- function(name, source, builder, source_b = NULL, multi = NULL, consensus = NULL, module = NULL, node = NULL) {
  message("Building graph via browser: ", name)
  set_input("graph_builder-source_id", source)
  Sys.sleep(0.5)
  if (!is.null(source_b)) {
    set_input("graph_builder-source_id_b", source_b)
  }
  if (!is.null(multi)) {
    set_input("graph_builder-multi_source_ids", multi)
  }
  if (!is.null(consensus)) {
    set_input("graph_builder-consensus_source_ids", consensus)
  }
  if (!is.null(module)) {
    set_input("graph_builder-module_id", module)
  }
  if (!is.null(node)) {
    set_input("graph_builder-node_id", node)
  }
  Sys.sleep(0.5)
  set_input("graph_builder-builder", builder)
  Sys.sleep(0.5)
  set_input("graph_builder-graph_name", name)
  click("#graph_builder-build")
  wait_for_text(paste("Built graph:", name))
}

click("#data_hub-load_gallery")
wait_for_text("gallery_sample_metadata")
wait_for_text("gallery_matrix_graph")
click("a[data-value='Graph Builder']")

set_input("graph_builder-source_id", "obj_0011")
Sys.sleep(0.5)
click("#graph_builder-run_rmt")
wait_for_text("Registered RMT result:")

build_graph("browser_matrix", "obj_0001", "matrix")
build_graph("browser_edge_module", "obj_0003", "edge_table", module = "obj_0004")
build_graph("browser_node_edge", "obj_0003", "node_edge", module = "", node = "obj_0012")
build_graph("browser_adjacency_module", "obj_0006", "adjacency", module = "obj_0004")
build_graph("browser_double", "obj_0001", "double_matrix", source_b = "obj_0002", module = "")
build_graph("browser_multi", "obj_0001", "multi_matrix", multi = "obj_0002", module = "")
build_graph("browser_wgcna", "obj_0007", "wgcna_tom", module = "obj_0004")
build_graph("browser_igraph", "obj_0009", "igraph", module = "")
build_graph("browser_stringdb", "obj_0017", "stringdb", module = "")
build_graph("browser_consensus", "obj_0006", "consensus", consensus = "obj_0009", module = "")

cat("graph builder modes browser smoke passed\n")
