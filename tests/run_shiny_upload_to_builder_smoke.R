# Run with: /usr/local/bin/Rscript tests/run_shiny_upload_to_builder_smoke.R

Sys.setenv(NOT_CRAN = Sys.getenv("NOT_CRAN", "true"))

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
script_path <- if (length(file_arg)) {
  normalizePath(sub("^--file=", "", file_arg[[1]]), mustWork = TRUE)
} else {
  normalizePath(file.path("tests", "run_shiny_upload_to_builder_smoke.R"), mustWork = TRUE)
}
repo_root <- normalizePath(file.path(dirname(script_path), ".."), mustWork = TRUE)
setwd(repo_root)

if (!requireNamespace("shinytest2", quietly = TRUE)) {
  stop("shinytest2 is required for upload-to-builder smoke.", call. = FALSE)
}

message("Starting ggNetView Shiny upload-to-builder smoke")

app <- shinytest2::AppDriver$new(
  app_dir = repo_root,
  name = "upload_to_builder",
  seed = 1115,
  height = 900,
  width = 1400,
  load_timeout = 60000,
  timeout = 120000
)
on.exit(app$stop(), add = TRUE)

upload_file <- function(id, path) {
  args <- c(stats::setNames(list(path), id), list(wait_ = TRUE))
  do.call(app$upload_file, args)
  app$wait_for_idle(timeout = 30000)
}

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

wait_for_builder_source <- function(timeout = 60000) {
  script <- paste(
    "(() => {",
    "const source = document.getElementById('graph_builder-source_id');",
    "const build = document.getElementById('graph_builder-build');",
    "if (!source || !build) return false;",
    "return source.options.length > 0 && source.value !== '';",
    "})()"
  )
  app$wait_for_js(script, timeout = timeout)
}

upload_file("data_hub-file", file.path(repo_root, "inst", "extdata", "example_matrix.csv"))
wait_for_text("Registered uploaded_matrix")

click_tab("Graph Builder")
wait_for_builder_source()
wait_for_text("Build graph")
set_input("graph_builder-module_method", "Walktrap")
set_input("graph_builder-r_threshold", 0.2)
click("#graph_builder-build")
wait_for_text("Built graph: network_graph", timeout = 120000)
wait_for_text('"module.method": "Walktrap"', timeout = 120000)
wait_for_text('"r.threshold": 0.2', timeout = 120000)

message("upload-to-builder smoke passed")
