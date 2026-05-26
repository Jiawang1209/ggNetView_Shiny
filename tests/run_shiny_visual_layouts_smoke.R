# Run with: NOT_CRAN=true /usr/local/bin/Rscript tests/run_shiny_visual_layouts_smoke.R
# or: /usr/local/bin/Rscript --vanilla tests/run_shiny_visual_layouts_smoke.R

Sys.setenv(NOT_CRAN = Sys.getenv("NOT_CRAN", "true"))

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
script_path <- if (length(file_arg)) {
  normalizePath(sub("^--file=", "", file_arg[[1]]), mustWork = TRUE)
} else {
  normalizePath(file.path("tests", "run_shiny_visual_layouts_smoke.R"), mustWork = TRUE)
}
repo_root <- normalizePath(file.path(dirname(script_path), ".."), mustWork = TRUE)
setwd(repo_root)
source(file.path(repo_root, "inst", "app", "modules", "mod_visual_lab.R"))

if (!requireNamespace("shinytest2", quietly = TRUE)) {
  stop("shinytest2 is required for visual layout browser smoke.", call. = FALSE)
}

message("Starting ggNetView Shiny visual layout smoke")

app <- shinytest2::AppDriver$new(
  app_dir = repo_root,
  name = "visual_layouts",
  seed = 1115,
  height = 950,
  width = 1400,
  load_timeout = 60000,
  timeout = 120000
)
on.exit(app$stop(), add = TRUE)

set_input <- function(id, value, wait = TRUE) {
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

select_graph_by_name <- function(name) {
  script <- sprintf(
    paste(
      "(() => {",
      "const el = document.getElementById('visual_lab-graph_id');",
      "if (!el) return null;",
      "const options = el.selectize ? Object.values(el.selectize.options) : Array.from(el.options);",
      "const option = options.find(o => (o.label || o.text || '').startsWith(%s + ' [graph]'));",
      "return option ? option.value : null;",
      "})()"
    ),
    jsonlite::toJSON(name, auto_unbox = TRUE)
  )
  value <- app$get_js(script)
  if (is.null(value) || !nzchar(value)) {
    stop("Could not find Visual Lab graph: ", name, call. = FALSE)
  }
  set_input("visual_lab-graph_id", value, wait = FALSE)
}

wait_for_plot_ready <- function(timeout = 120000) {
  script <- "(() => {
    const img = document.querySelector('#visual_lab-plot img');
    return img && img.complete && img.naturalWidth > 0;
  })();"
  app$wait_for_js(script, timeout = timeout)
}

wait_for_status_change <- function(previous_status, timeout = 120000) {
  if (is.null(previous_status)) {
    previous_status <- ""
  }
  script <- sprintf(
    "(() => {
      const el = document.getElementById('visual_lab-status');
      if (!el) return false;
      const text = el.innerText || el.textContent || '';
      return text.includes('Registered plot:') && text !== %s;
    })();",
    jsonlite::toJSON(previous_status, auto_unbox = TRUE)
  )
  app$wait_for_js(script, timeout = timeout)
}

status_text <- function() {
  app$get_js("(() => {
    const el = document.getElementById('visual_lab-status');
    return el ? (el.innerText || el.textContent || '') : '';
  })();")
}

click_tab("Data Hub")
click("#data_hub-load_gallery")
wait_for_text("gallery_matrix_graph")

click_tab("Visual Lab")

layout_cases <- visual_layout_smoke_cases(visual_lab_layout_choices())

for (i in seq_len(nrow(layout_cases))) {
  layout <- layout_cases$layout[[i]]
  module <- layout_cases$module[[i]]
  graph_name <- layout_cases$graph_name[[i]]
  message(sprintf(
    "Drawing Visual Lab layout %s/%s: %s on %s",
    i,
    nrow(layout_cases),
    layout,
    graph_name
  ))
  previous_status <- status_text()
  select_graph_by_name(graph_name)
  set_input("visual_lab-layout", layout, wait = FALSE)
  set_input("visual_lab-layout_module", module, wait = FALSE)
  app$wait_for_idle(timeout = 30000)
  click("#visual_lab-draw")
  wait_for_status_change(previous_status, timeout = 120000)
  wait_for_text(layout, timeout = 120000)
  wait_for_plot_ready(timeout = 120000)
}

cat("visual layouts browser smoke passed\n")
