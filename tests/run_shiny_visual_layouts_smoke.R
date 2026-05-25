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

wait_for_plot_src_change <- function(previous_src, timeout = 120000) {
  if (is.null(previous_src)) {
    previous_src <- ""
  }
  script <- sprintf(
    "(() => {
      const img = document.querySelector('#visual_lab-plot img');
      return img && img.complete && img.naturalWidth > 0 && img.src !== %s;
    })();",
    jsonlite::toJSON(previous_src, auto_unbox = TRUE)
  )
  app$wait_for_js(script, timeout = timeout)
}

plot_src <- function() {
  app$get_js("(() => {
    const img = document.querySelector('#visual_lab-plot img');
    return img ? img.src : '';
  })();")
}

click("#data_hub-load_gallery")
wait_for_text("gallery_matrix_graph")

click_tab("Visual Lab")

layout_cases <- data.frame(
  layout = c(
    "fr",
    "circle_outline",
    "circular_modules_equal_petal_layout",
    "bipartite_layout",
    "WGCNA"
  ),
  module = c(
    "adjacent",
    "adjacent",
    "order",
    "order",
    "order"
  ),
  stringsAsFactors = FALSE
)

for (i in seq_len(nrow(layout_cases))) {
  layout <- layout_cases$layout[[i]]
  module <- layout_cases$module[[i]]
  message("Drawing Visual Lab layout: ", layout)
  previous <- plot_src()
  set_input("visual_lab-layout", layout, wait = FALSE)
  set_input("visual_lab-layout_module", module, wait = FALSE)
  app$wait_for_idle(timeout = 30000)
  click("#visual_lab-draw")
  wait_for_text(layout, timeout = 120000)
  wait_for_plot_src_change(previous, timeout = 120000)
  wait_for_text("Registered plot:", timeout = 120000)
}

cat("visual layouts browser smoke passed\n")
