# Run with: NOT_CRAN=true /usr/local/bin/Rscript tests/run_shiny_environment_geometry_smoke.R
# or: /usr/local/bin/Rscript --vanilla tests/run_shiny_environment_geometry_smoke.R

Sys.setenv(NOT_CRAN = Sys.getenv("NOT_CRAN", "true"))

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
script_path <- if (length(file_arg)) {
  normalizePath(sub("^--file=", "", file_arg[[1]]), mustWork = TRUE)
} else {
  normalizePath(file.path("tests", "run_shiny_environment_geometry_smoke.R"), mustWork = TRUE)
}
repo_root <- normalizePath(file.path(dirname(script_path), ".."), mustWork = TRUE)
setwd(repo_root)

if (!requireNamespace("shinytest2", quietly = TRUE)) {
  stop("shinytest2 is required for environment geometry browser smoke.", call. = FALSE)
}

message("Starting ggNetView Shiny environment geometry smoke")

app <- shinytest2::AppDriver$new(
  app_dir = repo_root,
  name = "environment_geometry",
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

select_registry_object_by_name <- function(name) {
  script <- sprintf(
    paste(
      "(() => {",
      "const el = document.getElementById('export_center-object_id');",
      "if (!el) return null;",
      "const options = el.selectize ? Object.values(el.selectize.options) : Array.from(el.options);",
      "const option = options.find(o => (o.label || o.text || '').startsWith(%s));",
      "return option ? option.value : null;",
      "})()"
    ),
    jsonlite::toJSON(name, auto_unbox = TRUE)
  )
  value <- app$get_js(script)
  if (is.null(value) || !nzchar(value)) {
    stop("Could not find registry object: ", name, call. = FALSE)
  }
  set_input("export_center-object_id", value)
}

assert_export_plot_available <- function(plot_name) {
  click_tab("Export")
  select_registry_object_by_name(plot_name)
  wait_for_text(plot_name)
  wait_for_text("PNG")
  wait_for_text("PDF")
  wait_for_element("export_center-download_png")
  wait_for_element("export_center-download_pdf")
  click_tab("Data Hub")
}

recipe_cases <- data.frame(
  recipe = c(
    "environment_heatmap",
    "multi_omics_environment_blocks",
    "environment_collapsed_core",
    "environment_arc_collapsed_core"
  ),
  plot_name = c(
    "gallery_recipe_environment_heatmap",
    "gallery_recipe_multi_omics_environment_heatmap",
    "gallery_recipe_environment_collapsed_core_heatmap",
    "gallery_recipe_environment_arc_collapsed_core_heatmap"
  ),
  stats_name = c(
    "gallery_recipe_environment_stats",
    "gallery_recipe_multi_omics_environment_stats",
    "gallery_recipe_environment_collapsed_core_stats",
    "gallery_recipe_environment_arc_collapsed_core_stats"
  ),
  stringsAsFactors = FALSE
)

click("#data_hub-load_gallery")
wait_for_text("gallery_matrix_graph")
wait_for_text("gallery_matrix_b")
wait_for_text("gallery_sample_metadata")

click_tab("Compare & Environment")
wait_for_element("compare_environment-env_ncol")
wait_for_element("compare_environment-env_heatmap_label_size")
wait_for_element("compare_environment-env_heatmap_sig_size")
wait_for_element("compare_environment-env_heatmap_point_size")
wait_for_element("compare_environment-env_sig_line_width_min")
wait_for_element("compare_environment-env_sig_line_width_max")
wait_for_element("compare_environment-env_sig_line_color_low")
wait_for_element("compare_environment-env_sig_line_color_high")
wait_for_element("compare_environment-env_sig_line_alpha")
set_input("compare_environment-env_orientation", "top_right")
set_input("compare_environment-env_spec_layouts", "circle_outline")
set_input("compare_environment-env_group_layout", "row")
set_input("compare_environment-env_nrow", 1)
set_input("compare_environment-env_ncol", 2)
set_input("compare_environment-env_heatmap_label_size", 4)
set_input("compare_environment-env_heatmap_sig_size", 3)
set_input("compare_environment-env_heatmap_point_size", 4.5)
set_input("compare_environment-env_sig_line_width_min", 0.25)
set_input("compare_environment-env_sig_line_width_max", 1.75)
set_input("compare_environment-env_sig_line_color_low", "#2166ac")
set_input("compare_environment-env_sig_line_color_high", "#b2182b")
set_input("compare_environment-env_sig_line_alpha", 0.8)
click("#compare_environment-run_environment")
wait_for_text("Registered environment link plot:", timeout = 120000)

for (i in seq_len(nrow(recipe_cases))) {
  recipe <- recipe_cases$recipe[[i]]
  plot_name <- recipe_cases$plot_name[[i]]
  stats_name <- recipe_cases$stats_name[[i]]
  message(sprintf(
    "Running environment geometry recipe %s/%s: %s",
    i,
    nrow(recipe_cases),
    recipe
  ))

  click_tab("Data Hub")
  set_input("data_hub-gallery_recipe", recipe)
  click("#data_hub-run_gallery_recipe")
  wait_for_text(plot_name, timeout = 120000)
  wait_for_text(stats_name, timeout = 120000)
  assert_export_plot_available(plot_name)
}

cat("environment geometry browser smoke passed\n")
