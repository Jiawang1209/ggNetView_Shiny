# Run with: NOT_CRAN=true /usr/local/bin/Rscript tests/run_shiny_task_feedback_smoke.R
# or: /usr/local/bin/Rscript --vanilla tests/run_shiny_task_feedback_smoke.R

Sys.setenv(
  NOT_CRAN = Sys.getenv("NOT_CRAN", "true"),
  GGNV_TASK_FEEDBACK_TEST_DELAY = "2"
)

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
script_path <- if (length(file_arg)) {
  normalizePath(sub("^--file=", "", file_arg[[1]]), mustWork = TRUE)
} else {
  normalizePath(file.path("tests", "run_shiny_task_feedback_smoke.R"), mustWork = TRUE)
}
repo_root <- normalizePath(file.path(dirname(script_path), ".."), mustWork = TRUE)
setwd(repo_root)

if (!requireNamespace("shinytest2", quietly = TRUE)) {
  stop("shinytest2 is required for task feedback browser smoke.", call. = FALSE)
}

message("Starting ggNetView Shiny task feedback smoke")

app <- shinytest2::AppDriver$new(
  app_dir = repo_root,
  name = "task_feedback",
  seed = 1115,
  height = 900,
  width = 1400,
  load_timeout = 60000,
  timeout = 120000
)
on.exit(app$stop(), add = TRUE)

wait_for_js <- function(script, timeout = 60000) {
  app$wait_for_js(script, timeout = timeout)
}

button_is_busy <- function(id) {
  sprintf(
    "(() => {
      const el = document.getElementById(%s);
      return !!(el && el.disabled && el.classList.contains('ggnetview-task-busy'));
    })()",
    jsonlite::toJSON(id, auto_unbox = TRUE)
  )
}

button_is_idle <- function(id) {
  sprintf(
    "(() => {
      const el = document.getElementById(%s);
      return !!(el && !el.disabled && !el.classList.contains('ggnetview-task-busy'));
    })()",
    jsonlite::toJSON(id, auto_unbox = TRUE)
  )
}

app$click(selector = "#data_hub-load_gallery")
wait_for_js(button_is_busy("data_hub-load_gallery"), timeout = 1000)
wait_for_js(button_is_idle("data_hub-load_gallery"), timeout = 30000)
app$wait_for_js("document.body && document.body.innerText.includes('gallery_matrix_graph')", timeout = 60000)

cat("task feedback browser smoke passed\n")
