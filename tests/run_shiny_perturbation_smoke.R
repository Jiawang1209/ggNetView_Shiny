# Run with: NOT_CRAN=true /usr/local/bin/Rscript tests/run_shiny_perturbation_smoke.R
# or: /usr/local/bin/Rscript --vanilla tests/run_shiny_perturbation_smoke.R

Sys.setenv(NOT_CRAN = Sys.getenv("NOT_CRAN", "true"))

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
script_path <- if (length(file_arg)) {
  normalizePath(sub("^--file=", "", file_arg[[1]]), mustWork = TRUE)
} else {
  normalizePath(file.path("tests", "run_shiny_perturbation_smoke.R"), mustWork = TRUE)
}
repo_root <- normalizePath(file.path(dirname(script_path), ".."), mustWork = TRUE)
setwd(repo_root)

if (!requireNamespace("shinytest2", quietly = TRUE)) {
  stop("shinytest2 is required for perturbation browser smoke.", call. = FALSE)
}

message("Starting ggNetView Shiny perturbation smoke")

app <- shinytest2::AppDriver$new(
  app_dir = repo_root,
  name = "perturbation",
  seed = 1115,
  height = 900,
  width = 1400,
  load_timeout = 60000,
  timeout = 120000
)
on.exit(app$stop(), add = TRUE)

set_input <- function(id, value, wait = FALSE) {
  call_args <- c(stats::setNames(list(value), id), list(wait_ = wait))
  do.call(app$set_inputs, call_args)
}

click <- function(selector) {
  app$click(selector = selector)
  app$wait_for_idle(timeout = 30000)
}

click_tab <- function(label) {
  # Dropdown-safe: activate the tab via Bootstrap so panels nested in the
  # "Analysis" nav_menu (hidden until the menu opens) stay reachable.
  script <- sprintf(
    "(() => {
      const label = %s;
      const link = document.querySelector(`a[data-value='${label}']`);
      if (!link) throw new Error(`Cannot find tab: ${label}`);
      if (window.bootstrap && window.bootstrap.Tab) {
        window.bootstrap.Tab.getOrCreateInstance(link).show();
      } else {
        link.click();
      }
      return true;
    })();",
    jsonlite::toJSON(label, auto_unbox = TRUE)
  )
  app$run_js(script)
  app$wait_for_idle(timeout = 30000)
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

# Pick a registry option whose label contains a substring (e.g. "[graph]").
select_option_by_label <- function(input_id, label_substr) {
  script <- sprintf(
    paste(
      "(() => {",
      "const el = document.getElementById(%s);",
      "if (!el) return null;",
      "const options = el.selectize ? Object.values(el.selectize.options) : Array.from(el.options);",
      "const matching = options.filter(o => (o.label || o.text || '').includes(%s));",
      "const option = matching.length ? matching[matching.length - 1] : null;",
      "return option ? option.value : null;",
      "})()"
    ),
    jsonlite::toJSON(input_id, auto_unbox = TRUE),
    jsonlite::toJSON(label_substr, auto_unbox = TRUE)
  )
  value <- app$get_js(script)
  if (is.null(value) || !nzchar(value)) {
    stop("Could not find option containing '", label_substr, "' in ", input_id, call. = FALSE)
  }
  set_input(input_id, value)
  value
}

# Pick the first available option value for a select input.
select_first_option <- function(input_id) {
  script <- sprintf(
    paste(
      "(() => {",
      "const el = document.getElementById(%s);",
      "if (!el) return null;",
      "const options = el.selectize ? Object.values(el.selectize.options) : Array.from(el.options);",
      "const valid = options.map(o => o.value).filter(v => v && v.length);",
      "return valid.length ? valid[0] : null;",
      "})()"
    ),
    jsonlite::toJSON(input_id, auto_unbox = TRUE)
  )
  value <- app$get_js(script)
  if (is.null(value) || !nzchar(value)) {
    stop("No selectable option for ", input_id, call. = FALSE)
  }
  set_input(input_id, value)
  value
}

assert_download_nonempty <- function(output_id) {
  deadline <- Sys.time() + 30
  last_error <- NULL
  while (Sys.time() < deadline) {
    path <- tryCatch(app$get_download(output_id), error = function(e) {
      last_error <<- e
      NULL
    })
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

# ---- Set up a graph object via the gallery ----
click_tab("Data Hub")
click("#data_hub-load_gallery")
wait_for_text("gallery_matrix_graph")

click_tab("Perturbation")
wait_for_element("perturbation-run_attack")

# ---- Structural attack: random ----
select_option_by_label("perturbation-attack_graph_id", "[graph]")
set_input("perturbation-strategy", "random")
set_input("perturbation-fraction_step", 0.2)
set_input("perturbation-bootstrap", 5)
click("#perturbation-run_attack")
wait_for_text("Registered perturbation:")
wait_for_element("perturbation-download_curve")
assert_download_nonempty("perturbation-download_curve")
assert_download_nonempty("perturbation-download_attack_plot")

# ---- Structural attack: targeted by centrality ----
set_input("perturbation-strategy", "targeted")
set_input("perturbation-centrality", "degree")
click("#perturbation-run_attack")
wait_for_text("Registered perturbation:")

# ---- Node influence ----
select_option_by_label("perturbation-influence_graph_id", "[graph]")
app$wait_for_idle(timeout = 30000)
select_first_option("perturbation-influence_source")
click("#perturbation-run_influence")
wait_for_text("Registered node influence:")
wait_for_element("perturbation-download_influence")
assert_download_nonempty("perturbation-download_influence")

# ---- Press perturbation (graph-backed) ----
set_input("perturbation-press_input_type", "graph")
select_option_by_label("perturbation-press_graph_id", "[graph]")
click("#perturbation-run_press")
wait_for_text("Registered press perturbation:")
wait_for_element("perturbation-download_press_matrix")
assert_download_nonempty("perturbation-download_press_matrix")

cat("perturbation browser smoke passed\n")
