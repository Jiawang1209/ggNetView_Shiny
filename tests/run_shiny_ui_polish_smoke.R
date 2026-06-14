# Run with: NOT_CRAN=true /usr/local/bin/Rscript tests/run_shiny_ui_polish_smoke.R
# or: /usr/local/bin/Rscript --vanilla tests/run_shiny_ui_polish_smoke.R
#
# Smoke test: UI polish — landing CTA "Load example data" click, navigate to
# Graph Builder, then Graph Explorer, assert value-box elements are present.

Sys.setenv(NOT_CRAN = Sys.getenv("NOT_CRAN", "true"))

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
script_path <- if (length(file_arg)) {
  normalizePath(sub("^--file=", "", file_arg[[1]]), mustWork = TRUE)
} else {
  normalizePath(file.path("tests", "run_shiny_ui_polish_smoke.R"), mustWork = TRUE)
}
repo_root <- normalizePath(file.path(dirname(script_path), ".."), mustWork = TRUE)
setwd(repo_root)

# Gracefully skip when shinytest2 / chromote / Chrome is not available.
if (!requireNamespace("shinytest2", quietly = TRUE)) {
  cat("SKIP: shinytest2 not available — ui-polish smoke skipped\n")
  quit(save = "no", status = 0)
}
if (!requireNamespace("chromote", quietly = TRUE)) {
  cat("SKIP: chromote not available — ui-polish smoke skipped\n")
  quit(save = "no", status = 0)
}
chrome_ok <- tryCatch({
  b <- chromote::ChromoteSession$new()
  b$parent$close()
  TRUE
}, error = function(e) FALSE)
if (!chrome_ok) {
  cat("SKIP: Chrome/Chromium not available — ui-polish smoke skipped\n")
  quit(save = "no", status = 0)
}

message("Starting ggNetView Shiny ui-polish smoke")

app <- shinytest2::AppDriver$new(
  app_dir = repo_root,
  name = "ui_polish",
  seed = 1115,
  height = 900,
  width = 1440,
  load_timeout = 60000,
  timeout = 120000
)
on.exit(app$stop(), add = TRUE)

wait_for_text <- function(text, timeout = 60000) {
  script <- sprintf(
    "document.body && document.body.innerText.includes(%s)",
    jsonlite::toJSON(text, auto_unbox = TRUE)
  )
  app$wait_for_js(script, timeout = timeout)
}

click_tab <- function(label) {
  # Dropdown-safe tab activation via Bootstrap API.
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

# ── Step 1: Landing page loads ────────────────────────────────────────────────
wait_for_text("ggNetView")

# ── Step 2: Click the landing CTA — "Load example data" button ───────────────
# The namespaced button id is landing-start_example (module "landing", input "start_example").
# This loads the gallery example data into the session registry.
app$click(selector = "#landing-start_example")
app$wait_for_idle(timeout = 60000)

# ── Step 3: Navigate to Graph Builder ────────────────────────────────────────
click_tab("Graph Builder")
wait_for_text("Graph Builder")

# ── Step 4: Navigate to Graph Explorer and assert value-box cards ─────────────
click_tab("Graph Explorer")
app$wait_for_idle(timeout = 30000)

# bslib value_box renders elements with class containing "value-box".
# Assert that at least one value-box element is present in the DOM.
has_value_box <- app$get_js(
  "(() => {
    const els = document.querySelectorAll('.value-box, [class*=\"value-box\"]');
    return els.length > 0;
  })();"
)

if (!isTRUE(has_value_box)) {
  # Softer fallback: accept if the page rendered without startup error
  # (value-boxes only populate after a graph is selected).
  page_html <- app$get_html("body")
  if (!grepl("value-box", page_html, fixed = TRUE)) {
    # Check that the explorer UI at minimum loaded (no error state).
    app$wait_for_js(
      "document.getElementById('graph_explorer-register_info') !== null",
      timeout = 30000
    )
    message("Note: value-box elements not yet visible (no graph selected) — explorer UI present, no startup error.")
  }
} else {
  message("value-box elements confirmed present on Graph Explorer.")
}

# Final assertion: get body HTML and confirm "value-box" string present
# (either in rendered cards or in the page source as bslib markup).
page_html <- app$get_html("body")
if (!grepl("value-box", page_html, fixed = TRUE)) {
  stop("Expected 'value-box' in Graph Explorer HTML but it was not found.", call. = FALSE)
}

cat("ui-polish smoke OK\n")
