# Run with: NOT_CRAN=true /usr/local/bin/Rscript tests/run_shiny_visual_qa_polish_smoke.R
# or: /usr/local/bin/Rscript --vanilla tests/run_shiny_visual_qa_polish_smoke.R

Sys.setenv(NOT_CRAN = Sys.getenv("NOT_CRAN", "true"))

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
script_path <- if (length(file_arg)) {
  normalizePath(sub("^--file=", "", file_arg[[1]]), mustWork = TRUE)
} else {
  normalizePath(file.path("tests", "run_shiny_visual_qa_polish_smoke.R"), mustWork = TRUE)
}
repo_root <- normalizePath(file.path(dirname(script_path), ".."), mustWork = TRUE)
setwd(repo_root)

if (!requireNamespace("shinytest2", quietly = TRUE)) {
  stop("shinytest2 is required for visual QA polish smoke.", call. = FALSE)
}

message("Starting ggNetView Shiny visual QA polish smoke")

app <- shinytest2::AppDriver$new(
  app_dir = repo_root,
  name = "visual_qa_polish",
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

click <- function(selector, timeout = 60000) {
  script <- sprintf(
    "document.querySelector(%s) !== null",
    jsonlite::toJSON(selector, auto_unbox = TRUE)
  )
  app$wait_for_js(script, timeout = timeout)
  app$click(selector = selector)
  app$wait_for_idle(timeout = 30000)
}

activate_tab <- function(label, timeout = 60000) {
  script <- sprintf(
    "(() => {
      const link = document.querySelector(`a[data-value='%s']`);
      if (!link) return false;
      link.click();
      return true;
    })();",
    label
  )
  app$wait_for_js(script, timeout = timeout)
  app$wait_for_js(sprintf(
    "Array.from(document.querySelectorAll('a.nav-link.active')).some(el => (el.innerText || el.textContent || '').trim() === %s)",
    jsonlite::toJSON(label, auto_unbox = TRUE)
  ), timeout = timeout)
}

assert_no_horizontal_overflow <- function(context) {
  overflow <- app$get_js(
    "(() => {
      const root = document.documentElement;
      const viewport = root.clientWidth;
      const pageOverflow = root.scrollWidth - viewport;
      const offenders = Array.from(document.body.querySelectorAll('*')).filter((el) => {
        const rect = el.getBoundingClientRect();
        const style = window.getComputedStyle(el);
        if (style.display === 'none' || style.visibility === 'hidden') return false;
        if (!el.offsetParent && style.position !== 'fixed') return false;
        if (el.closest('.dataTables_wrapper')) return false;
        if (el.closest('.dataTables_scrollBody')) return false;
        return rect.width > viewport + 3 || rect.right > viewport + 3;
      }).slice(0, 8).map((el) => ({
        tag: el.tagName,
        id: el.id || '',
        className: String(el.className || '').slice(0, 80),
        width: Math.round(el.getBoundingClientRect().width),
        right: Math.round(el.getBoundingClientRect().right)
      }));
      return { viewport, scrollWidth: root.scrollWidth, pageOverflow, offenders };
    })();"
  )
  if (isTRUE(overflow$pageOverflow > 3) || length(overflow$offenders)) {
    stop(
      context,
      " has horizontal overflow: ",
      jsonlite::toJSON(overflow, auto_unbox = TRUE),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

activate_tab("Data Hub")
wait_for_text("Load manual examples")
click("#data_hub-load_gallery")
wait_for_text("Registered manual example workflow objects", timeout = 120000)
wait_for_text("gallery_matrix_graph", timeout = 120000)
assert_no_horizontal_overflow("Data Hub after manual examples")

app$wait_for_js(
  "(() => {
    const preview = document.querySelector('#data_hub-preview .dataTables_wrapper');
    if (!preview) return false;
    return preview.scrollWidth <= preview.clientWidth + 1;
  })();",
  timeout = 60000
)

activate_tab("Visual Lab")
click("#visual_lab-draw")
wait_for_text("Registered plot:", timeout = 120000)

app$wait_for_js(
  "(() => {
    const img = document.querySelector('#visual_lab-plot img');
    if (!img || !img.complete || img.naturalWidth <= 0) return false;
    const box = img.getBoundingClientRect();
    return box.width >= 500 && box.height >= 350;
  })();",
  timeout = 120000
)

app$wait_for_js(
  "(() => {
    const params = document.querySelector('#visual_lab-params');
    const plot = document.querySelector('#visual_lab-plot');
    if (!params || !plot) return false;
    const style = window.getComputedStyle(params);
    if (style.display === 'none' || style.visibility === 'hidden') return true;
    const paramsBox = params.getBoundingClientRect();
    const plotBox = plot.getBoundingClientRect();
    return paramsBox.height <= 1 || paramsBox.top >= plotBox.bottom;
  })();",
  timeout = 60000
)

assert_no_horizontal_overflow("Visual Lab after draw")

activate_tab("Topology")
wait_for_text("Calculate topology")

activate_tab("Export")
wait_for_text("Selected Object Downloads")

cat("visual QA polish browser smoke passed\n")
