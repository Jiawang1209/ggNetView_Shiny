# Run with: NOT_CRAN=true /usr/local/bin/Rscript tests/run_shiny_mobile_layout_smoke.R
# or: /usr/local/bin/Rscript --vanilla tests/run_shiny_mobile_layout_smoke.R

Sys.setenv(NOT_CRAN = Sys.getenv("NOT_CRAN", "true"))

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
script_path <- if (length(file_arg)) {
  normalizePath(sub("^--file=", "", file_arg[[1]]), mustWork = TRUE)
} else {
  normalizePath(file.path("tests", "run_shiny_mobile_layout_smoke.R"), mustWork = TRUE)
}
repo_root <- normalizePath(file.path(dirname(script_path), ".."), mustWork = TRUE)
setwd(repo_root)

if (!requireNamespace("shinytest2", quietly = TRUE)) {
  stop("shinytest2 is required for mobile layout smoke.", call. = FALSE)
}

message("Starting ggNetView Shiny mobile layout smoke")

app <- shinytest2::AppDriver$new(
  app_dir = repo_root,
  name = "mobile_layout",
  seed = 1115,
  height = 860,
  width = 390,
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

wait_for_visible_id <- function(id, timeout = 60000) {
  script <- sprintf(
    "(() => {
      const el = document.getElementById(%s);
      if (!el) return false;
      const rect = el.getBoundingClientRect();
      const style = window.getComputedStyle(el);
      return style.display !== 'none' && style.visibility !== 'hidden' &&
        rect.width > 0 && rect.height > 0;
    })();",
    jsonlite::toJSON(id, auto_unbox = TRUE)
  )
  app$wait_for_js(script, timeout = timeout)
}

activate_tab <- function(label) {
  script <- sprintf(
    "(() => {
      const label = %s;
      const link = document.querySelector(`a[data-value='${label}']`);
      if (!link) throw new Error(`Cannot find tab: ${label}`);
      const toggler = document.querySelector('.navbar-toggler');
      const collapsible = document.querySelector('.navbar-collapse');
      if (toggler && collapsible && !collapsible.classList.contains('show')) toggler.click();
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
        if (el.closest('.ggnv-introduction pre')) return false;
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

wait_for_text("ggNetView")
assert_no_horizontal_overflow("Introduction")

activate_tab("Data Hub")
wait_for_text("Upload")
wait_for_visible_id("data_hub-load_gallery")
assert_no_horizontal_overflow("Data Hub")

app$click(selector = "#data_hub-load_gallery")
app$wait_for_idle(timeout = 30000)
wait_for_text("gallery_matrix_graph", timeout = 120000)
assert_no_horizontal_overflow("Data Hub after gallery load")

tab_checks <- c(
  "Graph Builder" = "graph_builder-build",
  "Graph Explorer" = "graph_explorer-register_info",
  "Visual Lab" = "visual_lab-status",
  "Topology" = "topology_results-calculate",
  "Network Compare" = "network_compare-run_compare",
  "Environment Links" = "environment_links-run_environment",
  "Export" = "export_center-replay_status"
)
for (tab in names(tab_checks)) {
  activate_tab(tab)
  wait_for_visible_id(tab_checks[[tab]])
  assert_no_horizontal_overflow(tab)
}

cat("mobile layout browser smoke passed\n")
