# ggNetView Shiny Visual QA Polish Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Turn the completed ggNetView Shiny into a cleaner human-facing experience after the first manual visual QA pass.

**Architecture:** Keep the current workflow-based information architecture. Make targeted UI/UX fixes in the relevant Shiny modules, add browser smokes that reproduce the observed visual issues, and update the release evidence report after verification. Avoid broad redesign or new manual API scope in this pass.

**Tech Stack:** R, Shiny, bslib, shinytest2, testthat, ggplot2, ggNetView, `/usr/local/bin/Rscript`.

---

## Observed Baseline

- App was visually inspected at `http://127.0.0.1:7624/`.
- Data Hub loads and manual examples register objects successfully.
- Graph Builder builds a graph from `gallery_matrix` and shows status/toast feedback.
- Page-level horizontal overflow was not observed at desktop width.
- DT tables show internal horizontal scrollbars that are functional but visually heavy.
- Visual Lab can register a plot, but the preview is visually weak: the plot is small and the params JSON dominates the panel.
- During Visual Lab manual use, R emitted `graphics::plot.new: figure margins too large`, so preview rendering should be made more robust.
- After Visual Lab sidebar interaction, top navigation became hard to switch by click in the browser session. This needs a focused regression test before changing layout behavior.
- Toast notifications can overlap the lower-right content area.

## File Map

- Modify: `inst/app/modules/mod_visual_lab.R`
  - Improve Visual Lab preview hierarchy, plot sizing, parameter display, and sidebar defaults.
- Modify: `inst/app/modules/mod_data_hub.R` or the shared DT helper file if Data Hub tables use one.
  - Reduce unnecessary internal table scrollbars and improve column readability.
- Modify: shared UI/CSS file, likely `inst/app/www/app.css` if present, or create it and source it from `app.R`.
  - Add scoped polish for plot preview, notification placement, and table containers.
- Create: `tests/run_shiny_visual_qa_polish_smoke.R`
  - Browser smoke for the visual QA issues: navigation after Visual Lab sidebar use, plot image size, no page overflow, and export tab still reachable.
- Modify: `docs/ggnetview-shiny-release-evidence.md`
  - Regenerate after the new smoke passes.
- Modify: `docs/ggnetview-shiny-next-todos.md`
  - Move fixed visual QA items out of remaining polish notes.

---

### Task 1: Add Visual QA Browser Regression Smoke

**Files:**
- Create: `tests/run_shiny_visual_qa_polish_smoke.R`

- [ ] **Step 1: Write the failing browser smoke**

Create `tests/run_shiny_visual_qa_polish_smoke.R` with this structure:

```r
#!/usr/bin/env Rscript

repo_root <- normalizePath(file.path(dirname(sys.frame(1)$ofile %||% "tests"), ".."), mustWork = FALSE)
if (!dir.exists(file.path(repo_root, "inst"))) {
  repo_root <- normalizePath(getwd(), mustWork = TRUE)
}

source(file.path(repo_root, "tests", "shiny_smoke_helpers.R"))

app <- start_shiny_app(repo_root, width = 1440, height = 900)
on.exit(app$stop(), add = TRUE)

click <- function(selector, timeout = 60000) {
  app$wait_for_js(sprintf("document.querySelector(%s) !== null", jsonlite::toJSON(selector, auto_unbox = TRUE)), timeout = timeout)
  app$run_js(sprintf("document.querySelector(%s).click()", jsonlite::toJSON(selector, auto_unbox = TRUE)))
}

click_tab <- function(label, timeout = 60000) {
  script <- sprintf(
    "(() => {
      const links = Array.from(document.querySelectorAll('a.nav-link'));
      const link = links.find(el => (el.innerText || el.textContent || '').trim() === %s);
      if (!link) return false;
      link.click();
      return true;
    })();",
    jsonlite::toJSON(label, auto_unbox = TRUE)
  )
  app$wait_for_js(script, timeout = timeout)
  app$wait_for_js(sprintf(
    "Array.from(document.querySelectorAll('a.nav-link.active')).some(el => (el.innerText || el.textContent || '').trim() === %s)",
    jsonlite::toJSON(label, auto_unbox = TRUE)
  ), timeout = timeout)
}

wait_for_text <- function(text, timeout = 120000) {
  app$wait_for_js(sprintf("document.body.innerText.includes(%s)", jsonlite::toJSON(text, auto_unbox = TRUE)), timeout = timeout)
}

page_has_no_horizontal_overflow <- function() {
  app$run_js("document.documentElement.scrollWidth <= document.documentElement.clientWidth + 1")
}

app$wait_for_js("document.body.innerText.includes('Load manual examples')", timeout = 60000)
click("#data_hub-load_manual_examples")
wait_for_text("Registered manual example workflow objects")

click_tab("Visual Lab")
click("#visual_lab-draw")
wait_for_text("Registered plot:")

app$wait_for_js("(() => {
  const img = document.querySelector('#visual_lab-plot img');
  if (!img || !img.complete || img.naturalWidth <= 0) return false;
  const box = img.getBoundingClientRect();
  return box.width >= 500 && box.height >= 350;
})()", timeout = 120000)

app$wait_for_js("(() => {
  const params = document.querySelector('#visual_lab-params');
  if (!params) return false;
  const box = params.getBoundingClientRect();
  const plot = document.querySelector('#visual_lab-plot').getBoundingClientRect();
  return box.top > plot.top;
})()", timeout = 60000)

if (!isTRUE(page_has_no_horizontal_overflow())) {
  stop("Page has horizontal overflow after Visual Lab draw.", call. = FALSE)
}

click_tab("Topology")
wait_for_text("Calculate topology")

click_tab("Export")
wait_for_text("Selected object")

cat("visual QA polish browser smoke passed\n")
```

- [ ] **Step 2: Run the smoke to verify it fails on current UX**

Run:

```bash
/usr/local/bin/Rscript tests/run_shiny_visual_qa_polish_smoke.R
```

Expected before fixes: FAIL either because the plot image rendered too small, params dominate the preview area, or tab switching after Visual Lab interaction is unreliable.

- [ ] **Step 3: Commit only the red test if desired**

```bash
git add tests/run_shiny_visual_qa_polish_smoke.R
git commit -m "test: add visual qa polish smoke"
```

---

### Task 2: Make Visual Lab Preview Plot-First

**Files:**
- Modify: `inst/app/modules/mod_visual_lab.R`
- Test: `tests/run_shiny_visual_qa_polish_smoke.R`
- Test: `tests/run_shiny_visual_layouts_smoke.R`

- [ ] **Step 1: Move params into a collapsed details panel**

In `mod_visual_lab_ui()`, replace the preview card body:

```r
bslib::card(
  bslib::card_header("Preview"),
  shiny::plotOutput(ns("plot"), height = 650),
  shiny::verbatimTextOutput(ns("status")),
  shiny::verbatimTextOutput(ns("params"))
)
```

with:

```r
bslib::card(
  class = "visual-lab-preview-card",
  bslib::card_header("Preview"),
  shiny::div(
    class = "visual-lab-plot-frame",
    shiny::plotOutput(ns("plot"), height = "620px")
  ),
  shiny::div(
    class = "visual-lab-status",
    shiny::verbatimTextOutput(ns("status"))
  ),
  bslib::accordion(
    open = FALSE,
    bslib::accordion_panel(
      "Plot parameters",
      shiny::verbatimTextOutput(ns("params"))
    )
  )
)
```

- [ ] **Step 2: Make `renderPlot()` use a stable device size**

Replace:

```r
output$plot <- shiny::renderPlot({
  shiny::req(plot_obj())
  plot_obj()
})
```

with:

```r
output$plot <- shiny::renderPlot(
  {
    shiny::req(plot_obj())
    plot_obj()
  },
  width = function() {
    width <- session$clientData[[paste0("output_", session$ns("plot"), "_width")]]
    if (is.null(width) || is.na(width) || width < 700) 900 else width
  },
  height = function() 620,
  res = 96
)
```

- [ ] **Step 3: Run focused Visual Lab smokes**

Run:

```bash
/usr/local/bin/Rscript tests/run_shiny_visual_qa_polish_smoke.R
/usr/local/bin/Rscript tests/run_shiny_visual_layouts_smoke.R
```

Expected: both pass; no visible `figure margins too large` in the app terminal during the focused Visual Lab path.

- [ ] **Step 4: Commit**

```bash
git add inst/app/modules/mod_visual_lab.R tests/run_shiny_visual_qa_polish_smoke.R
git commit -m "fix: stabilize visual lab preview"
```

---

### Task 3: Fix Visual Lab Sidebar and Top Navigation Interaction

**Files:**
- Modify: `inst/app/modules/mod_visual_lab.R`
- Modify or create: `inst/app/www/app.css`
- Modify: `app.R` if a new CSS file must be included.
- Test: `tests/run_shiny_visual_qa_polish_smoke.R`

- [ ] **Step 1: Keep Visual Lab controls visible by default**

In `mod_visual_lab_ui()`, update the sidebar declaration from:

```r
sidebar = bslib::sidebar(
```

to:

```r
sidebar = bslib::sidebar(
  open = TRUE,
```

- [ ] **Step 2: Add scoped CSS for the Visual Lab sidebar and preview**

If `inst/app/www/app.css` exists, append this. If it does not exist, create it and ensure `app.R` includes it with `shiny::includeCSS("inst/app/www/app.css")` or the existing app resource pattern.

```css
.visual-lab-preview-card .card-body {
  min-height: 720px;
}

.visual-lab-plot-frame {
  min-height: 620px;
}

.visual-lab-status pre {
  white-space: pre-wrap;
  margin-bottom: 0.75rem;
}

.bslib-sidebar-layout > .sidebar {
  z-index: 1;
}

.navbar {
  z-index: 20;
}
```

- [ ] **Step 3: Run navigation regression smoke**

Run:

```bash
/usr/local/bin/Rscript tests/run_shiny_visual_qa_polish_smoke.R
```

Expected: PASS, including `Visual Lab -> Topology -> Export` navigation after drawing.

- [ ] **Step 4: Commit**

```bash
git add inst/app/modules/mod_visual_lab.R inst/app/www/app.css app.R tests/run_shiny_visual_qa_polish_smoke.R
git commit -m "fix: improve visual lab navigation layout"
```

---

### Task 4: Polish Data Hub and Object Table Readability

**Files:**
- Inspect then modify: `inst/app/modules/mod_data_hub.R`
- Inspect then modify shared DT helpers if tables are centralized.
- Test: `tests/run_shiny_phase2_workflow_smoke.R`
- Test: `tests/run_shiny_mobile_layout_smoke.R`

- [ ] **Step 1: Find the DT render points**

Run:

```bash
rg -n "DT::datatable|renderDT|datatable\\(" inst/app R app.R
```

Expected: identify the Preview and Objects table renderers.

- [ ] **Step 2: Keep horizontal scroll only where necessary**

For small preview matrices, use `scrollX = FALSE` and compact display options:

```r
DT::datatable(
  value,
  rownames = FALSE,
  options = list(
    pageLength = 10,
    autoWidth = TRUE,
    dom = "tip"
  )
)
```

For object registry tables where long names are expected, keep `scrollX = TRUE` but add compact columns:

```r
DT::datatable(
  value,
  rownames = FALSE,
  options = list(
    pageLength = 10,
    scrollX = TRUE,
    autoWidth = TRUE,
    columnDefs = list(
      list(width = "24%", targets = c(0, 1)),
      list(width = "18%", targets = c(2))
    )
  )
)
```

- [ ] **Step 3: Run Data Hub and mobile smokes**

Run:

```bash
/usr/local/bin/Rscript tests/run_shiny_phase2_workflow_smoke.R
/usr/local/bin/Rscript tests/run_shiny_mobile_layout_smoke.R
```

Expected: both pass and no page-level horizontal overflow.

- [ ] **Step 4: Commit**

```bash
git add inst/app/modules/mod_data_hub.R inst/app/www/app.css
git commit -m "fix: polish data hub tables"
```

---

### Task 5: Reduce Toast Overlap

**Files:**
- Modify or create: `inst/app/www/app.css`
- Test: `tests/run_shiny_task_feedback_smoke.R`

- [ ] **Step 1: Add notification spacing**

Add:

```css
.shiny-notification {
  max-width: min(420px, calc(100vw - 2rem));
}

#shiny-notification-panel {
  right: 1rem;
  bottom: 1rem;
}
```

- [ ] **Step 2: Run task feedback smoke**

Run:

```bash
/usr/local/bin/Rscript tests/run_shiny_task_feedback_smoke.R
```

Expected: PASS; busy button states still appear and clear.

- [ ] **Step 3: Commit**

```bash
git add inst/app/www/app.css
git commit -m "fix: reduce notification overlap"
```

---

### Task 6: Final Visual QA Pass and Evidence Update

**Files:**
- Modify: `docs/ggnetview-shiny-release-evidence.md`
- Modify: `docs/ggnetview-shiny-next-todos.md`

- [ ] **Step 1: Run focused and full smoke commands**

Run:

```bash
/usr/local/bin/Rscript tests/run_shiny_visual_qa_polish_smoke.R
/usr/local/bin/Rscript tests/run_shiny_app_startup.R
/usr/local/bin/Rscript tests/run_shiny_phase2_workflow_smoke.R
/usr/local/bin/Rscript tests/run_shiny_graph_builder_modes_smoke.R
/usr/local/bin/Rscript tests/run_shiny_analysis_export_smoke.R
/usr/local/bin/Rscript tests/run_shiny_visual_layouts_smoke.R
/usr/local/bin/Rscript tests/run_shiny_environment_geometry_smoke.R
/usr/local/bin/Rscript tests/run_shiny_mobile_layout_smoke.R
/usr/local/bin/Rscript tests/run_shiny_task_feedback_smoke.R
```

Expected: all pass.

- [ ] **Step 2: Launch app for human visual review**

Run:

```bash
/usr/local/bin/Rscript -e 'shiny::runApp(".", host = "127.0.0.1", port = 4627, launch.browser = FALSE)'
```

Open the actual listening URL printed by Shiny. Check:

- Data Hub loads examples.
- Graph Builder builds a graph.
- Visual Lab draws a graph with the plot visually dominant.
- Topology and Export tabs remain reachable after Visual Lab interaction.
- Page-level horizontal overflow remains absent at desktop and mobile widths.

- [ ] **Step 3: Regenerate evidence report**

Run:

```bash
/usr/local/bin/Rscript -e 'source("R/app_smoke_coverage.R"); source("R/app_release_evidence.R"); generate_release_evidence_report("docs/ggnetview-shiny-release-evidence.md", final_audit = TRUE)'
```

- [ ] **Step 4: Update TODO baseline**

In `docs/ggnetview-shiny-next-todos.md`, add a new baseline bullet:

```markdown
- Visual QA polish now keeps Visual Lab plot preview dominant, stabilizes plot rendering, preserves tab navigation after sidebar use, and reduces table/notification visual friction.
```

Remove any now-fixed wording that says Visual Lab preview or navigation remains an immediate UX blocker.

- [ ] **Step 5: Commit**

```bash
git add docs/ggnetview-shiny-release-evidence.md docs/ggnetview-shiny-next-todos.md
git commit -m "docs: update visual qa evidence"
```

---

## Done Criteria

- `tests/run_shiny_visual_qa_polish_smoke.R` passes.
- Existing Visual Lab all-layout smoke still passes for 57/57 layouts.
- Startup, Phase 2 workflow, analysis/export, environment geometry, mobile layout, and task feedback smokes pass.
- Visual Lab draw no longer produces `figure margins too large` during the focused manual/browser path.
- Human visual review confirms the plot is the primary element in Visual Lab Preview.
- Top navigation remains clickable after Visual Lab sidebar use.
- Worktree is clean after commits.

## Recommended Execution Mode

Use Subagent-Driven execution for Task 1-3 if possible, because Visual Lab preview, navigation, and CSS behavior are coupled but testable as one focused slice. Tasks 4-6 can be inline after the core Visual Lab regression is green.
