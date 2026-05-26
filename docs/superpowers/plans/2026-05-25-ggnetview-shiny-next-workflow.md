# ggNetView Shiny Next Workflow Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Turn the rebuilt Shiny shell into a practical ggNetView workflow app that can load example data, expose core graph-building parameters, inspect graph outputs, display topology/robustness results, export artifacts, and verify with the user's `/usr/local/bin/R` runtime.

**Architecture:** Keep the current modular Shiny structure under `inst/app/modules/`. Add small pure helper functions beside each module so behavior can be tested without driving a browser. Use the existing registry as the shared state model and avoid rebuilding the app into a separate package.

**Tech Stack:** R 4.5 via `/usr/local/bin/Rscript`, Shiny, bslib, DT, igraph, ggplot2, ggNetView source functions under root `R/`, testthat smoke scripts.

---

## File Structure

- Modify `Makefile`: add repeatable local commands that explicitly use `/usr/local/bin/R` and `/usr/local/bin/Rscript`.
- Modify `README.md`: document the preferred local runtime commands and the expected verification sequence.
- Modify `inst/app/modules/mod_data_hub.R`: add a built-in example loader and upload preview.
- Modify `inst/app/modules/mod_graph_builder.R`: add parameter controls and a pure `graph_builder_params()` helper.
- Modify `inst/app/modules/mod_graph_explorer.R`: keep node/edge table outputs and add graph metrics summary.
- Modify `inst/app/modules/mod_topology_results.R`: display and register both topology and robustness tables.
- Modify `inst/app/modules/mod_visual_lab.R`: add parameter preview and safer plot registration metadata.
- Modify `inst/app/modules/mod_export_center.R`: add plot PNG/PDF downloads and an object manifest export.
- Create or modify `tests/testthat/test-shiny-workflow-helpers.R`: pure helper tests for module behavior.
- Modify `tests/run_shiny_core_workflow_smoke.R`: use the same parameters exposed in the UI.
- Create `tests/run_shiny_app_startup.R`: a reusable app startup smoke.

---

### Task 1: Pin Local R Runtime Commands

**Files:**
- Create or modify: `Makefile`
- Modify: `README.md`

- [ ] **Step 1: Add Makefile targets**

Create `Makefile` if it does not exist. If it exists, add these targets without removing existing commands:

```makefile
R ?= /usr/local/bin/R
RSCRIPT ?= /usr/local/bin/Rscript

.PHONY: shiny-startup shiny-smoke shiny-build shiny-test-helpers shiny-run

shiny-startup:
	$(RSCRIPT) tests/run_shiny_app_startup.R

shiny-smoke:
	$(RSCRIPT) tests/run_shiny_core_workflow_smoke.R

shiny-build:
	$(R) CMD build . --no-build-vignettes --no-manual

shiny-test-helpers:
	$(RSCRIPT) -e 'library(testthat); source("R/app_registry.R"); source("R/app_validation.R"); source("R/app_adapters.R"); source("R/app_exports.R"); source("R/launch_ggNetView.R"); source("tests/testthat/test-app-registry.R"); source("tests/testthat/test-app-adapters.R"); source("tests/testthat/test-app-validation.R"); source("tests/testthat/test-app-exports.R"); source("tests/testthat/test-launch.R"); source("tests/testthat/test-shiny-files.R"); source("tests/testthat/test-shiny-modules.R"); if (file.exists("tests/testthat/test-shiny-workflow-helpers.R")) source("tests/testthat/test-shiny-workflow-helpers.R")'

shiny-run:
	$(RSCRIPT) -e 'shiny::runApp("inst/app", launch.browser = TRUE)'
```

- [ ] **Step 2: Create startup smoke script**

Create `tests/run_shiny_app_startup.R`:

```r
repo_root <- normalizePath(file.path(dirname(normalizePath(sys.frame(1)$ofile %||% "tests/run_shiny_app_startup.R")), ".."), mustWork = FALSE)
if (!dir.exists(file.path(repo_root, "inst", "app"))) {
  repo_root <- normalizePath(".", mustWork = TRUE)
}
setwd(file.path(repo_root, "inst", "app"))

source("global.R")
source("ui.R")
source("server.R")

app <- shiny::shinyApp(ui, server)
stopifnot(inherits(app, "shiny.appobj"))
cat("shiny app startup passed\n")
```

If `%||%` is not available in this execution context, use this simpler script instead:

```r
repo_root <- normalizePath(file.path("..", ".."), mustWork = TRUE)
setwd(file.path(repo_root, "inst", "app"))

source("global.R")
source("ui.R")
source("server.R")

app <- shiny::shinyApp(ui, server)
stopifnot(inherits(app, "shiny.appobj"))
cat("shiny app startup passed\n")
```

- [ ] **Step 3: Run startup target**

Run:

```bash
make shiny-startup
```

Expected:

```text
shiny app startup passed
```

- [ ] **Step 4: Update README commands**

In `README.md`, add a short local development section:

```markdown
### Local Shiny Development

This project should be verified with the system R runtime that has the ggNetView dependency stack installed:

```bash
make shiny-startup
make shiny-test-helpers
make shiny-smoke
make shiny-build
make shiny-run
```

The Makefile defaults to `/usr/local/bin/R` and `/usr/local/bin/Rscript`.
```
```

- [ ] **Step 5: Commit**

```bash
git add Makefile README.md tests/run_shiny_app_startup.R
git commit -m "chore: add Shiny runtime commands"
```

---

### Task 2: Add Example Data Loader and Preview

**Files:**
- Modify: `inst/app/modules/mod_data_hub.R`
- Create or modify: `tests/testthat/test-shiny-workflow-helpers.R`

- [ ] **Step 1: Add failing helper tests**

Append to `tests/testthat/test-shiny-workflow-helpers.R`:

```r
source(test_path("../../R/app_validation.R"))
source(test_path("../../R/app_registry.R"))
source(test_path("../../inst/app/modules/mod_data_hub.R"))

test_that("example matrix path resolves", {
  path <- app_example_matrix_path()
  expect_true(file.exists(path))
  expect_match(path, "example_matrix[.]csv$")
})

test_that("preview table limits rows and columns", {
  x <- data.frame(a = 1:20, b = 21:40, c = 41:60)
  preview <- preview_table(x, max_rows = 5, max_cols = 2)
  expect_equal(nrow(preview), 5L)
  expect_equal(ncol(preview), 2L)
  expect_equal(names(preview), c("a", "b"))
})
```

- [ ] **Step 2: Run test to verify it fails**

Run:

```bash
/usr/local/bin/Rscript -e 'library(testthat); source("tests/testthat/test-shiny-workflow-helpers.R")'
```

Expected: FAIL because `app_example_matrix_path()` and `preview_table()` are not defined.

- [ ] **Step 3: Add helper functions and UI controls**

In `inst/app/modules/mod_data_hub.R`, add above `mod_data_hub_ui()`:

```r
app_example_matrix_path <- function() {
  candidates <- c(
    file.path(getwd(), "inst", "extdata", "example_matrix.csv"),
    file.path(getwd(), "..", "..", "inst", "extdata", "example_matrix.csv"),
    system.file("extdata", "example_matrix.csv", package = "ggNetView")
  )
  candidates <- candidates[nzchar(candidates)]
  existing <- candidates[file.exists(candidates)]
  if (!length(existing)) {
    stop("Cannot find bundled example_matrix.csv.", call. = FALSE)
  }
  normalizePath(existing[[1]], mustWork = TRUE)
}

preview_table <- function(x, max_rows = 10, max_cols = 8) {
  x <- as.data.frame(x, check.names = FALSE)
  x[seq_len(min(nrow(x), max_rows)), seq_len(min(ncol(x), max_cols)), drop = FALSE]
}
```

In `mod_data_hub_ui()`, add below `actionButton(ns("register"), "Register object")`:

```r
shiny::actionButton(ns("load_example"), "Load example matrix")
```

Add a preview card:

```r
bslib::card(
  bslib::card_header("Preview"),
  DT::DTOutput(ns("preview"))
)
```

- [ ] **Step 4: Register example data**

Inside `mod_data_hub_server()`, add:

```r
current_table <- shiny::reactiveVal(NULL)

register_table <- function(table, name, source) {
  type <- detect_upload_type(table)
  validation <- if (identical(type, "matrix")) validate_matrix_like(table) else app_success(table)
  if (!validation$ok) {
    shiny::showNotification(validation$message, type = "error")
    return(NULL)
  }

  registry_add(
    registry,
    name = name,
    type = type,
    data = validation$value,
    source = source,
    warnings = validation$warnings
  )
}
```

Update upload registration to use `register_table()`, and add:

```r
shiny::observeEvent(input$load_example, {
  path <- app_example_matrix_path()
  table <- read_user_table(path)
  current_table(table)
  item <- register_table(table, "example_matrix", basename(path))
  if (!is.null(item)) {
    shiny::showNotification(paste("Registered", item$name), type = "message")
  }
})

output$preview <- DT::renderDT({
  table <- current_table()
  shiny::req(table)
  preview_table(table)
}, rownames = FALSE)
```

- [ ] **Step 5: Run helper tests**

Run:

```bash
make shiny-test-helpers
```

Expected: PASS for the new helper tests and existing helper tests.

- [ ] **Step 6: Commit**

```bash
git add inst/app/modules/mod_data_hub.R tests/testthat/test-shiny-workflow-helpers.R
git commit -m "feat: add example data loading"
```

---

### Task 3: Expose Graph Builder Parameters

**Files:**
- Modify: `inst/app/modules/mod_graph_builder.R`
- Modify: `tests/testthat/test-shiny-workflow-helpers.R`
- Modify: `tests/run_shiny_core_workflow_smoke.R`

- [ ] **Step 1: Add failing parameter test**

Append to `tests/testthat/test-shiny-workflow-helpers.R`:

```r
source(test_path("../../inst/app/modules/mod_graph_builder.R"))

test_that("graph builder params match ggNetView matrix workflow", {
  params <- graph_builder_params(
    builder = "matrix",
    method = "cor",
    cor_method = "pearson",
    proc = "none",
    r_threshold = 0.1,
    p_threshold = 1,
    module_method = "Fast_greedy"
  )

  expect_equal(params$method, "cor")
  expect_equal(params$cor.method, "pearson")
  expect_equal(params$proc, "none")
  expect_equal(params$r.threshold, 0.1)
  expect_equal(params$p.threshold, 1)
  expect_equal(params$module.method, "Fast_greedy")
})

test_that("graph builder params are empty for edge table builder", {
  params <- graph_builder_params(builder = "edge_table")
  expect_equal(params, list())
})
```

- [ ] **Step 2: Run test to verify it fails**

Run:

```bash
/usr/local/bin/Rscript -e 'library(testthat); source("R/app_validation.R"); source("R/app_registry.R"); source("tests/testthat/test-shiny-workflow-helpers.R")'
```

Expected: FAIL because `graph_builder_params()` is not defined.

- [ ] **Step 3: Add helper and controls**

Add above `mod_graph_builder_ui()`:

```r
graph_builder_params <- function(
  builder,
  method = "cor",
  cor_method = "pearson",
  proc = "none",
  r_threshold = 0.1,
  p_threshold = 1,
  module_method = "Fast_greedy"
) {
  if (!identical(builder, "matrix")) {
    return(list())
  }

  list(
    method = method,
    cor.method = cor_method,
    proc = proc,
    r.threshold = r_threshold,
    p.threshold = p_threshold,
    module.method = module_method
  )
}
```

In `mod_graph_builder_ui()`, add controls below builder:

```r
shiny::selectInput(ns("method"), "Association method", choices = c("cor")),
shiny::selectInput(ns("cor_method"), "Correlation", choices = c("pearson", "spearman")),
shiny::selectInput(ns("proc"), "Preprocess", choices = c("none")),
shiny::numericInput(ns("r_threshold"), "r threshold", value = 0.1, min = 0, max = 1, step = 0.01),
shiny::numericInput(ns("p_threshold"), "p threshold", value = 1, min = 0, max = 1, step = 0.01),
shiny::selectInput(ns("module_method"), "Module method", choices = c("Fast_greedy", "Louvain", "Walktrap"))
```

In `observeEvent(input$build, ...)`, replace `params = list()` with:

```r
params <- graph_builder_params(
  builder = input$builder,
  method = input$method,
  cor_method = input$cor_method,
  proc = input$proc,
  r_threshold = input$r_threshold,
  p_threshold = input$p_threshold,
  module_method = input$module_method
)

result <- safe_build_graph(source$data, input$builder, params = params)
```

Register `params = params`.

- [ ] **Step 4: Align smoke script**

In `tests/run_shiny_core_workflow_smoke.R`, replace the hard-coded `graph_params <- list(...)` with:

```r
source_repo_file("inst", "app", "modules", "mod_graph_builder.R")
graph_params <- graph_builder_params(
  builder = "matrix",
  method = "cor",
  cor_method = "pearson",
  proc = "none",
  r_threshold = 0.1,
  p_threshold = 1,
  module_method = "Fast_greedy"
)
```

- [ ] **Step 5: Run tests and smoke**

Run:

```bash
make shiny-test-helpers
make shiny-smoke
```

Expected:

```text
core workflow smoke passed
```

- [ ] **Step 6: Commit**

```bash
git add inst/app/modules/mod_graph_builder.R tests/testthat/test-shiny-workflow-helpers.R tests/run_shiny_core_workflow_smoke.R
git commit -m "feat: expose graph builder parameters"
```

---

### Task 4: Display Robustness Results

**Files:**
- Modify: `inst/app/modules/mod_topology_results.R`
- Modify: `tests/testthat/test-shiny-workflow-helpers.R`

- [ ] **Step 1: Add failing robustness test**

Append to `tests/testthat/test-shiny-workflow-helpers.R`:

```r
source(test_path("../../inst/app/modules/mod_topology_results.R"))

test_that("topology robustness table extracts optional robustness payload", {
  robustness <- data.frame(step = 1:3, score = c(1, 0.7, 0.2))
  payload <- list(
    topology = data.frame(metric = "nodes", value = 3),
    Robustness = robustness
  )

  expect_equal(topology_robustness_table(payload), robustness)
  expect_equal(topology_robustness_table(data.frame(a = 1)), data.frame())
})
```

- [ ] **Step 2: Run test to verify it fails**

Run:

```bash
/usr/local/bin/Rscript -e 'library(testthat); source("tests/testthat/test-shiny-workflow-helpers.R")'
```

Expected: FAIL because `topology_robustness_table()` is not defined.

- [ ] **Step 3: Add helper and UI output**

Add below `topology_result_table()`:

```r
topology_robustness_table <- function(value) {
  if (is.list(value) && is.data.frame(value$Robustness)) {
    return(value$Robustness)
  }
  data.frame()
}
```

In `mod_topology_results_ui()`, add another card:

```r
bslib::card(
  bslib::card_header("Robustness"),
  DT::DTOutput(ns("robustness"))
)
```

Use `col_widths = c(4, 8, 12)` if needed.

- [ ] **Step 4: Register robustness when present**

Inside server, add:

```r
robustness_table <- shiny::reactiveVal(data.frame())
```

After `table <- topology_result_table(result$value)`:

```r
robustness <- topology_robustness_table(result$value)
robustness_table(robustness)
if (nrow(robustness) > 0L) {
  registry_add(
    registry,
    name = unique_output_name(paste0(graph_item$name, "_robustness")),
    type = "result",
    data = robustness,
    source = graph_item$id,
    params = list(metric = "network_robustness")
  )
}
```

Add output:

```r
output$robustness <- DT::renderDT(robustness_table(), rownames = FALSE)
```

- [ ] **Step 5: Run tests**

Run:

```bash
make shiny-test-helpers
make shiny-startup
```

Expected: tests pass and startup passes.

- [ ] **Step 6: Commit**

```bash
git add inst/app/modules/mod_topology_results.R tests/testthat/test-shiny-workflow-helpers.R
git commit -m "feat: show topology robustness results"
```

---

### Task 5: Improve Visual Lab Parameter Metadata

**Files:**
- Modify: `inst/app/modules/mod_visual_lab.R`
- Modify: `tests/testthat/test-shiny-workflow-helpers.R`

- [ ] **Step 1: Add failing visual parameter test**

Append to `tests/testthat/test-shiny-workflow-helpers.R`:

```r
source(test_path("../../inst/app/modules/mod_visual_lab.R"))

test_that("visual lab params are stable and JSON-friendly", {
  params <- visual_lab_params(
    layout = "nicely",
    show_labels = TRUE,
    label_layout = "two_column",
    label_wrap_width = 18,
    bandwidth_scale = 1
  )

  expect_equal(params$layout, "nicely")
  expect_true(params$label)
  expect_equal(params$label_layout, "two_column")
  expect_equal(params$label_wrap_width, 18)
  expect_equal(params$bandwidth_scale, 1)
})
```

- [ ] **Step 2: Run test to verify it fails**

Run:

```bash
/usr/local/bin/Rscript -e 'library(testthat); source("tests/testthat/test-shiny-workflow-helpers.R")'
```

Expected: FAIL because `visual_lab_params()` is not defined.

- [ ] **Step 3: Add helper and metadata preview**

Add above `mod_visual_lab_ui()`:

```r
visual_lab_params <- function(
  layout,
  show_labels,
  label_layout,
  label_wrap_width,
  bandwidth_scale
) {
  list(
    layout = layout,
    label = isTRUE(show_labels),
    label_layout = label_layout,
    label_wrap_width = as.numeric(label_wrap_width),
    bandwidth_scale = as.numeric(bandwidth_scale)
  )
}
```

In server, replace the inline `params <- list(...)` with:

```r
params <- visual_lab_params(
  layout = input$layout,
  show_labels = input$show_labels,
  label_layout = input$label_layout,
  label_wrap_width = input$label_wrap_width,
  bandwidth_scale = input$bandwidth_scale
)
```

Add UI output below status:

```r
shiny::verbatimTextOutput(ns("params"))
```

Add server output:

```r
output$params <- shiny::renderText({
  jsonlite::toJSON(
    visual_lab_params(
      layout = input$layout,
      show_labels = input$show_labels,
      label_layout = input$label_layout,
      label_wrap_width = input$label_wrap_width,
      bandwidth_scale = input$bandwidth_scale
    ),
    auto_unbox = TRUE,
    pretty = TRUE
  )
})
```

- [ ] **Step 4: Run tests and smoke**

Run:

```bash
make shiny-test-helpers
make shiny-smoke
```

Expected: tests pass and smoke prints `core workflow smoke passed`.

- [ ] **Step 5: Commit**

```bash
git add inst/app/modules/mod_visual_lab.R tests/testthat/test-shiny-workflow-helpers.R
git commit -m "feat: preview visual lab parameters"
```

---

### Task 6: Add Plot and Manifest Exports

**Files:**
- Modify: `inst/app/modules/mod_export_center.R`
- Modify: `tests/testthat/test-shiny-workflow-helpers.R`

- [ ] **Step 1: Add failing manifest test**

Append to `tests/testthat/test-shiny-workflow-helpers.R`:

```r
source(test_path("../../inst/app/modules/mod_export_center.R"))

test_that("registry manifest captures export metadata", {
  registry <- registry_new()
  registry_add(registry, name = "m", type = "matrix", data = matrix(1, nrow = 1), source = "unit")
  registry_add(registry, name = "g", type = "graph", data = list(), source = "obj_1", params = list(builder = "matrix"))

  manifest <- registry_manifest(registry)

  expect_true(all(c("id", "name", "type", "source", "created_at") %in% names(manifest)))
  expect_equal(nrow(manifest), 2L)
})
```

- [ ] **Step 2: Run test to verify it fails**

Run:

```bash
/usr/local/bin/Rscript -e 'library(testthat); source("R/app_registry.R"); source("tests/testthat/test-shiny-workflow-helpers.R")'
```

Expected: FAIL because `registry_manifest()` is not defined.

- [ ] **Step 3: Add manifest helper and download**

Add above `mod_export_center_server()`:

```r
registry_manifest <- function(registry) {
  listed <- shiny::isolate(registry_list(registry))
  if (!nrow(listed)) {
    return(data.frame())
  }
  listed
}
```

In UI, add:

```r
shiny::downloadButton(ns("download_manifest"), "Download Manifest"),
shiny::downloadButton(ns("download_png"), "Download Plot PNG"),
shiny::downloadButton(ns("download_pdf"), "Download Plot PDF")
```

Add server download handlers:

```r
output$download_manifest <- shiny::downloadHandler(
  filename = function() "ggnetview_manifest.csv",
  content = function(file) write_registry_table(registry_manifest(registry), file)
)

output$download_png <- shiny::downloadHandler(
  filename = function() paste0(safe_download_base(selected_item()), ".png"),
  content = function(file) write_plot_png(selected_item()$data, file)
)

output$download_pdf <- shiny::downloadHandler(
  filename = function() paste0(safe_download_base(selected_item()), ".pdf"),
  content = function(file) write_plot_pdf(selected_item()$data, file)
)
```

Guard plot downloads by validating selected object type:

```r
shiny::validate(shiny::need(identical(selected_item()$type, "plot"), "PNG/PDF export requires a plot object."))
```

- [ ] **Step 4: Run tests**

Run:

```bash
make shiny-test-helpers
```

Expected: tests pass.

- [ ] **Step 5: Commit**

```bash
git add inst/app/modules/mod_export_center.R tests/testthat/test-shiny-workflow-helpers.R
git commit -m "feat: add plot and manifest exports"
```

---

### Task 7: Final Verification and Handoff

**Files:**
- Modify: `README.md`
- Modify if needed: `docs/superpowers/plans/2026-05-25-ggnetview-shiny-next-workflow.md`

- [ ] **Step 1: Run full local verification**

Run:

```bash
make shiny-startup
make shiny-test-helpers
make shiny-smoke
make shiny-build
```

Expected:

```text
shiny app startup passed
core workflow smoke passed
* building 'ggNetView_0.1.0.tar.gz'
```

- [ ] **Step 2: Remove build artifact**

Run:

```bash
rm -f ggNetView_*.tar.gz
git status --short
```

Expected: no build tarball is listed.

- [ ] **Step 3: Document current workflow**

Add to `README.md`:

```markdown
### Current Shiny Workflow

1. Load the bundled example matrix or upload a CSV/TSV/TXT table in Data Hub.
2. Build a graph from a matrix, adjacency matrix, or edge table in Graph Builder.
3. Inspect graph summary, nodes, and edges in Graph Explorer.
4. Draw and register a ggNetView plot in Visual Lab.
5. Calculate topology and robustness outputs in Topology Results.
6. Export objects, parameters, tables, plots, and the object manifest in Export Center.
```

- [ ] **Step 4: Commit**

```bash
git add README.md docs/superpowers/plans/2026-05-25-ggnetview-shiny-next-workflow.md
git commit -m "docs: update Shiny workflow handoff"
```

- [ ] **Step 5: Final status**

Run:

```bash
git status --short --branch
git log --oneline -8
```

Expected: only intentionally ignored local source materials remain hidden; branch is ready for the next checkpoint.

---

## Self-Review

- Spec coverage: This plan continues from the rebuilt Shiny shell and covers the next missing user-facing workflow slices: runtime commands, example load, graph builder params, topology robustness, visual metadata, exports, verification, and handoff docs.
- Placeholder scan: No `TBD`, `TODO`, or unspecified "add tests" steps remain. Each task includes exact files, code snippets, commands, expected results, and commit message.
- Type consistency: Helper names are stable across tasks: `app_example_matrix_path()`, `preview_table()`, `graph_builder_params()`, `topology_robustness_table()`, `visual_lab_params()`, and `registry_manifest()`.

