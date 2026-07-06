# Adapt inst/app Shiny to the new ggNetView — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Merge the new `package/ggNetView` core into this repo's bundled `R/` (preserving repo-only audit fixes) and adapt the Shiny app to expose the two new functions (`gglink_heatmaps_2`, `ggnetview_subgraph`) and survive four breaking changes.

**Architecture:** The app reaches core functions through `resolve_ggnetview_function()` + `safe_*` adapters (`R/app_adapters.R`, `R/app_compare_environment.R`, `R/app_graph_inspect.R`). New behavior is added by (1) a reviewed per-file core merge, (2) two new `safe_*` adapters that reuse existing infrastructure, (3) small additive wiring in `mod_graph_explorer` and `mod_compare_environment` (which backs `mod_environment_links`), and (4) defensive validation for the breaking changes. Test-first throughout with `testthat`.

**Tech Stack:** R, Shiny, bslib, DT, testthat, igraph/tidygraph, ggplot2/ggraph, patchwork.

**Reference spec:** `docs/superpowers/specs/2026-07-06-shiny-adapt-new-ggnetview-design.md`

---

## File structure

- Core sync (Part A): overwrite/merge 14 files under `R/`, edit `NAMESPACE`, add 2 files under `man/`.
- `R/app_compare_environment.R` — refactor `safe_environment_heatmap` to be function-name-parametrized; add `safe_link_heatmap_adaptive`.
- `R/app_adapters.R` — add `safe_magnified_subgraph`.
- `inst/app/modules/mod_graph_explorer.R` — add "Magnified subgraph" control + output.
- `inst/app/modules/mod_environment_links.R` — add second plot card (side-by-side).
- `inst/app/modules/mod_compare_environment.R` — compute + render the adaptive plot alongside the standard one.
- `inst/app/global.R` — add new helper names to the preload vector.
- `tests/testthat/test-new-functions.R` (new), plus edits to `test-shiny-files.R` and a `test-backend-layout-knn.R` regression assertion.

---

## Task 1: Create feature branch

**Files:** none (git only)

- [ ] **Step 1: Branch off main**

Run:
```bash
cd /Users/liuyue/Desktop/R/R_Package_development/ggNetView_Shiny
git checkout -b feat/adapt-new-ggnetview
```
Expected: `Switched to a new branch 'feat/adapt-new-ggnetview'`

---

## Task 2: Sync the two new core function files

**Files:**
- Create: `R/gglink_heatmaps_2.R` (copy of `package/ggNetView/R/gglink_heatmaps_2.R`)
- Create: `R/ggnetview_subgraph.R` (copy of `package/ggNetView/R/ggnetview_subgraph.R`)
- Create: `man/gglink_heatmaps_2.Rd`, `man/ggnetview_subgraph.Rd` (copy from `package/ggNetView/man/`)
- Modify: `NAMESPACE`

- [ ] **Step 1: Copy the two new source files and their man pages**

Run:
```bash
cp package/ggNetView/R/gglink_heatmaps_2.R R/gglink_heatmaps_2.R
cp package/ggNetView/R/ggnetview_subgraph.R R/ggnetview_subgraph.R
cp package/ggNetView/man/gglink_heatmaps_2.Rd man/gglink_heatmaps_2.Rd
cp package/ggNetView/man/ggnetview_subgraph.Rd man/ggnetview_subgraph.Rd
```
Expected: no output (files copied). If a man page is missing upstream, generate later with `devtools::document()`.

- [ ] **Step 2: Add the two exports to NAMESPACE (keep launch_ggNetView)**

In `NAMESPACE`, add these two lines in alphabetical position among the other `export(...)` lines:
```r
export(gglink_heatmaps_2)
export(ggnetview_subgraph)
```
Do **not** remove `export(launch_ggNetView)`.

- [ ] **Step 3: Verify both functions load**

Run:
```bash
Rscript -e 'pkgload::load_all(".", quiet=TRUE); stopifnot(is.function(gglink_heatmaps_2), is.function(ggnetview_subgraph)); cat("OK\n")'
```
Expected: `OK`

- [ ] **Step 4: Commit**

```bash
git add R/gglink_heatmaps_2.R R/ggnetview_subgraph.R man/gglink_heatmaps_2.Rd man/ggnetview_subgraph.Rd NAMESPACE
git commit -m "feat(core): vendor gglink_heatmaps_2 and ggnetview_subgraph from new ggNetView"
```

---

## Task 3: Merge the 12 changed core files, preserving repo audit fixes

**Files (all under `R/`):** `get_geo_neighbors.R`, `get_graph_adjacency.R`, `get_network_perturbation.R`, `get_node_centrality.R`, `get_subgraph.R`, `gglink_heatmaps.R`, `ggnetview_modularity_heatmaps.R`, `ggNetView_multi_link.R`, `ggnetview_zipi.R`, `ggnetview.R`, `globals.R`, `mantel_utils.R`

- [ ] **Step 1: Record the repo-only audit fixes BEFORE overwriting**

Run (saves the two known repo-only fixes for re-application):
```bash
diff R/ggnetview.R package/ggNetView/R/ggnetview.R > /tmp/ggnetview_fix.diff; cat /tmp/ggnetview_fix.diff
diff R/get_geo_neighbors.R package/ggNetView/R/get_geo_neighbors.R > /tmp/geo_fix.diff; cat /tmp/geo_fix.diff
```
Expected: the `k_nn` clamp lines shown (`audit H1`). Keep this output visible.

- [ ] **Step 2: Review each of the 12 diffs for any OTHER repo-only removed lines**

Run:
```bash
for b in get_geo_neighbors.R get_graph_adjacency.R get_network_perturbation.R get_node_centrality.R get_subgraph.R gglink_heatmaps.R ggnetview_modularity_heatmaps.R ggNetView_multi_link.R ggnetview_zipi.R ggnetview.R globals.R mantel_utils.R; do
  echo "===== $b ====="; diff R/$b package/ggNetView/R/$b | grep '^<' | grep -iv 'roxygen\|^< *#'
done
```
Expected: for each `<` line (present only in repo), confirm it is the code the new package intentionally replaces — NOT an unrelated repo fix. The only known repo-only *fixes* to preserve are the `audit H1` `k_nn` clamps in `ggnetview.R` and `get_geo_neighbors.R`. If any other `<` line looks like a repo-specific correctness fix, note it and preserve it in Step 4.

- [ ] **Step 3: Overwrite the 12 files with the new-package versions**

Run:
```bash
for b in get_geo_neighbors.R get_graph_adjacency.R get_network_perturbation.R get_node_centrality.R get_subgraph.R gglink_heatmaps.R ggnetview_modularity_heatmaps.R ggNetView_multi_link.R ggnetview_zipi.R ggnetview.R globals.R mantel_utils.R; do
  cp package/ggNetView/R/$b R/$b
done
```
Expected: no output.

- [ ] **Step 4: Re-apply the audit-H1 clamp in `R/ggnetview.R`**

In `R/ggnetview.R`, find (around the `k_nn_cap` block):
```r
    k_nn_cap <- max(1, nrow(ly1) - 1)
    k_nn_try <- k_nn
```
Replace the second line with:
```r
    k_nn_try <- min(k_nn, k_nn_cap)   # clamp first attempt to n-1 (audit H1)
```

- [ ] **Step 5: Re-apply the audit-H1 clamp in `R/get_geo_neighbors.R` (two spots)**

In `R/get_geo_neighbors.R`, immediately before each `FNN::get.knn(` call that uses `k_nn` on a `layout` (two occurrences), insert:
```r
  # Clamp k_nn to at most n-1 to avoid FNN::get.knn C-level ANN error on
  # small networks (audit H1).  This is a no-op when k_nn <= nrow(layout)-1.
  k_nn <- max(1L, min(as.integer(k_nn), nrow(layout) - 1L))
```
Use the pre-overwrite `/tmp/geo_fix.diff` to place them at the exact original locations.

- [ ] **Step 6: Confirm the clamps are back**

Run:
```bash
grep -n "audit H1" R/ggnetview.R R/get_geo_neighbors.R
```
Expected: 1 hit in `ggnetview.R`, 2 hits in `get_geo_neighbors.R`.

- [ ] **Step 7: Load-all sanity check**

Run:
```bash
Rscript -e 'pkgload::load_all(".", quiet=TRUE); cat("load OK\n")'
```
Expected: `load OK` (no errors).

- [ ] **Step 8: Commit**

```bash
git add R/*.R
git commit -m "feat(core): merge 12 changed core files from new ggNetView; preserve audit-H1 k_nn clamps"
```

---

## Task 4: Regression test guarding the k_nn clamp merge

**Files:**
- Test: `tests/testthat/test-backend-layout-knn.R` (append)

- [ ] **Step 1: Write the failing/guarding test**

Append to `tests/testthat/test-backend-layout-knn.R`:
```r
test_that("audit-H1 k_nn clamp survives the new-ggNetView merge", {
  gg_src <- readLines(testthat::test_path("../../R/ggnetview.R"), warn = FALSE)
  geo_src <- readLines(testthat::test_path("../../R/get_geo_neighbors.R"), warn = FALSE)

  expect_true(any(grepl("min(k_nn, k_nn_cap)", gg_src, fixed = TRUE)),
              info = "ggnetview.R must clamp k_nn_try to k_nn_cap (audit H1)")
  expect_equal(sum(grepl("audit H1", geo_src, fixed = TRUE)), 2L)
})
```

- [ ] **Step 2: Run the test**

Run:
```bash
Rscript -e 'testthat::test_file("tests/testthat/test-backend-layout-knn.R")'
```
Expected: all PASS (the clamps were re-applied in Task 3).

- [ ] **Step 3: Commit**

```bash
git add tests/testthat/test-backend-layout-knn.R
git commit -m "test: guard audit-H1 k_nn clamp against future core merges"
```

---

## Task 5: Adapter `safe_link_heatmap_adaptive` (wraps gglink_heatmaps_2)

**Files:**
- Modify: `R/app_compare_environment.R` (parametrize `safe_environment_heatmap`; add wrapper)
- Test: `tests/testthat/test-new-functions.R` (create)

- [ ] **Step 1: Write the failing test**

Create `tests/testthat/test-new-functions.R`:
```r
source(testthat::test_path("../../R/app_validation.R"))
source(testthat::test_path("../../R/app_adapters.R"))
source(testthat::test_path("../../R/app_compare_environment.R"))

test_that("safe_link_heatmap_adaptive resolves gglink_heatmaps_2 and returns a result object", {
  set.seed(1)
  spec <- as.data.frame(matrix(runif(6 * 8), nrow = 6,
                               dimnames = list(paste0("OTU", 1:6), paste0("S", 1:8))))
  env <- as.data.frame(matrix(runif(2 * 8), nrow = 2,
                              dimnames = list(c("pH", "temp"), paste0("S", 1:8))))

  result <- safe_link_heatmap_adaptive(env = env, spec = spec, params = list())

  expect_s3_class(result, "ggnetview_app_result")
  # Either a valid plot/stats list on success, or a friendly failure — never a raw error.
  if (result$ok) {
    expect_true(is.list(result$value))
  } else {
    expect_true(is.character(result$message) && nzchar(result$message))
  }
})
```

- [ ] **Step 2: Run to verify it fails**

Run:
```bash
Rscript -e 'testthat::test_file("tests/testthat/test-new-functions.R")'
```
Expected: FAIL with `could not find function "safe_link_heatmap_adaptive"`.

- [ ] **Step 3: Parametrize `safe_environment_heatmap` by function name**

In `R/app_compare_environment.R`, change the signature:
```r
safe_environment_heatmap <- function(env, spec, env_select = NULL, spec_select = NULL, env_blocks = NULL, spec_blocks = NULL, env_spec_pairs = NULL, params = list(), fn_name = "gglink_heatmaps") {
```
and change the resolver line at the top of its body from:
```r
  fn <- resolve_ggnetview_function("gglink_heatmaps")
  if (is.null(fn)) {
    return(app_failure("Cannot find ggNetView function: gglink_heatmaps"))
  }
```
to:
```r
  fn <- resolve_ggnetview_function(fn_name)
  if (is.null(fn)) {
    return(app_failure(paste0("Cannot find ggNetView function: ", fn_name)))
  }
```
Then, at the point where the assembled argument list is passed to `do.call(fn, ...)`, filter to the target function's formals so the two signatures stay compatible. Find the `do.call(` call and wrap its argument list:
```r
  call_args <- call_args[names(call_args) %in% names(formals(fn))]
```
(Insert this line immediately before the `do.call(fn, call_args ...)` invocation, using the actual local variable name that holds the argument list in that function — inspect the surrounding lines to confirm the name.)

- [ ] **Step 4: Add the thin adaptive wrapper**

At the end of `R/app_compare_environment.R`, add:
```r
#' Adaptive-tile variant of safe_environment_heatmap (gglink_heatmaps_2).
safe_link_heatmap_adaptive <- function(env, spec, env_select = NULL, spec_select = NULL, env_blocks = NULL, spec_blocks = NULL, env_spec_pairs = NULL, params = list()) {
  safe_environment_heatmap(
    env = env, spec = spec,
    env_select = env_select, spec_select = spec_select,
    env_blocks = env_blocks, spec_blocks = spec_blocks,
    env_spec_pairs = env_spec_pairs, params = params,
    fn_name = "gglink_heatmaps_2"
  )
}
```

- [ ] **Step 5: Run the test to verify it passes**

Run:
```bash
Rscript -e 'testthat::test_file("tests/testthat/test-new-functions.R")'
```
Expected: PASS.

- [ ] **Step 6: Confirm the existing env-heatmap tests still pass (no regression from the refactor)**

Run:
```bash
Rscript -e 'testthat::test_file("tests/testthat/test-app-compare-environment.R")'
Rscript -e 'testthat::test_file("tests/testthat/test-backend-heatmap-cor.R")'
```
Expected: all PASS.

- [ ] **Step 7: Commit**

```bash
git add R/app_compare_environment.R tests/testthat/test-new-functions.R
git commit -m "feat(app): add safe_link_heatmap_adaptive wrapping gglink_heatmaps_2"
```

---

## Task 6: Adapter `safe_magnified_subgraph` (wraps ggnetview_subgraph)

**Files:**
- Modify: `R/app_adapters.R`
- Test: `tests/testthat/test-new-functions.R` (append)

- [ ] **Step 1: Write the failing tests**

Append to `tests/testthat/test-new-functions.R`:
```r
test_that("safe_magnified_subgraph fails gracefully on non-igraph input", {
  result <- safe_magnified_subgraph(list(), select_module = "1")
  expect_false(result$ok)
  expect_match(result$message, "igraph", ignore.case = TRUE)
})

test_that("safe_magnified_subgraph fails gracefully when select_module is not a module level", {
  library(igraph)
  nodes <- data.frame(name = c("A","B","C","D"),
                      Modularity = factor(c("1","1","2","2")),
                      stringsAsFactors = FALSE)
  edges <- data.frame(from = c("A","B","C"), to = c("B","C","D"), stringsAsFactors = FALSE)
  g <- igraph::graph_from_data_frame(edges, directed = FALSE, vertices = nodes)

  result <- safe_magnified_subgraph(g, select_module = "999")
  expect_false(result$ok)
  expect_match(result$message, "module", ignore.case = TRUE)
})
```

- [ ] **Step 2: Run to verify failure**

Run:
```bash
Rscript -e 'testthat::test_file("tests/testthat/test-new-functions.R")'
```
Expected: FAIL with `could not find function "safe_magnified_subgraph"`.

- [ ] **Step 3: Implement the adapter**

Append to `R/app_adapters.R`:
```r
#' Wrap ggnetview_subgraph: full network + magnified module panel.
#' Returns a ggnetview_app_result whose $value is the composed patchwork plot.
safe_magnified_subgraph <- function(graph, select_module = NULL, full_layout = "gephi", sub_layout = "same", params = list()) {
  if (!inherits(graph, "igraph")) {
    return(app_failure("Magnified subgraph requires an igraph graph object."))
  }
  fn <- resolve_ggnetview_function("ggnetview_subgraph")
  if (is.null(fn)) {
    return(app_failure("Cannot find ggNetView function: ggnetview_subgraph"))
  }

  mods <- tryCatch(igraph::vertex_attr(graph, "Modularity"), error = function(e) NULL)
  if (is.null(mods)) {
    return(app_failure("The graph has no Modularity column; rebuild it with a build_graph_from_* function."))
  }
  levels_present <- as.character(unique(mods))
  requested <- as.character(select_module)
  if (length(requested) == 0L || !all(requested %in% levels_present)) {
    return(app_failure(paste0(
      "Selected module(s) not found. Available modules: ",
      paste(sort(levels_present), collapse = ", "), "."
    )))
  }

  args <- utils::modifyList(
    list(graph_obj = graph, select_module = select_module,
         full_layout = full_layout, sub_layout = sub_layout),
    params
  )
  safe_call(do.call(fn, args), "Failed to build magnified subgraph figure.")
}
```

- [ ] **Step 4: Run to verify pass**

Run:
```bash
Rscript -e 'testthat::test_file("tests/testthat/test-new-functions.R")'
```
Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add R/app_adapters.R tests/testthat/test-new-functions.R
git commit -m "feat(app): add safe_magnified_subgraph wrapping ggnetview_subgraph"
```

---

## Task 7: Wire "Magnified subgraph" into mod_graph_explorer

**Files:**
- Modify: `inst/app/modules/mod_graph_explorer.R`
- Modify: `tests/testthat/test-shiny-files.R`

- [ ] **Step 1: Write the failing wiring assertion**

Append to `tests/testthat/test-shiny-files.R`:
```r
test_that("graph explorer wires the magnified subgraph function", {
  txt <- paste(readLines(test_path("../../inst/app/modules/mod_graph_explorer.R"), warn = FALSE), collapse = "\n")
  expect_match(txt, "safe_magnified_subgraph", fixed = TRUE)
  expect_match(txt, "render_magnified", fixed = TRUE)
})
```

- [ ] **Step 2: Run to verify failure**

Run:
```bash
Rscript -e 'testthat::test_file("tests/testthat/test-shiny-files.R")'
```
Expected: the new test FAILs (`safe_magnified_subgraph` not present).

- [ ] **Step 3: Add the UI control**

In `mod_graph_explorer_ui`, inside the `"Module subgraph"` `accordion_panel` (after the `register_module_subgraph` button, line ~15), add:
```r
          shiny::actionButton(ns("render_magnified"), "Show magnified subgraph", class = "w-100 mt-2")
```

- [ ] **Step 4: Add a plot card to the results grid**

In `mod_graph_explorer_ui`, add a new card to the `bslib::layout_columns(...)` results grid (before `col_widths`), and extend `col_widths` by one `12`:
```r
      bslib::card(
        bslib::card_header("Magnified Subgraph"),
        shiny::plotOutput(ns("magnified"), height = 500)
      ),
```
Update `col_widths = c(12, 6, 6, 6, 6)` to `col_widths = c(12, 6, 6, 6, 6, 12)`.

- [ ] **Step 5: Add the server logic**

In `mod_graph_explorer_server`, add a reactiveVal near the others (line ~101):
```r
    magnified_plot <- shiny::reactiveVal(NULL)
```
Then add an observer (after the `register_module_subgraph` observer):
```r
    shiny::observeEvent(input$render_magnified, {
      item <- selected_graph()
      shiny::req(item, input$module)
      ig <- tryCatch(coerce_tbl_graph(item$data), error = function(e) NULL)
      if (is.null(ig)) {
        status("Could not coerce the selected object to a graph.")
        shiny::showNotification("Could not coerce the selected object to a graph.", type = "error")
        return()
      }
      result <- safe_magnified_subgraph(ig, select_module = input$module)
      if (!result$ok) {
        detail <- if (!is.null(result$trace)) paste(result$message, result$trace, sep = "\n") else result$message
        status(detail)
        shiny::showNotification(result$message, type = "error")
        return()
      }
      magnified_plot(result$value)
      registry_add(
        registry,
        name = paste0(item$name, "_magnified_", input$module),
        type = "result",
        data = result$value,
        source = item$id,
        params = list(action = "ggnetview_subgraph", select_module = input$module)
      )
      status(paste0("Rendered magnified subgraph for module ", input$module, "."))
    })
```
Then add the output (near the other `output$...` blocks):
```r
    output$magnified <- shiny::renderPlot({
      shiny::req(magnified_plot())
      magnified_plot()
    })
```

- [ ] **Step 6: Run the wiring test**

Run:
```bash
Rscript -e 'testthat::test_file("tests/testthat/test-shiny-files.R")'
```
Expected: PASS.

- [ ] **Step 7: Confirm the module still parses**

Run:
```bash
Rscript -e 'source("inst/app/modules/mod_graph_explorer.R"); cat("parse OK\n")'
```
Expected: `parse OK`.

- [ ] **Step 8: Commit**

```bash
git add inst/app/modules/mod_graph_explorer.R tests/testthat/test-shiny-files.R
git commit -m "feat(app): add magnified subgraph panel to graph explorer"
```

---

## Task 8: Wire adaptive side-by-side heatmap into environment links

**Files:**
- Modify: `inst/app/modules/mod_environment_links.R` (UI: second plot card)
- Modify: `inst/app/modules/mod_compare_environment.R` (server: compute + render adaptive)
- Modify: `tests/testthat/test-shiny-files.R`

- [ ] **Step 1: Write the failing wiring assertion**

Append to `tests/testthat/test-shiny-files.R`:
```r
test_that("environment links wires the adaptive heatmap variant", {
  ui_txt <- paste(readLines(test_path("../../inst/app/modules/mod_environment_links.R"), warn = FALSE), collapse = "\n")
  srv_txt <- paste(readLines(test_path("../../inst/app/modules/mod_compare_environment.R"), warn = FALSE), collapse = "\n")
  expect_match(ui_txt, "plot_adaptive", fixed = TRUE)
  expect_match(srv_txt, "safe_link_heatmap_adaptive", fixed = TRUE)
})
```

- [ ] **Step 2: Run to verify failure**

Run:
```bash
Rscript -e 'testthat::test_file("tests/testthat/test-shiny-files.R")'
```
Expected: the new test FAILs.

- [ ] **Step 3: Add the side-by-side plot card in the UI**

In `mod_environment_links_ui`, change the first `bslib::card` (the "Preview" card, lines 102-107) so the standard and adaptive plots sit side by side. Replace the single `plotOutput(ns("plot"), height = 650)` card with a two-column layout:
```r
      bslib::card(
        bslib::card_header("Preview — standard vs. adaptive tile sizing"),
        shiny::uiOutput(ns("compare_metrics")),
        bslib::layout_columns(
          col_widths = c(6, 6),
          shiny::plotOutput(ns("plot"), height = 650),
          shiny::plotOutput(ns("plot_adaptive"), height = 650)
        ),
        shiny::verbatimTextOutput(ns("status"))
      ),
```

- [ ] **Step 4: Add the adaptive reactiveVal and output in the server**

In `mod_compare_environment.R`, add near the other reactiveVals (line ~250):
```r
    plot_obj_adaptive <- shiny::reactiveVal(NULL)
```
Add the output near `output$plot` (line ~1007):
```r
    output$plot_adaptive <- shiny::renderPlot({
      shiny::req(plot_obj_adaptive())
      plot_obj_adaptive()
    })
```

- [ ] **Step 5: Compute the adaptive plot in the run_environment handler**

In the `observeEvent(input$run_environment, ...)` handler, immediately after the existing `plot_obj(result$value$plot)` assignment (the success path that sets the standard plot), add:
```r
      adaptive <- safe_link_heatmap_adaptive(env = env, spec = spec, params = params)
      if (adaptive$ok && !is.null(adaptive$value$plot)) {
        plot_obj_adaptive(adaptive$value$plot)
        register_plot_result(
          unique_output_name("environment_heatmap_adaptive_plot"),
          adaptive$value$plot,
          source_ids,
          c(params, list(variant = "adaptive"))
        )
      } else {
        plot_obj_adaptive(NULL)
      }
```
Note: use the same `env`, `spec`, `params`, and `source_ids` locals already in scope in that handler. Inspect the handler (starts ~line 572) to confirm those variable names before inserting.

- [ ] **Step 6: Run the wiring test**

Run:
```bash
Rscript -e 'testthat::test_file("tests/testthat/test-shiny-files.R")'
```
Expected: PASS.

- [ ] **Step 7: Parse-check both modules**

Run:
```bash
Rscript -e 'source("inst/app/modules/mod_environment_links.R"); source("inst/app/modules/mod_compare_environment.R"); cat("parse OK\n")'
```
Expected: `parse OK`.

- [ ] **Step 8: Commit**

```bash
git add inst/app/modules/mod_environment_links.R inst/app/modules/mod_compare_environment.R tests/testthat/test-shiny-files.R
git commit -m "feat(app): show standard vs adaptive environment heatmaps side by side"
```

---

## Task 9: Harden the four breaking changes

**Files:**
- Modify: `R/app_adapters.R` (or the relevant `safe_*` builder/plot adapter)
- Modify: `R/app_exports.R` (adjacency writer, if needed)
- Modify: `inst/app/modules/mod_visual_lab.R` (help text for q_outer/expand_outer)
- Test: `tests/testthat/test-new-functions.R` (append)

- [ ] **Step 1: Write tests for the multipartite + igraph validations**

Append to `tests/testthat/test-new-functions.R`:
```r
test_that("safe_magnified_subgraph rejects a wrong module count for multipartite sub_layout", {
  library(igraph)
  nodes <- data.frame(name = c("A","B","C","D"),
                      Modularity = factor(c("1","1","2","2")),
                      stringsAsFactors = FALSE)
  edges <- data.frame(from = c("A","B","C"), to = c("B","C","D"), stringsAsFactors = FALSE)
  g <- igraph::graph_from_data_frame(edges, directed = FALSE, vertices = nodes)

  # tripartite needs 3 modules; selecting 2 must fail gracefully, not crash.
  result <- safe_magnified_subgraph(g, select_module = c("1","2"),
                                    sub_layout = "tripartite_gephi_layout")
  expect_false(result$ok)
  expect_true(nzchar(result$message))
})
```

- [ ] **Step 2: Run to verify current behavior**

Run:
```bash
Rscript -e 'testthat::test_file("tests/testthat/test-new-functions.R")'
```
Expected: PASS (the `safe_call` wrapper already converts the new package's exact-module-count error into a friendly `app_failure`). If it does NOT pass because the error escapes, proceed to Step 3; otherwise this confirms hardening already holds for the subgraph path and you can skip to Step 4.

- [ ] **Step 3: Confirm the general plot adapter also traps the multipartite error**

Inspect `safe_plot_ggnetview` in `R/app_adapters.R`. Confirm it wraps the `ggNetView(...)` call in `safe_call` / `tryCatch` so the new "requires exactly N modules" error surfaces as `app_failure`. If it calls `ggNetView` outside a trap, wrap it:
```r
  safe_call(fn(graph_obj = graph, layout = layout, ...), "Failed to render network plot.")
```
Run the visual-lab-adjacent tests to confirm no regression:
```bash
Rscript -e 'testthat::test_file("tests/testthat/test-visualization.R")'
```
Expected: PASS.

- [ ] **Step 4: Verify build_graph_from_igraph module_attr error is trapped**

Inspect `safe_build_graph` / `safe_graph_builder` in `R/app_adapters.R` and `R/app_graph_builders.R`. Confirm builder calls run through `safe_call`/`tryCatch` so the new `build_graph_from_igraph` "module_attr not present" error becomes an `app_failure`. If already wrapped (it is for other builders), no change needed — note it in the commit message.

- [ ] **Step 5: Verify the adjacency exporter tolerates the new weight column**

Inspect `write_graph_adjacency_csv` and the edge writer in `R/app_exports.R`. The new `trans_adjacency_matrix_to_df()` returns `from, to, weight`. Confirm the writer does not assume exactly two columns (`from`, `to`). If it hard-codes column selection, change it to write all returned columns:
```r
  utils::write.csv(df, file = path, row.names = FALSE)
```
Run:
```bash
Rscript -e 'testthat::test_file("tests/testthat/test-app-exports.R"); testthat::test_file("tests/testthat/test-app-export-types.R")'
```
Expected: PASS.

- [ ] **Step 6: Update add_outer help text in the visual lab**

In `inst/app/modules/mod_visual_lab.R`, find the `q_outer` and `expand_outer` inputs and update their labels/help text to reflect the HDR reinterpretation, e.g. change help text to:
```r
        # add_outer now uses 2D KDE + Highest-Density-Region (HDR) contours.
        # q_outer = HDR mass to enclose (higher = tighter); expand_outer pads the contour.
```
(Keep the input names and defaults unchanged — behavior-only change.)

- [ ] **Step 7: Run the new-functions test file**

Run:
```bash
Rscript -e 'testthat::test_file("tests/testthat/test-new-functions.R")'
```
Expected: PASS.

- [ ] **Step 8: Commit**

```bash
git add R/app_adapters.R R/app_exports.R inst/app/modules/mod_visual_lab.R tests/testthat/test-new-functions.R
git commit -m "fix(app): harden multipartite/igraph/adjacency-weight/add_outer breaking changes"
```

---

## Task 10: Register new adapters in global preload + export center choices

**Files:**
- Modify: `inst/app/global.R`
- Modify: `R/app_exports.R` (if magnified result needs an export format entry)

- [ ] **Step 1: Add new helper names to the preload vector**

In `inst/app/global.R`, in the big `lapply(c(... ), load_app_helper)` vector, add:
```r
  "safe_link_heatmap_adaptive", "safe_magnified_subgraph",
```
(place near the other `safe_*` names, e.g. after `"safe_environment_triple_heatmap"`).

- [ ] **Step 2: Ensure the magnified result is exportable as PNG/PDF**

Inspect `export_formats_for_type()` in `R/app_exports.R`. The magnified subgraph is registered with `type = "result"` holding a ggplot. Confirm a ggplot-bearing `result` can be exported via `write_plot_png`/`write_plot_pdf`; if `result` type has no plot formats, register the magnified figure as `type = "plot"` instead in Task 7 Step 5 (change `type = "result"` to `type = "plot"`). Re-run:
```bash
Rscript -e 'testthat::test_file("tests/testthat/test-app-export-types.R")'
```
Expected: PASS.

- [ ] **Step 3: Commit**

```bash
git add inst/app/global.R R/app_exports.R inst/app/modules/mod_graph_explorer.R
git commit -m "chore(app): preload new adapters and allow magnified figure export"
```

---

## Task 11: Full test suite + app smoke test

**Files:** none (verification)

- [ ] **Step 1: Run the full test suite**

Run:
```bash
make test 2>&1 | tail -40
```
(or `Rscript -e 'devtools::test()'`). Expected: 0 failures. Investigate and fix any failure before proceeding.

- [ ] **Step 2: Smoke-launch the app and drive the two new paths**

Run:
```bash
Rscript -e 'shiny::runApp("inst/app", port = 8123, launch.browser = FALSE)' &
```
Then, via the browser/devtools driver: build a small graph in Graph Builder, open Graph Explorer → "Show magnified subgraph" (confirm the full+zoom figure renders), and open Environment Links → "Run environment link" (confirm both standard and adaptive plots render side by side). Stop the app afterward.

Expected: both new UI paths render without error; the status line shows success messages.

- [ ] **Step 3: Final verification commit (docs/news)**

Update `NEWS.md` with a short entry describing the adaptation, then:
```bash
git add NEWS.md
git commit -m "docs: note new-ggNetView adaptation (gglink_heatmaps_2, ggnetview_subgraph, breaking-change hardening)"
```

- [ ] **Step 4: Stop here — do not push**

Per the repo owner's standing rule, do NOT `git push`. Report the branch name and that it is ready for their review and push.

---

## Self-review notes

- **Spec coverage:** Part A → Tasks 2-4; B1 adapters → Tasks 5-6; B2 (side-by-side) → Task 8; B3 (magnified) → Task 7; B4 breaking changes → Task 9; global preload/tests → Tasks 10-11. All spec sections mapped.
- **Divergence risk:** Task 3 Steps 1-2 review every repo-only removed line; Task 4 locks the k_nn clamp with a regression test.
- **Naming consistency:** `safe_link_heatmap_adaptive`, `safe_magnified_subgraph`, `plot_obj_adaptive`, `plot_adaptive`, `magnified_plot`/`render_magnified` used consistently across tasks and tests.
