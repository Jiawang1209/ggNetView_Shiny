# Module Environment Heatmap Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add a non-gallery module-level environment heatmap workflow using the real `ggnetview_modularity_heatmaps()` API.

**Architecture:** Keep the feature inside the existing Compare & Environment workflow. Add a focused adapter in `R/app_compare_environment.R` that prepares graph, environment, and OTU matrix inputs, then calls the real package function. The Shiny module adds a graph selector and action button in the existing Environment Links panel, registers both plot and stats objects, and uses the same export/registry model as other environment workflows.

**Tech Stack:** R, Shiny, ggplot2/igraph/ggNetView helpers, testthat, shinytest2 smoke scripts, `/usr/local/bin/Rscript`.

---

## Files

- Modify: `R/app_compare_environment.R`
- Modify: `R/ggnetview_modularity_heatmaps.R`
- Modify: `inst/app/modules/mod_compare_environment.R`
- Modify: `inst/app/global.R`
- Modify: `tests/testthat/test-app-compare-environment.R`
- Modify: `tests/run_shiny_phase2_workflow_smoke.R`
- Modify: `docs/ggnetview-new-package-shiny-audit.md`
- Modify: `docs/ggnetview-shiny-next-todos.md`

## Tasks

### Task 1: Lock Adapter Behavior With Tests

- [x] Add a test that `safe_module_environment_heatmap()` returns a plot and stats table for the Phase 2 matrix graph, OTU matrix, and environment table.
- [x] Add a test that invalid environment block/orientation combinations return a clear failure.
- [x] Run:

```bash
/usr/local/bin/Rscript -e 'testthat::test_file("tests/testthat/test-app-compare-environment.R")'
```

Expected before implementation: FAIL because `safe_module_environment_heatmap()` does not exist.

### Task 2: Implement Adapter

- [x] Resolve `ggnetview_modularity_heatmaps`.
- [x] Parse environment blocks with `parse_table_blocks()`.
- [x] Parse orientation and geometry options with existing `environment_geometry_params()`.
- [x] Validate that `env_select` length matches `orientation`.
- [x] Return `plot`, `curved_plot`, `stats`, `env_select`, `call_params`, and raw value.
- [x] Patch the copied API's single-column environment self-correlation path so one-column blocks work with `psych::corr.test()` behavior.

### Task 3: Wire Shiny UI

- [x] Add `module_graph_id` selector to the Environment Links panel.
- [x] Add `Run module heatmap` button.
- [x] Update graph-choice observer to populate `module_graph_id`.
- [x] On click, call `safe_module_environment_heatmap()` with selected graph, spec matrix, and env matrix.
- [x] Register plot/result objects with source IDs and params.

### Task 4: Verify Browser Path

- [x] Extend `tests/run_shiny_phase2_workflow_smoke.R` to click `run_module_environment`.
- [x] Wait for `Registered module environment heatmap:`.

### Task 5: Verify, Document, Commit

- [x] Run:

```bash
/usr/local/bin/Rscript -e 'testthat::test_file("tests/testthat/test-app-compare-environment.R")'
/usr/local/bin/Rscript tests/run_shiny_app_startup.R
/usr/local/bin/Rscript tests/run_shiny_phase2_workflow_smoke.R
git diff --check
```

- [x] Update `docs/ggnetview-shiny-next-todos.md`.
- [x] Commit:

```bash
git add R/app_compare_environment.R R/ggnetview_modularity_heatmaps.R inst/app/modules/mod_compare_environment.R inst/app/global.R tests/testthat/test-app-compare-environment.R tests/run_shiny_phase2_workflow_smoke.R docs/ggnetview-shiny-next-todos.md docs/ggnetview-new-package-shiny-audit.md docs/superpowers/plans/2026-05-26-ggnetview-shiny-module-environment-heatmap.md
git commit -m "feat: add module environment heatmap"
```
