# Export Center Sections Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Make Export Center downloads unambiguous by grouping selected-object, type-specific, and workflow-level controls while preserving existing export IDs and handlers.

**Architecture:** Keep `mod_export_center.R` as the single Export Center module. Add small UI helper functions that return grouped `tagList()` controls, then reuse those helpers in the module UI and existing tests. No export handler IDs change, so existing browser smoke downloads remain compatible.

**Tech Stack:** R, Shiny, bslib, testthat, shinytest2 smoke scripts, `/usr/local/bin/Rscript`.

---

## Files

- Modify: `inst/app/modules/mod_export_center.R`
- Modify: `inst/app/www/styles.css`
- Modify: `tests/testthat/test-shiny-workflow-helpers.R`
- Modify: `tests/run_shiny_phase2_workflow_smoke.R`
- Modify: `docs/ggnetview-shiny-next-todos.md`

## Tasks

### Task 1: Lock Export Center Grouping With Tests

- [x] Add tests for `object_download_controls()`, `workflow_download_controls()`, graph-specific sections, and plot-specific sections.
- [x] Run:

```bash
/usr/local/bin/Rscript -e 'testthat::test_file("tests/testthat/test-shiny-workflow-helpers.R")'
```

Expected before implementation: FAIL because the new helper functions and section labels do not exist.

### Task 2: Implement Grouped Download Controls

- [x] Add `export_control_section()` for compact section markup.
- [x] Add `object_download_controls(ns = identity)` with existing IDs:
  - `download_rds`: `Download Object RDS`
  - `download_csv`: `Download Object CSV`
  - `download_params`: `Download Parameters JSON`
- [x] Add `workflow_download_controls(ns = identity)` with existing IDs:
  - `download_manifest`: `Download Session Manifest CSV`
  - `download_workflow_manifest`: `Download Workflow Manifest JSON`
- [x] Wrap graph controls under `Graph Downloads`.
- [x] Wrap plot controls under `Plot Downloads`.
- [x] Update `mod_export_center_ui()` to render the new helper sections.

### Task 3: Extend Browser Smoke Assertions

- [x] Add text assertions for:
  - `Selected Object Downloads`
  - `Graph Downloads`
  - `Session & Workflow Downloads`
  - `Download Workflow Manifest JSON`
- [x] Keep the existing download ID assertions unchanged.

### Task 4: Verify and Commit

- [x] Run:

```bash
/usr/local/bin/Rscript -e 'testthat::test_file("tests/testthat/test-shiny-workflow-helpers.R")'
/usr/local/bin/Rscript tests/run_shiny_app_startup.R
/usr/local/bin/Rscript tests/run_shiny_phase2_workflow_smoke.R
/usr/local/bin/Rscript tests/run_shiny_analysis_export_smoke.R
git diff --check
```

- [x] Update `docs/ggnetview-shiny-next-todos.md` to move Export Center ambiguity from remaining gap to baseline.
- [x] Commit:

```bash
git add docs/superpowers/plans/2026-05-26-ggnetview-shiny-export-center-sections.md docs/ggnetview-shiny-next-todos.md inst/app/modules/mod_export_center.R inst/app/www/styles.css tests/testthat/test-shiny-workflow-helpers.R tests/run_shiny_phase2_workflow_smoke.R
git commit -m "feat: group export center downloads"
```
