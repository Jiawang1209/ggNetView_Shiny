# Environment Interpretation Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add report-oriented interpretation tables for environment, module-environment, and Mantel linkage results without creating new top-level Shiny tabs.

**Architecture:** Keep the existing Compare & Environment workflow. Add a pure helper in `R/app_compare_environment.R` that normalizes real ggNetView statistics into detail and summary tables, then show/register the summary through the existing Link Summary panel.

**Tech Stack:** R, Shiny, DT, testthat, shinytest2 smoke scripts, `/usr/local/bin/Rscript`.

---

## Files

- Modify: `R/app_compare_environment.R`
- Modify: `inst/app/global.R`
- Modify: `inst/app/modules/mod_compare_environment.R`
- Modify: `tests/testthat/test-app-compare-environment.R`
- Modify: `tests/run_shiny_analysis_export_smoke.R`
- Modify: `docs/ggnetview-new-package-shiny-audit.md`
- Modify: `docs/ggnetview-shiny-next-todos.md`

## Tasks

### Task 1: Lock Interpretation Helper With Tests

- [x] Add a focused test for `interpret_environment_links()` using synthetic correlation rows.
- [x] Verify the test fails before implementation because the helper does not exist.

Run:

```bash
/usr/local/bin/Rscript -e 'testthat::test_file("tests/testthat/test-app-compare-environment.R")'
```

### Task 2: Implement Helper And Shiny Handoff

- [x] Add `interpret_environment_links()` to normalize detail rows and summarize by environment block, species block, and method.
- [x] Expose the helper through `inst/app/global.R`.
- [x] Use the helper after environment link, manual heatmap, module heatmap, and Mantel table runs.
- [x] Register summary result objects for export when the summary has rows.

### Task 3: Browser Smoke And Docs

- [x] Extend analysis/export smoke to wait for the environment interpretation summary.
- [x] Update docs to describe report-level interpretation as covered.
- [x] Run verification:

```bash
/usr/local/bin/Rscript -e 'testthat::test_file("tests/testthat/test-app-compare-environment.R")'
/usr/local/bin/Rscript tests/run_shiny_app_startup.R
/usr/local/bin/Rscript tests/run_shiny_analysis_export_smoke.R
git diff --check
```

### Task 4: Commit

- [x] Commit:

```bash
git add R/app_compare_environment.R inst/app/global.R inst/app/modules/mod_compare_environment.R tests/testthat/test-app-compare-environment.R tests/run_shiny_analysis_export_smoke.R docs/ggnetview-new-package-shiny-audit.md docs/ggnetview-shiny-next-todos.md docs/superpowers/plans/2026-05-26-ggnetview-shiny-environment-interpretation.md
git commit -m "feat: add environment interpretation summaries"
```
