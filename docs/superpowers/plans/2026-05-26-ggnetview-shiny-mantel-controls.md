# Mantel Controls Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Expose direct Mantel method, distance, alternative, and permutation controls in Compare & Environment while routing table output to the appropriate real Mantel helper.

**Architecture:** Keep Mantel controls inside the existing Compare & Environment workflow. Add a small normalization helper for UI Mantel params, pass those params into `gglink_heatmaps()` / `gglink_heatmaps_2()` / `ggnetview_modularity_heatmaps()`, and route the "Run Mantel table" button to `mantel_block_vs_col()` or `mantel_pairwise()` based on `mantel_kind`.

**Tech Stack:** R, Shiny, testthat, shinytest2 smoke scripts, `/usr/local/bin/Rscript`.

---

## Files

- Modify: `R/app_compare_environment.R`
- Modify: `inst/app/global.R`
- Modify: `inst/app/modules/mod_compare_environment.R`
- Modify: `tests/testthat/test-app-compare-environment.R`
- Modify: `tests/run_shiny_analysis_export_smoke.R`
- Modify: `tests/run_shiny_phase2_workflow_smoke.R`
- Modify: `docs/ggnetview-new-package-shiny-audit.md`
- Modify: `docs/ggnetview-shiny-next-todos.md`

## Tasks

### Task 1: Lock Mantel Params With Tests

- [x] Add tests for `environment_mantel_params()`.
- [x] Add tests for `safe_mantel_table()` routing to block-vs-column and column-vs-column Mantel helpers.
- [x] Verify the tests fail before implementation.

Run:

```bash
/usr/local/bin/Rscript -e 'testthat::test_file("tests/testthat/test-app-compare-environment.R")'
```

Expected before implementation: FAIL because `environment_mantel_params()` and `safe_mantel_table()` do not exist.

### Task 2: Implement Mantel Params And UI

- [x] Add `environment_mantel_params()` in `R/app_compare_environment.R`.
- [x] Add `safe_mantel_table()` and make `safe_mantel_pairwise()` filter unsupported args.
- [x] Add UI controls for Mantel method, alternative, spec distance, env distance, and permutations.
- [x] Pass these params into environment link, manual heatmap, module heatmap, and Mantel table observers.

### Task 3: Browser Smoke And Docs

- [x] Extend browser smoke to assert the new controls exist and run Mantel with non-default params.
- [x] Update docs to mark Mantel helpers as direct controls rather than partial.
- [x] Run verification:

```bash
/usr/local/bin/Rscript -e 'testthat::test_file("tests/testthat/test-app-compare-environment.R")'
/usr/local/bin/Rscript tests/run_shiny_app_startup.R
/usr/local/bin/Rscript tests/run_shiny_analysis_export_smoke.R
/usr/local/bin/Rscript tests/run_shiny_phase2_workflow_smoke.R
git diff --check
```

### Task 4: Commit

- [x] Commit:

```bash
git add R/app_compare_environment.R inst/app/global.R inst/app/modules/mod_compare_environment.R tests/testthat/test-app-compare-environment.R tests/run_shiny_analysis_export_smoke.R tests/run_shiny_phase2_workflow_smoke.R docs/ggnetview-new-package-shiny-audit.md docs/ggnetview-shiny-next-todos.md docs/superpowers/plans/2026-05-26-ggnetview-shiny-mantel-controls.md
git commit -m "feat: add direct mantel controls"
```
