# Environment Style Controls Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Expose additional direct Compare & Environment controls for real `gglink_heatmaps()` and `gglink_heatmaps_2()` visual styling parameters.

**Architecture:** Keep the feature inside the existing Compare & Environment workflow. Extend `environment_geometry_params()` so UI observers pass validated style values through the existing safe adapters, then register those params with plot/result outputs.

**Tech Stack:** R, Shiny, testthat, shinytest2 smoke scripts, `/usr/local/bin/Rscript`.

---

## Files

- Modify: `R/app_compare_environment.R`
- Modify: `inst/app/modules/mod_compare_environment.R`
- Modify: `tests/testthat/test-app-compare-environment.R`
- Modify: `tests/run_shiny_phase2_workflow_smoke.R`
- Modify: `tests/run_shiny_environment_geometry_smoke.R`
- Modify: `docs/ggnetview-new-package-shiny-audit.md`
- Modify: `docs/ggnetview-shiny-next-todos.md`

## Tasks

### Task 1: Lock Parser And Adapter Behavior With Tests

- [x] Add a focused test showing `environment_geometry_params()` parses `ncol`, `HeatmapLabelSize`, `HeatmapSigSize`, `HeatmapPointSize`, `SigLineWidth`, `SigLineColor`, and `SigLineAlpha`.
- [x] Verify the test fails before implementation.

Run:

```bash
/usr/local/bin/Rscript -e 'testthat::test_file("tests/testthat/test-app-compare-environment.R")'
```

Expected before implementation: FAIL because the parser does not return the new style params.

### Task 2: Implement Parser And UI Controls

- [x] Extend `environment_geometry_params()` with optional validated style arguments.
- [x] Add Compare & Environment controls for columns, label/sig/point sizes, line width range, line colors, and line alpha.
- [x] Pass the new inputs into every direct environment heatmap path that already uses `environment_geometry_params()`.

### Task 3: Browser Smoke And Docs

- [x] Extend browser smoke to assert the new controls exist and drive a real environment workflow with non-default values.
- [x] Update audit/todo docs so direct non-gallery geometry controls mention the new style controls and narrow the remaining gap.
- [x] Run verification:

```bash
/usr/local/bin/Rscript -e 'testthat::test_file("tests/testthat/test-app-compare-environment.R")'
/usr/local/bin/Rscript tests/run_shiny_app_startup.R
/usr/local/bin/Rscript tests/run_shiny_phase2_workflow_smoke.R
/usr/local/bin/Rscript tests/run_shiny_environment_geometry_smoke.R
git diff --check
```

### Task 4: Commit

- [ ] Commit:

```bash
git add R/app_compare_environment.R inst/app/modules/mod_compare_environment.R tests/testthat/test-app-compare-environment.R tests/run_shiny_phase2_workflow_smoke.R tests/run_shiny_environment_geometry_smoke.R docs/ggnetview-new-package-shiny-audit.md docs/ggnetview-shiny-next-todos.md docs/superpowers/plans/2026-05-26-ggnetview-shiny-environment-style-controls.md
git commit -m "feat: add environment heatmap style controls"
```
