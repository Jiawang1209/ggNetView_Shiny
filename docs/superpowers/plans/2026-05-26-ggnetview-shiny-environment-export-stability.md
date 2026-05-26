# Environment Export Stability Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Make API-backed environment-link plots exportable as PNG/PDF even when real correlation/Mantel statistics contain zero P values.

**Architecture:** Fix the root plotting data in `gglink_heatmaps_2()` by mapping link linewidth through a finite `link_width_value` column. Keep Export Center generic, so plot downloads continue to use the normal object-aware export path.

**Tech Stack:** R, ggplot2, ggraph, Shiny export helpers, testthat, shinytest2 smoke scripts, `/usr/local/bin/Rscript`.

---

## Files

- Modify: `R/gglink_heatmaps_2.R`
- Modify: `tests/testthat/test-app-compare-environment.R`
- Modify: `docs/ggnetview-shiny-next-todos.md`

## Tasks

### Task 1: Lock The Export Failure

- [x] Add a focused test that builds a real `safe_environment_link()` plot from Phase 2 fixtures and writes it with `write_plot_png()`.
- [x] Verify the test fails before implementation with `'lwd' must be non-negative and finite`.

Run:

```bash
/usr/local/bin/Rscript -e 'testthat::test_file("tests/testthat/test-app-compare-environment.R")'
```

### Task 2: Fix The Root Plot Data

- [x] In `R/gglink_heatmaps_2.R`, create a finite `link_width_value` from `Pvalue` using `pmax(Pvalue, .Machine$double.xmin)`.
- [x] Replace both segment and curve linewidth aesthetics with `link_width_value`.

### Task 3: Verify And Commit

- [x] Run verification:

```bash
/usr/local/bin/Rscript -e 'testthat::test_file("tests/testthat/test-app-compare-environment.R")'
/usr/local/bin/Rscript tests/run_shiny_app_startup.R
/usr/local/bin/Rscript tests/run_shiny_analysis_export_smoke.R
git diff --check
```

- [x] Commit:

```bash
git add R/gglink_heatmaps_2.R tests/testthat/test-app-compare-environment.R docs/ggnetview-shiny-next-todos.md docs/superpowers/plans/2026-05-26-ggnetview-shiny-environment-export-stability.md
git commit -m "fix: stabilize environment plot exports"
```
