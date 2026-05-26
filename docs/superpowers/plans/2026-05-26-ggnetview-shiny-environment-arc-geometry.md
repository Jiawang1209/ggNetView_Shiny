# ggNetView Shiny Environment Arc Geometry Implementation Plan

Date: 2026-05-26

## Goal

Expose the manual's `gglink_heatmaps()` collapsed-core geometry variants in Shiny instead of only the existing row/snake style. This slice covers `group_layout = "arc"`, `group_angle`, `group_arc_angle`, and inward heatmap offsets through real API calls.

## Scope

- Extend environment geometry parameter parsing to accept the API-supported `arc` group layout.
- Add numeric controls for group rotation and arc angle in Compare & Environment.
- Allow negative heatmap distance so users can reproduce the manual's inward collapsed-core example.
- Add a Gallery recipe that runs a rotated arc collapsed-core heatmap with real `gglink_heatmaps()`.
- Cover the behavior with focused unit tests and the Phase 2 browser smoke.

## Verification

Run:

```bash
/usr/local/bin/Rscript -e 'testthat::test_file("tests/testthat/test-app-compare-environment.R")'
/usr/local/bin/Rscript -e 'testthat::test_file("tests/testthat/test-app-gallery-presets.R")'
/usr/local/bin/Rscript tests/run_shiny_app_startup.R
/usr/local/bin/Rscript tests/run_shiny_phase2_workflow_smoke.R
git diff --check
```

