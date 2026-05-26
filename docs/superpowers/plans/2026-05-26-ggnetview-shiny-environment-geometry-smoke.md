# ggNetView Shiny Environment Geometry Smoke Plan

Date: 2026-05-26

## Goal

Add a focused browser smoke that exercises the environment and multi-omics geometry presets that are already exposed through the Gallery recipes. This closes the current visible coverage gap for environment heatmap geometry beyond the single representative row in the main Phase 2 workflow.

## Scope

- Add a file-level regression test that requires the new smoke script and named geometry recipe coverage.
- Create `tests/run_shiny_environment_geometry_smoke.R`.
- Drive the real Shiny UI through Data Hub gallery recipes for default environment heatmap, multi-omics environment blocks, collapsed-core heatmap, and rotated arc collapsed-core heatmap.
- Verify each recipe registers a plot and statistics object, and verify the plot can be selected in Export with PNG/PDF buttons available.
- Keep this smoke focused on Gallery-backed environment geometry; lower-level adapter unit tests already cover the parameter parsing details.

## Verification

Run:

```bash
/usr/local/bin/Rscript -e 'testthat::test_file("tests/testthat/test-shiny-files.R")'
/usr/local/bin/Rscript tests/run_shiny_environment_geometry_smoke.R
/usr/local/bin/Rscript tests/run_shiny_app_startup.R
/usr/local/bin/Rscript tests/run_shiny_phase2_workflow_smoke.R
git diff --check
```
