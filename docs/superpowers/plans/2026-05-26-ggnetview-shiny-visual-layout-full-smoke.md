# ggNetView Shiny Visual Layout Full Smoke Plan

Date: 2026-05-26

## Goal

Convert the Visual Lab layout smoke from representative family coverage to full preset coverage. Every layout exposed by `visual_lab_layout_choices()` should be exercised through the real Shiny browser path at least once.

## Scope

- Add a focused test that requires the visual layout smoke to source `mod_visual_lab.R` and enumerate `visual_lab_layout_choices()`.
- Replace the hard-coded five-layout smoke matrix with a helper that derives layout cases from the current UI choices.
- Use conservative module placement defaults for layouts that require ordered modules.
- Keep the browser smoke bounded to Visual Lab only; broader cross-tab workflows remain in existing smoke scripts.
- Update TODO documentation after verification.

## Verification

Run:

```bash
/usr/local/bin/Rscript -e 'testthat::test_file("tests/testthat/test-shiny-files.R")'
/usr/local/bin/Rscript tests/run_shiny_visual_layouts_smoke.R
/usr/local/bin/Rscript tests/run_shiny_app_startup.R
git diff --check
```
