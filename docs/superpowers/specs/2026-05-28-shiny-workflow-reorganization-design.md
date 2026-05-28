# Shiny Workflow Reorganization Design

## Goal

Reorganize the ggNetView Shiny app into clearer top-level workflows so optional and advanced analyses do not crowd the common path.

## Navigation

The target navigation order is:

`Introduction -> Manual -> Data Hub -> Graph Builder -> RMT Builder -> Graph Explorer -> Visual Lab -> Topology -> Network Compare -> Environment Links -> Export`

## Scope

- `Graph Builder` keeps standard graph construction and removes the dedicated RMT scan button from the common build panel.
- `RMT Builder` becomes a separate page for RMT threshold scans and RMT-assisted graph building from matrix inputs.
- `Network Compare` becomes a separate page for multi-network plotting, comparison links, topology comparison, and report presets.
- `Environment Links` keeps ecological/environment workflows: environment/spec links, Mantel tables, module heatmaps, manual heatmaps, and triple heatmaps.

## Implementation Notes

- Reuse existing Shiny server logic and adapter functions where possible.
- Split UI modules by workflow, not by backend function family.
- Keep the existing registry object contract unchanged.
- Add structure tests before production changes so the new navigation and module files are locked in.

## Verification

- Run `testthat::test_file("tests/testthat/test-shiny-files.R")`.
- Run `tests/run_shiny_app_startup.R`.
- Run at least one smoke touching network comparison or environment links after the split.
- Open the app locally and inspect the new top-level tabs.
