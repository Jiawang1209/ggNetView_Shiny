# Workflow Restore UX Implementation Plan

Goal: make workflow manifest restore safer and more usable in Export Center by adding conflict handling, selective replay, and richer restore summaries.

Architecture: keep replay logic in `R/app_exports.R` and expose only workflow-level controls in `inst/app/modules/mod_export_center.R`. Restore should keep replay-first behavior for recipe and graph-builder outputs, while snapshotted inputs or unreplayable objects can be restored with explicit conflict policy.

Tasks:

- [x] Add failing tests for restore conflict policies, replay selectable steps, and restore summaries.
- [x] Implement restore planning, conflict-aware snapshot restore, and selectable replay helpers.
- [x] Wire Export Center controls for conflict policy and selected replay steps.
- [x] Update TODO docs and run focused/browser verification.
