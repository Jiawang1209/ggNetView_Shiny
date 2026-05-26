# Smoke Coverage Log Implementation Plan

Goal: make final Shiny/manual verification auditable by writing machine-readable smoke coverage records for the manual-backed workflow.

Architecture: add a small coverage helper in `R/app_smoke_coverage.R`; keep smoke scripts responsible for recording the manual areas they actually exercise; write generated logs under ignored `tests/_smoke_coverage/`.

Tasks:

- [x] Add failing tests for manual area coverage manifest, JSON write/read, and required-area audits.
- [x] Implement coverage helper functions.
- [x] Wire manual workflow smoke to record all ten manual areas with direct evidence.
- [x] Run focused tests and manual workflow smoke.
- [x] Update TODO docs and commit.
