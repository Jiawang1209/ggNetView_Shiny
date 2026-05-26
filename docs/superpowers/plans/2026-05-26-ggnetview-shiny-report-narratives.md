# Report Narrative Templates Implementation Plan

Goal: deepen Compare & Environment report outputs so registered report preset tables include domain-specific labels, statistical interpretation, and concise caveats instead of only terse evidence strings.

Architecture: keep interpretation helpers in `R/app_compare_environment.R`; extend the existing report preset tables so the Shiny `Report Presets` panel and Export Center automatically receive richer report rows without adding new top-level UI.

Tasks:

- [x] Add failing tests for environment and multi-network narrative columns.
- [x] Implement reusable report narrative helpers and extend existing report preset tables.
- [x] Verify Shiny registration/export continues to use the enriched report tables.
- [x] Update TODO docs and run focused/browser verification.
