# Environment Report Presets Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:test-driven-development for this focused feature slice.

**Goal:** Add report-ready interpretation presets for environment and multi-omics environment workflows, so Shiny outputs are closer to the narrative/result tables users expect from the ggNetView manual examples.

**Architecture:** Keep the existing Compare & Environment tab. Extend the current interpretation helper so environment stats produce details, block summaries, and report preset rows. Register report presets as ordinary `result` objects for export and workflow manifests. Gallery recipes should also register the report preset for the multi-omics environment block workflow.

**Tech Stack:** R, Shiny, DT, testthat, existing ggNetView adapters, `/usr/local/bin/Rscript`.

## Tasks

- [x] Add failing tests for report preset helper output and gallery multi-omics report registration.
- [x] Implement report preset helper in `R/app_compare_environment.R`.
- [x] Show/register report presets in `inst/app/modules/mod_compare_environment.R`.
- [x] Register gallery multi-omics environment report presets in `R/app_gallery_presets.R`.
- [x] Update TODO docs and run focused/browser verification.
