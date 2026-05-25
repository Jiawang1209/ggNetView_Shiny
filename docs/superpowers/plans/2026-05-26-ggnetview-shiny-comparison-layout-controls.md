# Comparison Layout Controls Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:test-driven-development for this focused feature slice.

**Goal:** Bring the Compare Networks workflow closer to the manual by exposing the broader `ggNetView_multi_link()` / `ggNetView_multi()` group-layout families and related layout parameters in Shiny.

**Architecture:** Keep Compare & Environment as one workflow tab. Add small helper functions for comparison layout choices and normalized layout params, wire those helpers into both graph-object comparison and grouped matrix multi-plot actions, and keep advanced controls in an accordion instead of creating new tabs.

**Tech Stack:** R, Shiny, testthat, shinytest2 smoke, existing `safe_multi_network_compare()` and `safe_multi_group_network()`, `/usr/local/bin/Rscript`.

## Tasks

- [x] Add failing tests for comparison group layout choices and parameter normalization.
- [x] Implement comparison layout helper functions in `inst/app/modules/mod_compare_environment.R`.
- [x] Add advanced Shiny controls and pass params to real `ggNetView_multi_link()` / `ggNetView_multi()` calls.
- [x] Update TODO docs and run focused/browser verification.
