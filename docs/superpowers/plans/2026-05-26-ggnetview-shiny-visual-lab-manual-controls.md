# Visual Lab Manual Controls Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:test-driven-development for this focused feature slice.

**Goal:** Expose more manual-backed `ggNetView()` layout and rendering parameters in Visual Lab, especially controls needed by geometric, circular-module, multipartite, WGCNA, and line-rendering workflows.

**Architecture:** Keep the existing Visual Lab module. Extend `visual_lab_params()` with backward-compatible optional arguments, add corresponding Shiny controls, pass them into `safe_plot_ggnetview()`, and protect the parameter contract with focused helper tests plus an existing browser layout smoke.

**Tech Stack:** R, Shiny, testthat, shinytest2 smoke, existing ggNetView adapter, `/usr/local/bin/Rscript`.

## Tasks

- [x] Add failing tests for advanced Visual Lab layout/rendering parameters and normalization.
- [x] Extend `visual_lab_params()` with manual-backed numeric/logical/rendering controls.
- [x] Add Visual Lab UI inputs and server wiring for the new parameters.
- [x] Update TODO docs and run focused/browser verification.
