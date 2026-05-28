# Shiny Workflow Reorganization Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Split optional RMT, network comparison, and environment-link workflows into dedicated top-level Shiny pages.

**Architecture:** Preserve the current registry and adapter contracts. Extract RMT UI/server behavior from `mod_graph_builder.R`; split the network-comparison and environment-link halves of `mod_compare_environment.R` into focused modules while retaining shared helper functions.

**Tech Stack:** R, Shiny, bslib, DT, testthat, existing ggNetView adapter helpers.

---

### Task 1: Lock In Navigation And File Shape

**Files:**
- Modify: `tests/testthat/test-shiny-files.R`
- Modify: `inst/app/ui.R`
- Modify: `inst/app/global.R`

- [ ] Write tests requiring `RMT Builder`, `Network Compare`, and `Environment Links` top-level tabs in the target order.
- [ ] Require module files for `mod_rmt_builder.R`, `mod_network_compare.R`, and `mod_environment_links.R`.
- [ ] Run `testthat::test_file("tests/testthat/test-shiny-files.R")` and confirm the new tests fail before implementation.

### Task 2: Extract RMT Builder

**Files:**
- Create: `inst/app/modules/mod_rmt_builder.R`
- Modify: `inst/app/modules/mod_graph_builder.R`
- Modify: `inst/app/global.R`

- [ ] Move RMT scan UI/server logic into `mod_rmt_builder.R`.
- [ ] Add RMT-assisted graph build controls to the new page.
- [ ] Remove the `Run RMT` button and `Matrix + RMT` builder choice from the common Graph Builder UI.

### Task 3: Split Compare And Environment

**Files:**
- Create: `inst/app/modules/mod_network_compare.R`
- Create: `inst/app/modules/mod_environment_links.R`
- Modify: `inst/app/modules/mod_compare_environment.R`
- Modify: `inst/app/server.R`

- [ ] Move compare-network controls, outputs, and observers into `mod_network_compare.R`.
- [ ] Move environment, Mantel, module heatmap, and triple heatmap controls into `mod_environment_links.R`.
- [ ] Leave `mod_compare_environment.R` as a compatibility wrapper only if needed by old tests.

### Task 4: Verify Runtime

**Files:**
- Modify tests only if they refer to old tab labels.

- [ ] Run `testthat::test_file("tests/testthat/test-shiny-files.R")`.
- [ ] Run `tests/run_shiny_app_startup.R`.
- [ ] Run a smoke covering the split compare/environment workflows.
- [ ] Open `http://127.0.0.1:7624/` and inspect the new nav tabs.
