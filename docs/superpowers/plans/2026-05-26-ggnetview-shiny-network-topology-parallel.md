# Network Topology Parallel Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Expose `get_network_topology_parallel()` as a direct Topology & Keystone workflow option while preserving the current topology result display and export behavior.

**Architecture:** Keep the feature inside the existing Topology & Keystone tab. Extend `safe_topology()` so `params$parallel_api = TRUE` routes to the real `get_network_topology_parallel()` API. Add conservative UI controls for the global topology path: API toggle, worker count, and bootstrap count.

**Tech Stack:** R, Shiny, testthat, shinytest2 smoke scripts, `/usr/local/bin/Rscript`.

---

## Files

- Modify: `R/app_adapters.R`
- Modify: `inst/app/modules/mod_topology_results.R`
- Modify: `tests/testthat/test-app-adapters.R`
- Modify: `tests/run_shiny_analysis_export_smoke.R`
- Modify: `docs/ggnetview-new-package-shiny-audit.md`
- Modify: `docs/ggnetview-shiny-next-todos.md`

## Tasks

### Task 1: Lock Adapter Behavior With Tests

- [x] Add a test proving `safe_topology(..., parallel_api = TRUE, parallel = FALSE)` calls the parallel topology API in sequential mode and returns topology/robustness tables.
- [x] Run:

```bash
/usr/local/bin/Rscript -e 'testthat::test_file("tests/testthat/test-app-adapters.R")'
```

Expected before implementation: FAIL because `safe_topology()` ignores `parallel_api`.

### Task 2: Implement Adapter And UI

- [x] Route `safe_topology()` to `get_network_topology_parallel()` when requested.
- [x] Filter call args against function formals to keep both topology APIs stable.
- [x] Add global topology parallel API, bootstrap, and worker controls to Topology UI.
- [x] Register params that record whether the direct parallel API was used.

### Task 3: Browser Smoke And Docs

- [x] Extend analysis/export smoke to run the direct parallel topology path in sequential mode.
- [x] Update docs from partial to covered for `get_network_topology_parallel()` direct entry.
- [x] Run:

```bash
/usr/local/bin/Rscript -e 'testthat::test_file("tests/testthat/test-app-adapters.R")'
/usr/local/bin/Rscript tests/run_shiny_app_startup.R
/usr/local/bin/Rscript tests/run_shiny_analysis_export_smoke.R
git diff --check
```

### Task 4: Commit

- [ ] Commit:

```bash
git add R/app_adapters.R inst/app/modules/mod_topology_results.R tests/testthat/test-app-adapters.R tests/run_shiny_analysis_export_smoke.R docs/ggnetview-new-package-shiny-audit.md docs/ggnetview-shiny-next-todos.md docs/superpowers/plans/2026-05-26-ggnetview-shiny-network-topology-parallel.md
git commit -m "feat: add parallel network topology path"
```
