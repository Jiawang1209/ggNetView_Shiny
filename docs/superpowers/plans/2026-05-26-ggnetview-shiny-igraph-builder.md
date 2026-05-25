# Igraph Builder Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Expose `build_graph_from_igraph()` as an advanced Graph Builder path so existing graph objects can be standardized through the real ggNetView API while reusing module attributes when present.

**Architecture:** Keep this inside the existing Graph Builder workflow. Graph registry objects become eligible for an `Igraph object` builder mode. The adapter calls `build_graph_from_igraph()` and the UI registers the resulting standardized graph through the existing registry/export pipeline.

**Tech Stack:** R, Shiny, igraph/tidygraph, testthat, shinytest2 smoke scripts, `/usr/local/bin/Rscript`.

---

## Files

- Modify: `R/app_graph_builders.R`
- Modify: `inst/app/modules/mod_graph_builder.R`
- Modify: `tests/testthat/test-app-graph-builders.R`
- Modify: `tests/run_shiny_graph_builder_modes_smoke.R`
- Modify: `docs/ggnetview-new-package-shiny-audit.md`
- Modify: `docs/ggnetview-shiny-next-todos.md`

## Tasks

### Task 1: Lock Adapter Behavior With Tests

- [x] Add a test proving `graph_builder_modes()` includes `igraph`.
- [x] Add a test proving `safe_graph_builder("igraph", ...)` calls `build_graph_from_igraph()` and returns a graph with manual-compatible module attributes.
- [x] Run:

```bash
/usr/local/bin/Rscript -e 'testthat::test_file("tests/testthat/test-app-graph-builders.R")'
```

Expected before implementation: FAIL because `igraph` builder mode is not wired.

### Task 2: Implement Adapter And UI Routing

- [x] Add `Igraph object` builder mode.
- [x] Require a `graph` input and route it to `build_graph_from_igraph()`.
- [x] Preserve module-method params and default `use_existing_modules = TRUE`.
- [x] Include `igraph` in graph-source builder choices.
- [x] Register output through existing graph registration/exports.

### Task 3: Browser Smoke And Docs

- [x] Extend graph-builder modes smoke to build an igraph-standardized graph from a gallery graph.
- [x] Update coverage docs from not exposed to covered.
- [x] Run:

```bash
/usr/local/bin/Rscript -e 'testthat::test_file("tests/testthat/test-app-graph-builders.R")'
/usr/local/bin/Rscript tests/run_shiny_app_startup.R
/usr/local/bin/Rscript tests/run_shiny_graph_builder_modes_smoke.R
git diff --check
```

### Task 4: Commit

- [x] Commit:

```bash
git add R/app_graph_builders.R inst/app/modules/mod_graph_builder.R tests/testthat/test-app-graph-builders.R tests/run_shiny_graph_builder_modes_smoke.R docs/ggnetview-new-package-shiny-audit.md docs/ggnetview-shiny-next-todos.md docs/superpowers/plans/2026-05-26-ggnetview-shiny-igraph-builder.md
git commit -m "feat: add igraph graph builder"
```
