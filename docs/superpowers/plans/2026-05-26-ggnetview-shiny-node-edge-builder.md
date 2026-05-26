# Node + Edge Builder Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Expose `build_graph_from_node_edge()` as an advanced Graph Builder path so Shiny can build graph objects from separate node and edge tables while preserving isolated nodes.

**Architecture:** Keep this inside the existing Graph Builder workflow. Add a `node_table` registry type in Data Hub, route the new builder mode through `safe_graph_builder()`, and reuse existing graph registration/export behavior. The UI should use the primary source selector for the edge table and a secondary selector for the node table.

**Tech Stack:** R, Shiny, igraph/tidygraph, testthat, shinytest2 smoke scripts, `/usr/local/bin/Rscript`.

---

## Files

- Modify: `R/app_validation.R`
- Modify: `R/app_graph_builders.R`
- Modify: `R/app_gallery_presets.R`
- Modify: `inst/app/modules/mod_data_hub.R`
- Modify: `inst/app/modules/mod_graph_builder.R`
- Modify: `tests/testthat/test-app-input-types.R`
- Modify: `tests/testthat/test-app-graph-builders.R`
- Modify: `tests/run_shiny_graph_builder_modes_smoke.R`
- Modify: `docs/ggnetview-new-package-shiny-audit.md`
- Modify: `docs/ggnetview-shiny-next-todos.md`

## Tasks

### Task 1: Lock Behavior With Tests

- [x] Add input-type tests proving node tables are detected/accepted as `node_table`.
- [x] Add graph-builder tests proving `node_edge` calls `build_graph_from_node_edge()` and preserves an isolated node from the authoritative node table.
- [x] Run:

```bash
/usr/local/bin/Rscript -e 'testthat::test_file("tests/testthat/test-app-input-types.R")'
/usr/local/bin/Rscript -e 'testthat::test_file("tests/testthat/test-app-graph-builders.R")'
```

Expected before implementation: FAIL because `node_table` and `node_edge` are not yet wired.

### Task 2: Implement Adapter And Input Type

- [x] Add `node_table` upload choice/detection for tables with node/name/id identifiers plus optional node attributes.
- [x] Add `Node + edge table` graph-builder mode.
- [x] Require `edge_table` plus `node_table`.
- [x] Route adapter calls to the real `build_graph_from_node_edge()`.
- [x] Preserve existing module method parameters and registry metadata.

### Task 3: Wire Shiny UI

- [x] Add a node table selector to Graph Builder.
- [x] Include `node_edge` in builder choices for edge-table sources.
- [x] Build inputs from selected edge table and node table.
- [x] Keep result registration/export behavior unchanged.

### Task 4: Browser Smoke And Docs

- [x] Extend graph-builder modes smoke to build the node+edge path.
- [x] Update coverage docs from not exposed to covered.
- [x] Run:

```bash
/usr/local/bin/Rscript -e 'testthat::test_file("tests/testthat/test-app-input-types.R")'
/usr/local/bin/Rscript -e 'testthat::test_file("tests/testthat/test-app-graph-builders.R")'
/usr/local/bin/Rscript tests/run_shiny_app_startup.R
/usr/local/bin/Rscript tests/run_shiny_graph_builder_modes_smoke.R
git diff --check
```

### Task 5: Commit

- [x] Commit:

```bash
git add R/app_validation.R R/app_graph_builders.R R/app_gallery_presets.R inst/app/modules/mod_data_hub.R inst/app/modules/mod_graph_builder.R tests/testthat/test-app-input-types.R tests/testthat/test-app-graph-builders.R tests/run_shiny_graph_builder_modes_smoke.R docs/ggnetview-new-package-shiny-audit.md docs/ggnetview-shiny-next-todos.md docs/superpowers/plans/2026-05-26-ggnetview-shiny-node-edge-builder.md
git commit -m "feat: add node edge graph builder"
```
