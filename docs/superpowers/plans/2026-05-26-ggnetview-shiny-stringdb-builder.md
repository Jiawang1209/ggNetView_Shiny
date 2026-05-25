# STRINGDB Builder Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Expose `build_graph_from_stringdb()` as an advanced Graph Builder path for uploaded or example STRING-style PPI tables, with score filtering and real ggNetView API execution.

**Architecture:** Keep this inside the existing Graph Builder workflow. Add a `stringdb` registry type for data frames containing `node1`, `node2`, and `combined_score`, register a small gallery STRINGDB fixture, and route Graph Builder's STRINGDB mode through `safe_graph_builder()` to the real `build_graph_from_stringdb()`.

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

- [x] Add input-type tests proving STRING-style tables are detected/accepted as `stringdb`.
- [x] Add graph-builder tests proving `safe_graph_builder("stringdb", ...)` calls `build_graph_from_stringdb()` and preserves evidence-channel edge attributes.
- [x] Run:

```bash
/usr/local/bin/Rscript -e 'testthat::test_file("tests/testthat/test-app-input-types.R")'
/usr/local/bin/Rscript -e 'testthat::test_file("tests/testthat/test-app-graph-builders.R")'
```

Expected before implementation: FAIL because `stringdb` is not yet wired into app detection or graph builders.

### Task 2: Implement Adapter And Registry Type

- [x] Add `stringdb` upload choice/detection for `node1`, `node2`, and `combined_score` tables.
- [x] Add `STRINGDB/PPI table` graph-builder mode.
- [x] Require a `stringdb` input and call the real `build_graph_from_stringdb()`.
- [x] Expose score threshold and score column controls through the existing Graph Builder panel.
- [x] Register output through existing graph registration/exports.

### Task 3: Gallery And Browser Smoke

- [x] Register a small in-memory STRINGDB fixture in Gallery examples.
- [x] Extend graph-builder modes smoke to build the STRINGDB path.
- [x] Update coverage docs from not exposed to covered for imported STRING-style tables, with REST/service querying still documented as out of scope.
- [x] Run:

```bash
/usr/local/bin/Rscript -e 'testthat::test_file("tests/testthat/test-app-input-types.R")'
/usr/local/bin/Rscript -e 'testthat::test_file("tests/testthat/test-app-graph-builders.R")'
/usr/local/bin/Rscript -e 'testthat::test_file("tests/testthat/test-app-gallery-presets.R")'
/usr/local/bin/Rscript tests/run_shiny_app_startup.R
/usr/local/bin/Rscript tests/run_shiny_graph_builder_modes_smoke.R
git diff --check
```

### Task 4: Commit

- [x] Commit:

```bash
git add R/app_validation.R R/app_graph_builders.R R/app_gallery_presets.R inst/app/modules/mod_data_hub.R inst/app/modules/mod_graph_builder.R tests/testthat/test-app-input-types.R tests/testthat/test-app-graph-builders.R tests/run_shiny_graph_builder_modes_smoke.R docs/ggnetview-new-package-shiny-audit.md docs/ggnetview-shiny-next-todos.md docs/superpowers/plans/2026-05-26-ggnetview-shiny-stringdb-builder.md
git commit -m "feat: add stringdb graph builder"
```
