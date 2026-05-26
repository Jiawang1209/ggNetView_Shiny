# Non-Gallery Workflow Replay Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Extend workflow manifest replay beyond gallery recipes so ordinary Graph Builder outputs can be rerun when their source objects are still available in the registry.

**Architecture:** Keep workflow export data-light: manifests continue to store provenance, params, and source IDs rather than serialized datasets. Add replay metadata for Graph Builder outputs, a helper that reconstructs builder inputs from current registry objects, and clear unsupported/missing-source statuses when rerun is not possible. Export Center keeps the same upload and button surface but runs both gallery recipes and supported graph-builder replay steps.

**Tech Stack:** R, Shiny, testthat, existing `safe_graph_builder()` adapter, `/usr/local/bin/Rscript`.

---

## Files

- Modify: `R/app_exports.R`
- Modify: `inst/app/modules/mod_graph_builder.R`
- Modify: `inst/app/modules/mod_export_center.R`
- Modify: `inst/app/global.R`
- Modify: `tests/testthat/test-app-export-types.R`
- Modify: `tests/run_shiny_phase2_workflow_smoke.R`
- Modify: `docs/ggnetview-shiny-next-todos.md`

## Tasks

### Task 1: Lock Replay Semantics With Tests

- [x] Add tests showing `workflow_replay_plan()` labels graph-builder outputs as `builder-output-needs-rerun`.
- [x] Add tests showing `workflow_replay_graph_builders()` reruns a matrix graph from current registry source data.
- [x] Add tests showing missing sources return a failure result with a clear message.
- [x] Run:

```bash
/usr/local/bin/Rscript -e 'testthat::test_file("tests/testthat/test-app-export-types.R")'
```

Expected before implementation: FAIL because graph-builder replay helpers and statuses do not exist.

### Task 2: Persist Graph Builder Replay Metadata

- [x] Add `graph_builder_registry_params()` in `mod_graph_builder.R`.
- [x] Store `builder`, `source_ids`, and optional `module_id` alongside existing ggNetView parameters when registering graph outputs.
- [x] Keep source IDs as character vectors so workflow JSON can preserve multi-input builders.

### Task 3: Implement Replay Helpers

- [x] Add raw-param helpers in `R/app_exports.R`.
- [x] Add `workflow_replay_builder_items()` to extract graph-builder candidates from a manifest.
- [x] Add `workflow_replay_graph_builder()` and `workflow_replay_graph_builders()` to reconstruct inputs and call `safe_graph_builder()`.
- [x] Register replayed graph outputs with source IDs, params, and replay warnings.

### Task 4: Wire Export Center

- [x] Change the replay action label from `Run Replay Recipes` to `Run Replay Plan`.
- [x] On replay, run supported gallery recipes and graph-builder candidates.
- [x] Report success/failure counts in `replay_status`.
- [x] Extend the Phase 2 browser smoke to expect graph-builder replay status.

### Task 5: Verify, Document, Commit

- [x] Run:

```bash
/usr/local/bin/Rscript -e 'testthat::test_file("tests/testthat/test-app-export-types.R")'
/usr/local/bin/Rscript -e 'testthat::test_file("tests/testthat/test-shiny-workflow-helpers.R")'
/usr/local/bin/Rscript tests/run_shiny_app_startup.R
/usr/local/bin/Rscript tests/run_shiny_phase2_workflow_smoke.R
git diff --check
```

- [x] Update `docs/ggnetview-shiny-next-todos.md` to describe graph-builder replay support and remaining data-light manifest limits.
- [x] Commit:

```bash
git add R/app_exports.R inst/app/modules/mod_graph_builder.R inst/app/modules/mod_export_center.R inst/app/global.R tests/testthat/test-app-export-types.R tests/run_shiny_phase2_workflow_smoke.R docs/ggnetview-shiny-next-todos.md docs/superpowers/plans/2026-05-26-ggnetview-shiny-non-gallery-replay.md
git commit -m "feat: replay graph builder workflows"
```
