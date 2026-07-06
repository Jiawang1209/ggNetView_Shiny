# Adapt the `inst/app` Shiny app to the new ggNetView

**Date:** 2026-07-06
**Status:** Draft — awaiting author review
**Scope owner:** Yue Liu

## 1. Problem & goal

This repository (`ggNetView_Shiny`, package version 0.2.0) vendors a full copy of
the ggNetView core R source under `R/` (131 core files), plus Shiny-only helpers
(`R/app_*.R`, 16 files) and the app itself under `inst/app/`. The app reaches core
functions through a `resolve_ggnetview_function()` + `safe_*` adapter layer.

A newer line of the core package lives under `package/ggNetView`
(version 0.1.0, development). Relative to the repo's bundled core it:

- **Adds 2 exported functions**: `gglink_heatmaps_2()` (adaptive-tile-sized variant
  of `gglink_heatmaps()`) and `ggnetview_subgraph()` (full-network + magnified-module
  "local magnification" figure via `patchwork`).
- **Changes 12 core files** carrying new features and bug fixes (see NEWS: HDR-based
  module outer boundary, `generateMask_ggnetview()` `polygon_id` column, strict
  multipartite module-count validation, `build_graph_from_igraph` module-attr error,
  `trans_adjacency_matrix_to_df` now returns a `weight` column, boundary-`Pvalue`
  significance-bin fixes, and empty/single-element robustness).
- **Drops** `launch_ggNetView()` (the core package is no longer Shiny-aware).

**Goal:** adapt the existing Shiny app so it (a) runs against the new core code and
(b) exposes the two new functions — with minimal, well-isolated changes, reusing the
existing module/adapter architecture. No new top-level navigation, no module redesign,
no repackaging into the core package.

## 2. Key constraint discovered — the two source lines diverged

The new package is **not strictly newer**. The repo carries backend **audit fixes** the
new package lacks. Confirmed cases (both the `audit H1` `k_nn` clamp that prevents an
`FNN::get.knn` C-level ANN crash on small networks):

- `R/ggnetview.R` (line 484-485): repo has `k_nn_try <- min(k_nn, k_nn_cap)`; new package
  reverts to the unclamped `k_nn_try <- k_nn`.
- `R/get_geo_neighbors.R` (two spots): repo clamps
  `k_nn <- max(1L, min(as.integer(k_nn), nrow(layout) - 1L))`; new package removes both.

**Consequence:** the core sync must be a **reviewed per-file merge**, not a blind
overwrite. Blindly copying the 12 files would reintroduce the crash on small networks.

## 3. Design

### Part A — Merge the new core into the repo (reviewed, not blind)

For each of the 12 changed files + 2 new files:

1. **New files** (`gglink_heatmaps_2.R`, `ggnetview_subgraph.R`): copy verbatim from
   `package/ggNetView/R/`.
2. **12 changed files**: adopt the new-package version as the base, then **re-apply the
   repo's audit-H1 clamp fixes** into `ggnetview.R` and `get_geo_neighbors.R`. For the
   other 10, review each `diff` and confirm the repo-only removed lines (`<`) are the
   feature/logic the new package intentionally replaces — not an unrelated repo fix being
   dropped. Any repo-only fix found is preserved.
3. **NAMESPACE**: add `export(gglink_heatmaps_2)` and `export(ggnetview_subgraph)`.
   **Keep** `export(launch_ggNetView)` and `R/launch_ggNetView.R` — the Shiny distro
   needs it even though upstream core dropped it. This is the one deliberate divergence
   from upstream.
4. **man/**: copy the two new `.Rd` files from `package/ggNetView/man/`; regenerate any
   Rd affected by the 12 files if signatures changed (roxygen where available, else copy).
5. **DESCRIPTION**: keep the repo's Shiny deps (`shiny`, `bslib`, `DT`,
   `shinycssloaders`, `bsicons`, `jsonlite`) and version. No downgrade to 0.1.0.

### Part B — Adapt the app

**B1. New adapters** in `R/app_adapters.R`, mirroring `safe_environment_heatmap`
(`R/app_compare_environment.R`) and `safe_module_subgraph` (`R/app_graph_inspect.R`):

- `safe_link_heatmap_adaptive(env, spec, ...)` → resolves and calls `gglink_heatmaps_2`;
  returns the same 3-element list contract (straight plot / curved plot / stats df) via
  `app_result` / `app_failure`.
- `safe_magnified_subgraph(graph, select_module, ...)` → resolves and calls
  `ggnetview_subgraph`; returns the composed patchwork ggplot via the `safe_*` contract.
  Validates that the graph has a `Modularity` column and that `select_module` is a subset
  of its levels before calling (friendly failure otherwise).

**B2. `mod_environment_links`** (`inst/app/modules/mod_environment_links.R`): add a
**separate "Adaptive vs. standard" output panel** that renders both variants side by
side — the standard `gglink_heatmaps` result and the adaptive `gglink_heatmaps_2` result —
so the user can compare tile sizing directly. Both share the existing input controls
(`gglink_heatmaps_2` uses `@inheritParams gglink_heatmaps`, so no new inputs). The adaptive
variant is produced by `safe_link_heatmap_adaptive`; the standard one keeps using
`safe_environment_heatmap`. Both feed the export center.

**B3. `mod_graph_explorer`** (`inst/app/modules/mod_graph_explorer.R`): add a
**"Magnified subgraph"** output panel that reuses the existing `module` selector, calls
`safe_magnified_subgraph(item$data, select_module = input$module)`, renders the composed
full+zoom figure, and registers it so the export center can save it.

**B4. Breaking-change hardening** (surface friendly errors, never crash):

- **Multipartite layouts now require an exact module count.** In the visual-lab layout
  path, validate module count before offering / calling
  `bipartite|tripartite|quadripartite|pentapartite` layouts (2/3/4/5), and catch the
  new error via the existing `safe_plot_ggnetview` wrapper with a clear message.
- **`build_graph_from_igraph` errors on missing `module_attr`.** Add validation in the
  graph-builder path so a missing attribute is reported, not raised raw.
- **`trans_adjacency_matrix_to_df` now returns a `weight` column.** Verify the export
  center (`R/app_exports.R`, `write_graph_adjacency_csv` / edge writers) handles the
  extra column without breaking; adjust column selection if it assumed 2 columns.
- **`add_outer` is now HDR-based.** Update the visual-lab help text for `q_outer` /
  `expand_outer` to the reinterpreted HDR meaning (behavior-only change; API unchanged).

**B5. `inst/app/global.R`**: add any new `safe_*` helper names to the `load_app_helper`
preload vector so they resolve at app startup.

### 4. Testing

- Extend `tests/testthat/test-shiny-files.R` to assert both modules reference the new
  functions/adapters.
- Add `tests/testthat/test-new-functions.R`: unit-test `safe_link_heatmap_adaptive` and
  `safe_magnified_subgraph` on a small built graph — assert the success contract shape and
  a graceful `app_failure` on bad input (e.g. `select_module` not in levels).
- Add a regression test asserting the `k_nn` clamp survives the merge (small-network
  layout that would otherwise trip `FNN::get.knn`).
- Run `make test` (or `devtools::test()`), then smoke-launch the app (`app.R`) and drive
  the two new paths.

### 5. Out of scope (YAGNI)

New nav tabs; redesign of existing modules; moving the Shiny app into the core package;
adopting features from the 12 files beyond what the two new functions and the four
breaking changes require.

## 6. Risks

- **Silent repo-fix loss during merge** — mitigated by per-file diff review (Part A.2)
  and the k_nn regression test (§4).
- **`ggnetview_subgraph` layout coupling** — with multiple `select_module`s it uses
  multipartite layouts, which now enforce exact module counts; the adapter validates
  counts before calling.
