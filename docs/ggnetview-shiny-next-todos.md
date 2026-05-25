# ggNetView Shiny Next TODOs

Date: 2026-05-26

## Current Baseline

- The Shiny app runs from the root project with `/usr/local/bin/Rscript`.
- The example workflow can load the bundled matrix, build a graph, inspect objects, draw a plot, calculate topology, and reach export.
- Local source loading now keeps same-package helper dependencies and common ggNetView plotting/data-manipulation helper functions available.
- Export is object-aware: graph objects expose node, edge, and adjacency CSV exports; plot PNG/PDF buttons remain plot-only.
- Data Hub can load manual workflow examples for matrix, edge-table-with-module, adjacency, TOM-like, and starter graph workflows.
- Graph Builder has real API-backed entry points for matrix, RMT-assisted matrix, edge table, adjacency, double matrix, multi-matrix, WGCNA/TOM, and consensus graph construction.
- Graph Explorer can register graph info, module subgraphs, and sample subgraphs.
- Visual Lab exposes major manual layout families and common `ggNetView()` parameters.
- Topology can register global topology, robustness, centrality, IVI, and Zi-Pi/keystone result tables.
- Compare & Environment can register multi-network comparison plots, environment-link plots/statistics, and Mantel pairwise tables.
- `tests/run_shiny_manual_workflow_smoke.R` now exercises the broad manual-backed backend workflow: gallery registration, graph info/subgraphs, Visual Lab layouts, topology/centrality/Zi-Pi/IVI boundary, multi-network comparison, environment links, Mantel, and object-aware exports.
- `tests/run_shiny_phase2_workflow_smoke.R` now runs a real shinytest2 browser path across Data Hub, Compare & Environment, Graph Builder, Graph Explorer, Visual Lab, Topology, and Export graph-node downloads.

## Remaining Gaps

1. Full browser-level workflow smoke.
   - Current smoke covers startup, the original core programmatic workflow, the broad manual-backed backend workflow, and a real browser click-through for the main Phase 2/3 path.
   - Still needed: richer browser coverage for every graph-builder mode, sample subgraph registration, centrality/IVI buttons, Mantel button, plot PNG/PDF downloads, and mobile-width layout checks.

2. Long-running operation feedback.
   - Add progress/status indicators for graph build, plot draw, topology, comparison, environment, and Mantel calculations.
   - Disable action buttons while a calculation is running.
   - Keep the last successful result visible but clearly mark failed attempts.

3. Environment and multi-omics depth.
   - The first environment workflow uses `gglink_heatmaps_2()` and Mantel pairwise helpers.
   - Still needed: richer `gglink_heatmaps()` controls, `gglink_heatmap_triple()`, block-vs-column Mantel UI, multi-core options, and clearer multi-omics presets.

4. Multi-network comparison depth.
   - The first comparison workflow uses `ggNetView_multi_link()` with graph objects.
   - Still needed: `ggNetView_multi()` from matrix + group metadata, richer comparison-group controls, topology comparison display, and better link-info tables.

5. Polish layout and wording after real use.
   - Reduce ambiguity in Export Center buttons.
   - Make selected object type visible near the export controls.
   - Consider grouping controls into object-specific sections.

6. Gallery completion.
   - Manual workflow examples are now loadable as starter objects.
   - Still needed: one-click end-to-end reproducible gallery recipes that draw/export final figures for more manual chapters.

7. Add regression tests for issues found during manual use.
   - Local source dependencies for `build_graph_from_mat()`.
   - Empty source type in Graph Builder.
   - Plot downloads hidden for non-plot objects.
   - Graph node/edge/adjacency export behavior.
   - Comparison/environment adapter dependency behavior.
   - Sample subgraph matrix orientation and selected sample ID behavior.

## Suggested Next Work Session

Start with browser workflow smoke and UX stabilization, because the manual-backed backend workflow is now protected and the remaining regression risk is mostly cross-tab interaction and visible UI state.

Target outcome:

- A real browser can click through: load manual examples, build or select graphs, register graph info/subgraphs, draw Visual Lab plots, calculate topology/keystone tables, run comparison/environment workflows, and inspect Export Center options.
- Smoke logs record which manual areas were exercised.
- Known unsupported or partial APIs are listed in this file with exact failure reasons.
