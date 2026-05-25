# ggNetView Shiny Next TODOs

Date: 2026-05-26

## Current Baseline

- The Shiny app runs from the root project with `/usr/local/bin/Rscript`.
- The example workflow can load the bundled matrix, build a graph, inspect objects, draw a plot, calculate topology, and reach export.
- Local source loading now keeps same-package helper dependencies and common ggNetView plotting/data-manipulation helper functions available.
- Export is object-aware: graph objects expose node, edge, and adjacency CSV exports; plot PNG/PDF buttons remain plot-only; workflow JSON manifests preserve registry provenance, params, warnings, and recipe metadata; the selected object summary shows type, source, supported formats, summary, and parameter keys. Export Center can import a workflow JSON, preview a replay plan, and identify supported gallery recipes for guarded reruns.
- Data Hub can load manual workflow examples for matrix, edge-table-with-module, adjacency, TOM-like, and starter graph workflows.
- Data Hub can run one-click gallery recipes that register final plot/result objects from those starters, including network layout, grouped comparison, graph info/topology, multi-network comparison, multi-omics network construction, multi-omics double-matrix and environment-block presets, environment heatmaps, and Mantel pairwise workflows.
- Data Hub can load sample metadata and Compare & Environment can use it for grouped matrix network plots.
- Graph Builder has real API-backed entry points for matrix, RMT-assisted matrix, edge table, adjacency, double matrix, multi-matrix, WGCNA/TOM, and consensus graph construction.
- Graph Explorer can register graph info, module subgraphs, and sample subgraphs.
- Visual Lab exposes major manual layout families and common `ggNetView()` parameters, including outline/geometric variants plus additional circular-module petal, square, star, diamond, and heart layouts that pass real `ggNetView()` smoke checks.
- Topology can register global topology, robustness, centrality, IVI, and Zi-Pi/keystone result tables.
- Graph Builder, Visual Lab, Topology, Compare & Environment, and Gallery recipe execution now wrap real ggNetView calls with progress feedback and temporary action-button busy states.
- Compare & Environment can register multi-network comparison plots with optional pair restrictions, normalized link tables, topology comparison summaries, environment-link plots/statistics with environment/spec block selectors, env/spec pair restrictions, multi-core geometry controls, and Mantel pairwise tables.
- `tests/run_shiny_manual_workflow_smoke.R` now exercises the broad manual-backed backend workflow: gallery registration, graph info/subgraphs, Visual Lab layouts, topology/centrality/Zi-Pi/IVI boundary, multi-network comparison with custom sample metadata, environment links/triple heatmaps, Mantel, and object-aware exports.
- `tests/run_shiny_phase2_workflow_smoke.R` now runs a real shinytest2 browser path across Data Hub, Compare & Environment, Graph Builder, Graph Explorer, Visual Lab, Topology, Export graph-node/workflow-manifest downloads, workflow manifest replay preview, and gallery recipe execution.
- `tests/run_shiny_graph_builder_modes_smoke.R` now runs real browser builds for RMT, matrix, edge+module, adjacency+module, double matrix, multi-matrix, WGCNA/TOM, and consensus builder paths.
- `tests/run_shiny_analysis_export_smoke.R` now covers sample subgraphs, centrality, IVI, Mantel, Visual Lab plot registration, and plot PNG/PDF downloads in a real browser.
- `tests/run_shiny_mobile_layout_smoke.R` now checks 390px-wide navigation across the main workflow tabs and guards against page-level horizontal overflow.
- `tests/run_shiny_visual_layouts_smoke.R` now draws representative Visual Lab layouts in a real browser across general, geometric, circular-module, multipartite, and WGCNA-style layout families.

## Remaining Gaps

1. Full browser-level workflow smoke.
   - Current smoke covers startup, the original core programmatic workflow, the broad manual-backed backend workflow, and a real browser click-through for the main Phase 2/3 path.
   - Current browser smoke also covers every major graph-builder mode.
   - Current browser smoke also covers sample subgraph registration, centrality/IVI buttons, a circular-module Visual Lab preset, Mantel button, and plot PNG/PDF downloads.
   - Current browser smoke also covers mobile-width navigation, page-level overflow checks, and representative Visual Lab layout families.
   - Still needed: broader visual-regression coverage for all individual layout presets.

2. Long-running operation feedback.
   - Graph build, RMT scan, plot draw, topology, centrality, IVI, Zi-Pi, comparison, grouped multi-network, environment heatmaps, Mantel, and Gallery recipe paths now use shared progress feedback and temporary button busy states.
   - Still needed: add deeper browser-level assertions for the busy-state handler on deliberately slow test actions.

3. Environment and multi-omics depth.
   - The environment workflow now uses `gglink_heatmaps_2()`, original `gglink_heatmaps()`, `gglink_heatmap_triple()`, block-vs-column Mantel controls, and Mantel pairwise helpers. Gallery recipes can reproduce the heatmap, triple-heatmap, and Mantel starter paths.
   - Gallery recipes now include multi-omics starter paths that build multi-matrix and double-matrix graphs, plus a block-restricted multi-omics environment heatmap from two omics-like matrices.
   - Environment/spec block selectors, block-pair restrictions, and multi-core geometry controls are now exposed for heatmap workflows.
   - Still needed: more visual examples that demonstrate how each multi-core geometry preset changes the rendered plot.

4. Multi-network comparison depth.
   - The comparison workflow now uses `ggNetView_multi_link()` with graph objects and `ggNetView_multi()` from a matrix plus generated or uploaded/custom group metadata.
   - Users can restrict comparisons to selected group pairs, which are passed to the real `comparisons_groups` argument.
   - Link information is normalized into a result table and graph-level topology comparison summaries are displayed and registered for export.
   - Still needed: more detailed link interpretation tables.

5. Polish layout and wording after real use.
   - Reduce ambiguity in Export Center buttons.
   - Selected object type/source/format metadata is now visible near the export controls.
   - Consider grouping controls into object-specific sections.

6. Gallery completion.
   - Manual workflow examples are now loadable as starter objects, and one-click recipes can register final plot/result outputs for layout, grouped comparison, multi-network comparison, multi-omics graph construction, double-matrix omics construction, multi-omics environment links, graph-info/topology, environment, triple heatmap, and Mantel manual areas.
   - Workflow JSON manifests now export registered objects with source IDs, params, warnings, summaries, and recipe metadata.
   - Exported manifests can be re-imported as a replay plan preview, with supported gallery recipes identified for guarded reruns.
   - Still needed: fuller rerun support for non-gallery imported objects.

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
