# ggNetView Shiny Next TODOs

Date: 2026-05-26

## Current Baseline

- The Shiny app runs from the root project with `/usr/local/bin/Rscript`.
- The example workflow can load the bundled matrix, build a graph, inspect objects, draw a plot, calculate topology, and reach export.
- Local source loading now keeps same-package helper dependencies and common ggNetView plotting/data-manipulation helper functions available.
- Export is object-aware: graph objects expose node, edge, and adjacency CSV exports; plot PNG/PDF buttons remain plot-only; workflow JSON manifests preserve registry provenance, params, warnings, and recipe metadata; the selected object summary shows type, source, supported formats, summary, and parameter keys. Export Center now groups selected-object downloads, graph/plot-specific downloads, and session/workflow downloads with explicit labels. Export Center can import a workflow JSON, preview restore/replay plans, choose snapshot conflict handling, select replay steps, identify supported gallery recipes, and rerun graph-builder outputs when the referenced source objects are restorable or present in the current registry.
- Data Hub auto-registers uploaded CSV/TSV/TXT files into the shared object registry and can load manual workflow examples for matrix, edge-table-with-module, adjacency, TOM-like, and starter graph workflows.
- Data Hub can run one-click gallery recipes that register final plot/result objects from those starters, including network layout, grouped comparison, graph info/topology, multi-network comparison, multi-omics network construction, multi-omics double-matrix and environment-block presets, environment heatmaps, collapsed-core and rotated arc collapsed-core environment heatmaps, and Mantel pairwise workflows.
- Data Hub can load sample metadata and Compare & Environment can use it for grouped matrix network plots.
- Graph Builder has real API-backed entry points for matrix, RMT-assisted matrix, edge table, node+edge table, STRINGDB/PPI table, igraph object, adjacency, double matrix, multi-matrix, WGCNA/TOM, and consensus graph construction. Build status now prints the builder mode and actual parameter JSON used by `Build graph`, and supported non-matrix builders keep the selected module method.
- Graph Explorer can register graph info, module subgraphs, and sample subgraphs.
- Visual Lab exposes major manual layout families and common `ggNetView()` parameters, including outline/geometric variants plus additional circular-module petal, square, star, diamond, and heart layouts that pass real `ggNetView()` smoke checks. Advanced Visual Lab controls now cover manual layout/rendering knobs such as node layers, rings, radius, shrink/inner-shrink, nearest-neighbor module placement, jitter, edge visibility/curves, edge alpha/color, point labels, and "Others" offsets. Visual Lab also has direct plot width/height controls plus local PNG/PDF downloads next to the plot preview.
- Topology can register global topology, direct parallel topology, robustness, sample-level topology/statistics, centrality, IVI, and Zi-Pi/keystone result tables.
- Topology result cards now expose local CSV downloads for topology, robustness, node metrics, sample topology, sample statistics, and sample robustness tables, so generated results can be saved from the same place they are inspected.
- Graph Builder, Visual Lab, Topology, Compare & Environment, and Gallery recipe execution now wrap real ggNetView calls with progress feedback and temporary action-button busy states.
- Compare & Environment can register multi-network comparison plots with optional pair restrictions, normalized link detail tables, pair-level link summaries, topology comparison summaries, environment-link plots/statistics with environment/spec block selectors, env/spec pair restrictions, multi-core geometry controls, module-level environment heatmaps, Mantel pairwise tables, and report-oriented interpretation summaries with domain labels, signal levels, narrative text, and caveats.
- Compare Networks now exposes the broader manual-backed multi-network group layouts, including row/column/geometric/snake/sine/cosine/center-pairs arrangements plus advanced rotation, anchor, row/column, sine-period, and label controls for real `ggNetView_multi_link()` and compatible grouped `ggNetView_multi()` paths.
- `tests/run_shiny_manual_workflow_smoke.R` now exercises the broad manual-backed backend workflow: gallery registration, graph/RMT/multi-matrix builders, graph info/subgraphs, Visual Lab layouts, topology/centrality/Zi-Pi/IVI boundary, multi-network comparison with custom sample metadata, environment links/triple heatmaps, Mantel, and object-aware exports. It also writes an ignored machine-readable smoke coverage JSON covering all ten manual areas.
- `tests/run_shiny_phase2_workflow_smoke.R` now runs a real shinytest2 browser path across Data Hub, Compare & Environment, Graph Builder, Graph Explorer, Visual Lab, Topology, Export graph-node/workflow-manifest downloads, workflow manifest replay preview, and gallery recipe execution.
- `tests/run_shiny_upload_to_builder_smoke.R` guards the human upload path: uploading a file in Data Hub immediately registers it, makes it selectable in Graph Builder without an extra Register click, builds a graph, and checks that changed build parameters are visible in the status output.
- `tests/run_shiny_graph_builder_modes_smoke.R` now runs real browser builds for RMT, matrix, edge+module, node+edge, STRINGDB/PPI, igraph object, adjacency+module, double matrix, multi-matrix, WGCNA/TOM, and consensus builder paths.
- `tests/run_shiny_analysis_export_smoke.R` now covers sample subgraphs, direct parallel topology, topology/node/sample CSV downloads, sample topology, centrality, IVI, Mantel, Visual Lab plot registration, plot width/height controls, and plot PNG/PDF downloads in a real browser.
- `tests/run_shiny_mobile_layout_smoke.R` now checks 390px-wide navigation across the main workflow tabs and guards against page-level horizontal overflow.
- `tests/run_shiny_visual_layouts_smoke.R` now draws every Visual Lab layout preset in a real browser, including general, geometric, circular-module, multipartite, WGCNA, dendrogram, and multirings layouts.
- `tests/run_shiny_visual_qa_polish_smoke.R` now guards the human visual QA path: Data Hub preview table fit, Visual Lab plot-first preview, collapsed parameters, no page-level horizontal overflow, and Topology/Export navigation after drawing.
- `tests/run_shiny_environment_geometry_smoke.R` now runs Gallery-backed environment geometry recipes in a real browser and checks Export plot controls for default heatmap, multi-omics blocks, collapsed-core, and rotated-arc collapsed-core outputs.
- `tests/run_shiny_task_feedback_smoke.R` now deliberately slows a real Shiny action and asserts the shared busy-state button class/disabled state appears and clears in the browser.
- `docs/ggnetview-shiny-release-evidence.md` can now be regenerated from the manual smoke coverage JSON, current git history, and the final validation command list. It summarizes manual-area coverage, release validation commands, recent commits, remaining limits, and next release steps.

## Remaining Gaps

1. Full browser-level workflow smoke.
   - Current smoke covers startup, the original core programmatic workflow, the broad manual-backed backend workflow, and a real browser click-through for the main Phase 2/3 path.
   - Current browser smoke also covers every major graph-builder mode.
   - Current browser smoke also covers sample subgraph registration, centrality/IVI buttons, a circular-module Visual Lab preset, Mantel button, local topology/node/sample CSV downloads, Visual Lab width/height controls, and plot PNG/PDF downloads.
   - Current browser smoke also covers mobile-width navigation, page-level overflow checks, and every individual Visual Lab layout preset.
   - Manual backend smoke now writes a coverage log that audits all ten manual areas.
   - Release evidence now has a generated Markdown summary that links the coverage log to the final validation checklist.

2. Long-running operation feedback.
   - Graph build, RMT scan, plot draw, topology, centrality, IVI, Zi-Pi, comparison, grouped multi-network, environment heatmaps, Mantel, and Gallery recipe paths now use shared progress feedback and temporary button busy states.
   - Browser smoke now asserts a deliberately slow task enters and exits the shared busy state.
   - Still needed: keep adding targeted busy-state assertions if future long-running actions add new button paths.

3. Environment and multi-omics depth.
   - The environment workflow now uses `gglink_heatmaps_2()`, original `gglink_heatmaps()`, `gglink_heatmap_triple()`, `ggnetview_modularity_heatmaps()`, block-vs-column Mantel controls, distance/permutation Mantel controls, Mantel pairwise/block helpers, and interpretation summaries that identify strongest/significant links by block and method. Gallery recipes can reproduce the heatmap, triple-heatmap, and Mantel starter paths.
   - Gallery recipes now include multi-omics starter paths that build multi-matrix and double-matrix graphs, plus a block-restricted multi-omics environment heatmap from two omics-like matrices.
   - Environment/spec block selectors, block-pair restrictions, multi-core geometry controls, direct heatmap style controls, inward heatmap distance, arc/rotation controls, and collapsed-core Gallery recipes are now exposed for heatmap workflows.
   - Browser smoke now covers the representative geometry matrix for default heatmap, multi-omics block heatmap, collapsed-core heatmap, and rotated-arc/inward-distance collapsed-core heatmap.
   - Environment and multi-omics environment outputs now register report-ready preset tables with block-pair evidence labels, domain labels, statistical signal levels, narrative text, and caveats for export.
   - Still needed: refine biological/statistical wording after longer real-use sessions and add project-specific report templates when target manuscript styles are known.

4. Multi-network comparison depth.
   - The comparison workflow now uses `ggNetView_multi_link()` with graph objects and `ggNetView_multi()` from a matrix plus generated or uploaded/custom group metadata.
   - Users can restrict comparisons to selected group pairs, which are passed to the real `comparisons_groups` argument.
   - Group layouts now include the manual's broader row/column/geometric/snake/sine/cosine/center-pairs arrangements with advanced layout controls.
   - Link information is normalized into detail and pair-level summary result tables, and graph-level topology comparison summaries are displayed and registered for export.
   - Multi-network comparison outputs now register report-ready presets that summarize shared node/module connections by comparison pair, link level, source/target diversity, layout distance, signal level, narrative text, and caveats.
   - Still needed: refine biological/statistical interpretation after longer real-use sessions and add project-specific labels for shared modules/nodes.

5. Polish layout and wording after real use.
   - Export Center buttons are now grouped into selected-object, type-specific, and session/workflow sections.
   - Selected object type/source/format metadata is now visible near the export controls.
   - Environment-link plot export now guards zero P values from producing non-finite line widths, with a focused PNG export regression test alongside the Visual Lab export smoke.
   - Visual Lab now uses a plot-first preview, collapses verbose plot parameters, uses explicit plot width/height controls, keeps controls visible by default, and preserves top navigation after drawing.
   - Data Hub preview tables now suppress unnecessary search/pagination chrome and fit their card without a visual horizontal scrollbar.
   - Still needed: continue polishing labels after longer real-use sessions, especially for future report/project-level exports.

6. Gallery completion.
   - Manual workflow examples are now loadable as starter objects, and one-click recipes can register final plot/result outputs for layout, grouped comparison, multi-network comparison, multi-omics graph construction, double-matrix omics construction, multi-omics environment links, collapsed-core/rotated-arc environment links, graph-info/topology, environment, triple heatmap, and Mantel manual areas.
   - Workflow JSON manifests now export registered objects with source IDs, graph-builder params, warnings, summaries, recipe metadata, and restorable data snapshots for table-like inputs/results.
   - Exported manifests can be re-imported as a replay plan preview, with supported gallery recipes and graph-builder outputs identified for guarded reruns.
   - Graph-builder replay now restores snapshotted non-gallery source inputs into empty sessions before rerunning supported graph-builder steps; missing-source failures remain explicit for unsupported or unsnapshotted objects.
   - Workflow restore now snapshots and restores otherwise unreplayable graph/plot objects while preserving replay-first behavior for graph-builder and gallery-recipe outputs.
   - Restore UX now includes snapshot/conflict summaries, explicit conflict handling, and selective replay.
   - Still needed: richer project-level restore flows for cross-session merge review and editable restored-object summaries after longer real-use sessions.

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
