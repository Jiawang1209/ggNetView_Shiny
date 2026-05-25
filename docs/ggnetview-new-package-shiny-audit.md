# ggNetView New Package and Shiny Coverage Audit

Original audit: 2026-05-25
Status refreshed: 2026-05-26

This note started as the first evidence-based pass over the newly added
`package/ggNetView/` source package and `package/ggNetView-manual/` manual. It
now records the current rebuilt Shiny coverage so the project does not regress
to the early five-tab prototype assumptions.

## Scope

- Source reference: `package/ggNetView/`
- Manual reference: `package/ggNetView-manual/`
- Current Shiny app: `inst/app/`
- Root package/API copy: `R/`, `DESCRIPTION`, `NAMESPACE`, `tests/`

## High-Level Finding

The project is now a manual-driven ggNetView Shiny workbench rather than an
independent Shiny wrapper package. The useful direction remains: copy and adapt
the authoritative `package/ggNetView/` API into the root `R/` package, then
organize Shiny around workflows instead of one tab per function.

The current app has moved well past the original prototype. It now has a shared
object registry, typed graph builders, graph inspection and subgraph workflows,
manual layout families, topology and keystone analysis, compare/environment
workflows, export helpers, gallery recipes, workflow replay preview, and task
feedback. Remaining work is depth, completeness, and polish across some
advanced manual paths, not basic viability.

## New Public API Surface

New exported functions in `package/ggNetView/NAMESPACE` that were absent from
the old root package export list:

| Function | Meaning for Shiny |
| --- | --- |
| `build_graph_from_consensus()` | Consensus builder fed by multiple adjacency matrices or graphs. |
| `build_graph_from_node_edge()` | Node+edge upload path that preserves isolated nodes. |
| `build_graph_from_stringdb()` | STRINGDB/PPI import workflow with score filtering. |
| `get_node_centrality()` | Per-node metric computation and ranking UI. |
| `get_node_ivi()` | Node influence analysis; depends on suggested package `influential`. |
| `get_sample_subgraph()` | Sample-wise subgraph extraction UI. |
| `mantel_block_vs_col()` | Supporting environment-linkage helper for advanced Mantel outputs. |
| `deg()` | Small angle helper; not a first-screen user workflow. |

New dependencies relative to the old root package:

- `Imports`: `rlang`
- `Suggests`: `dynamicTreeCut`, `influential`, `RobustRankAggreg`

## Same-Name Function Changes That Affect Shiny

| Function | Notable new arguments / behavior | Current Shiny state |
| --- | --- | --- |
| `ggNetView()` | Adds `label_layout`, `label_wrap_width`, `label_outer_pad`, and `bandwidth_scale`. | Visual Lab exposes the main layout families and common visual controls; exhaustive per-argument coverage still needs broader visual regression. |
| `gglink_heatmaps()` | Adds Mantel/correlation distance choices, collapse modes, significance mapping, group angle, and line styling. | Compare & Environment exposes the main heatmap and Mantel path; deeper block/multi-core controls remain. |
| `ggnetview_modularity_heatmaps()` | Adds module-level Mantel heatmap behavior. | Not yet a direct first-class Shiny workflow. |
| `ggNetView_multi()` | Adds `bandwidth_scale`. | Covered as grouped multi-network plot path with room for richer group controls. |
| `ggNetView_multi_link()` | Adds `bandwidth_scale`; manual chapter 07 relies on it. | Covered as a multi-network comparison path with remaining display-depth work. |

## Manual-Driven Workflow Map

| Manual chapter | Core workflow | Current Shiny coverage |
| --- | --- | --- |
| `01-create_graph_object.Rmd` | Build graphs from matrix, edge list, module annotation, adjacency, double matrix, igraph, WGCNA, consensus | Covered for matrix, RMT-fed matrix, edge table, adjacency, double matrix, multi matrix, WGCNA/TOM, and consensus. Direct igraph, node+edge, and STRINGDB paths remain candidates for advanced import. |
| `02-RMT.Rmd` | RMT threshold scan, then build graph with chosen threshold | Covered in Graph Builder and smoke-tested through the graph builder modes workflow. |
| `03-graph_info.Rmd` | Node/edge info, module subgraph info, sample subgraph info | Covered in Graph Explorer with graph info plus module/sample subgraph registration. |
| `04-subgraph.Rmd` | Extract module and sample subgraphs | Covered through module/sample subgraph workflows, registry handoff, plotting, topology, and export. |
| `05-layout.Rmd` | Full graph and subgraph visual layout gallery | Covered for major layout families and representative browser smoke. Exhaustive visual regression for every preset remains. |
| `06-network_topology.Rmd` | Network topology, sample topology, node centrality, IVI, Zi-Pi | Covered for global topology, robustness, centrality, IVI, and Zi-Pi. Direct parallel topology and dedicated sample-topology runners remain partial. |
| `07-network_compare.Rmd` | Multi-network comparison via `ggNetView_multi_link()` | Covered with comparison plotting and task feedback. Richer topology comparison displays and link-info tables remain. |
| `08-network_environment.Rmd` | Advanced `gglink_heatmaps()` with correlation/Mantel, multi-core, collapse modes | Covered for environment links, statistics, and Mantel helpers. Deeper multi-core/spec-block controls remain. |
| `09-multi-omics_netwotk.Rmd` | Multi-omics graph and plot workflows | Starter coverage through multi-matrix graph building and gallery/plot presets. More multi-omics-specific recipes remain. |
| `10-Gallery_of_Reproducible_Examples.Rmd` | Publication recipes and reusable parameter presets | Covered through gallery recipes, guarded reruns, manifest metadata, and workflow replay preview. Non-gallery imported object rerun support remains partial. |

## Function Coverage Matrix

Status definitions:

- `covered_shiny`: directly available in the current Shiny workflow.
- `covered_indirect`: used as a helper or reachable through another workflow.
- `partial`: present but still missing depth, controls, or direct UI affordances.
- `not_exposed`: useful API exists but is not a current first-class Shiny path.

| Function | Status | Manual evidence / note |
| --- | --- | --- |
| `build_graph_from_adj_mat` | covered_shiny | `01-create_graph_object.Rmd`; Graph Builder adjacency mode. |
| `build_graph_from_adj_mat_module` | covered_shiny | `01-create_graph_object.Rmd`; module-preserving adjacency path. |
| `build_graph_from_consensus` | covered_shiny | `01-create_graph_object.Rmd`; consensus builder mode. |
| `build_graph_from_df` | covered_shiny | `01-create_graph_object.Rmd`; edge table mode. |
| `build_graph_from_double_mat` | covered_shiny | `01-create_graph_object.Rmd`; double matrix mode. |
| `build_graph_from_igraph` | not_exposed | Useful for advanced import, but not a primary manual-driven Shiny path yet. |
| `build_graph_from_mat` | covered_shiny | Chapters 01, 02, 03, 04, 05, 06, 10; default matrix builder. |
| `build_graph_from_module` | covered_shiny | `01-create_graph_object.Rmd`; module annotation builder. |
| `build_graph_from_multi_mat` | covered_shiny | Multi-matrix graph builder and multi-omics starter path. |
| `build_graph_from_node_edge` | not_exposed | Candidate advanced import for isolated-node preservation. |
| `build_graph_from_stringdb` | not_exposed | Candidate advanced import; external data/runtime constraints need careful UX. |
| `build_graph_from_wgcna` | covered_shiny | Chapters 01, 10; WGCNA/TOM builder path. |
| `get_graph_adjacency` | covered_indirect | Used for object/export style workflows rather than as a standalone tab. |
| `get_info_from_graph` | covered_shiny | `03-graph_info.Rmd`; Graph Explorer info panels. |
| `get_network_topology` | covered_shiny | `06-network_topology.Rmd`; Topology module. |
| `get_network_topology_parallel` | partial | Parallel/list depth remains a follow-up. |
| `get_node_centrality` | covered_shiny | Node metric table and ranking workflow. |
| `get_node_ivi` | covered_shiny | Keystone/IVI workflow with graceful dependency handling. |
| `get_sample_subgraph` | covered_shiny | Chapters 03, 04; sample subgraph extraction and registry handoff. |
| `get_sample_subgraph_topology` | partial | Sample subgraph topology is reachable through saved subgraphs; direct runner remains. |
| `get_subgraph` | covered_shiny | Chapters 03, 04, 05; module subgraph extraction and visualization. |
| `gglink_heatmaps` | covered_shiny | `08-network_environment.Rmd`; environment linkage workflow. |
| `gglink_heatmaps_2` | covered_shiny | Advanced Compare & Environment path. |
| `gglink_heatmap_triple` | covered_shiny | Advanced environment/statistics path. |
| `ggNetView` | covered_shiny | Chapters 01, 05, 10; Visual Lab and Gallery. |
| `ggNetView_multi` | covered_shiny | Multi-network grouped plot path. |
| `ggNetView_multi_link` | covered_shiny | `07-network_compare.Rmd`; comparison path. |
| `ggNetView_RMT` | covered_shiny | `02-RMT.Rmd`; RMT graph builder path. |
| `ggnetview_modularity_heatmaps` | not_exposed | Useful advanced module-level environment workflow. |
| `ggnetview_zipi` | covered_shiny | `06-network_topology.Rmd`; Zi-Pi workflow. |
| `mantel_pairwise`, `mantel_between_blocks`, `mantel_block_vs_col` | partial | Mantel helpers are represented in environment workflows; finer block controls remain. |
| `trans_TOM_in_WGCNA` | covered_shiny | Chapters 01, 10; WGCNA/TOM builder path. |

## Current Evidence

Recent focused checks and browser smokes cover the rebuilt workflow surface:

- `tests/testthat/test-shiny-files.R`
- `tests/testthat/test-app-registry.R`
- `tests/testthat/test-graph-builder.R`
- `tests/testthat/test-graph-explorer.R`
- `tests/testthat/test-visual-lab.R`
- `tests/testthat/test-topology-results.R`
- `tests/testthat/test-compare-environment.R`
- `tests/testthat/test-export-center.R`
- `tests/run_shiny_app_startup.R`
- `tests/run_shiny_manual_workflow_smoke.R`
- `tests/run_shiny_phase2_workflow_smoke.R`
- `tests/run_shiny_graph_builder_modes_smoke.R`
- `tests/run_shiny_analysis_export_smoke.R`
- `tests/run_shiny_mobile_layout_smoke.R`
- `tests/run_shiny_visual_layouts_smoke.R`

## Remaining Implementation Risks

1. Visual coverage is representative, not exhaustive. Add broader visual
   regression for individual layout presets after the current workflow surface
   stabilizes.
2. Task feedback is broadly wired, but deliberately slow-action browser tests
   would make busy-state behavior more defensible.
3. Environment and multi-omics workflows still need deeper multi-core controls,
   richer presets, and finer environment/species block selections.
4. Multi-network comparison works, but richer comparison-group controls,
   topology comparison displays, and link-info tables remain valuable.
5. Export Center behavior is functional, but grouping and wording can be made
   clearer for publication users.
6. Gallery is usable for curated recipes and guarded reruns, but fuller replay
   for non-gallery imported objects remains a follow-up.
7. Direct advanced import paths for `build_graph_from_igraph()`,
   `build_graph_from_node_edge()`, and `build_graph_from_stringdb()` are not
   yet exposed.

## Architecture Direction

Keep the current workflow-level information architecture:

1. `Data Hub`: datasets, uploads, examples, and shared object registry.
2. `Graph Builder`: typed API-backed graph construction modes.
3. `Graph Explorer`: graph info, module subgraphs, sample subgraphs, and object handoff.
4. `Visual Lab`: `ggNetView()` layouts and publication-oriented visual controls.
5. `Topology and Keystone`: topology, robustness, centrality, IVI, and Zi-Pi.
6. `Compare and Environment`: multi-network, environment linkage, statistics, and Mantel helpers.
7. `Export Center`: object-aware downloads and workflow replay metadata.
8. `Gallery`: manual-derived recipes and guarded rerun paths.

Do not turn the remaining APIs into one tab per function. Add them as advanced
builder modes, advanced analysis panels, or gallery recipes when they support a
real user workflow.

## Repository Hygiene Findings

The newly added `package/` tree is large because `package/ggNetView-manual/`
contains rendered docs, generated figures, PDF, EPUB, `_book/`, and `.RData`.
This is useful as a local reference but still too heavy to treat as a runtime
dependency for the Shiny app.

Keep these rules:

- root `R/` remains the app-facing API copy;
- generated manual outputs should not become app runtime dependencies;
- commit source and app changes in small slices;
- keep verification evidence near the docs and tests so the next `/goal`
  session can resume without re-discovering the same state.
