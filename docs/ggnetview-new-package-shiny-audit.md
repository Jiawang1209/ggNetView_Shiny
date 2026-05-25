# ggNetView New Package and Shiny Rebuild Audit

Date: 2026-05-25

This note records the first evidence-based pass over the newly added
`package/ggNetView/` source package and `package/ggNetView-manual/` manual.
It is intended to guide the Shiny rebuild and to prevent the old GUI from
silently binding to obsolete assumptions.

## Scope

- New source package: `package/ggNetView/`
- New manual: `package/ggNetView-manual/`
- Existing Shiny package/app: `ggNetView.shiny/`
- Existing root package copy: current repository root (`R/`, `DESCRIPTION`,
  `NAMESPACE`, `tests/`)

## High-Level Finding

The newly added `package/ggNetView/` should be treated as the authoritative
next version of the package, not as a small patch over the root-level copy.
Compared with the root package, the new package changes 62 R source files,
adds about 4,390 lines, removes about 720 lines, and introduces several new
public workflows. The manual is also broader than the current Shiny app:
it describes graph construction, RMT thresholding, graph information,
module/sample subgraphs, layouts, topology, node influence, multi-network
comparison, environment linkage, and reproducible galleries.

The current Shiny app covers only a narrow subset of this surface. Rebuilding
the app should be manual-driven and module-based rather than incremental
control additions to the current five-tab interface.

## New Public API Surface

New exported functions in `package/ggNetView/NAMESPACE` that are absent from
the root package export list:

| Function | Meaning for Shiny |
| --- | --- |
| `build_graph_from_consensus()` | Needs a consensus-network builder fed by multiple adjacency matrices or graphs. |
| `build_graph_from_node_edge()` | Needs a node+edge upload path that preserves isolated nodes. |
| `build_graph_from_stringdb()` | Needs a STRINGDB/PPI import workflow with score filtering. |
| `get_node_centrality()` | Needs per-node metric computation and ranking UI. |
| `get_node_ivi()` | Needs node influence analysis; depends on suggested package `influential`. |
| `get_sample_subgraph()` | Needs sample-wise subgraph extraction UI. |
| `mantel_block_vs_col()` | Internal/supporting environment-linkage helper, useful for advanced Mantel outputs. |
| `deg()` | Small angle helper; probably not user-facing. |

New dependencies relative to the root package:

- `Imports`: `rlang`
- `Suggests`: `dynamicTreeCut`, `influential`, `RobustRankAggreg`

## Same-Name Function Changes That Affect Shiny

These exported functions keep their names but changed their argument surface.
The current Shiny app can keep calling them, but it does not expose new
capabilities and may miss important defaults or output variants.

| Function | Notable new arguments / behavior |
| --- | --- |
| `ggNetView()` | Adds `label_layout`, `label_wrap_width`, `label_outer_pad`, and `bandwidth_scale`. Gallery examples use label layout and group outer controls heavily. |
| `gglink_heatmaps()` | Adds `spec_dist_method`, `env_dist_method`, `mantel_kind`, `permutations`, `spec_collapse`, `SigLineMid`, `link_color_by`, `link_width_by`, `NonsigLineColor`, `NonsigLineType`, `sig_threshold`, `group_angle`, `group_arc_angle`. Current Shiny only exposes the older simple correlation/Mantel shell. |
| `ggnetview_modularity_heatmaps()` | Adds Mantel distance/permutation controls, matching the expanded environment-linkage logic. Current Shiny does not expose this function. |
| `ggNetView_multi()` | Adds `bandwidth_scale`. Current Shiny does not expose multi-network plots. |
| `ggNetView_multi_link()` | Adds `bandwidth_scale`; manual chapter 07 relies on this workflow. Current Shiny does not expose it. |

## Manual-Driven Workflow Map

The manual chapters imply these Shiny product modules:

| Manual chapter | Core workflow | Current Shiny coverage |
| --- | --- | --- |
| `01-create_graph_object.Rmd` | Build graphs from matrix, edge list, module annotation, adjacency, double matrix, igraph, WGCNA, consensus | Partial: matrix, adjacency, edge-dataframe only |
| `02-RMT.Rmd` | RMT threshold scan, then build graph with chosen threshold | Missing |
| `03-graph_info.Rmd` | Node/edge info, module subgraph info, sample subgraph info | Missing except basic graph summary |
| `04-subgraph.Rmd` | Extract module and sample subgraphs | Missing |
| `05-layout.Rmd` | Full graph and subgraph visual layout gallery | Partial: only basic `ggNetView()` controls |
| `06-network_topology.Rmd` | Network topology, sample topology, node IVI | Partial: topology and zi-pi only |
| `07-network_compare.Rmd` | Multi-network comparison via `ggNetView_multi_link()` | Missing |
| `08-network_environment.Rmd` | Advanced `gglink_heatmaps()` with correlation/Mantel, multi-core, collapse modes | Partial: older simplified heatmap-link UI |
| `09-multi-omics_netwotk.Rmd` | Placeholder/empty content | Defer until manual is completed |
| `10-Gallery_of_Reproducible_Examples.Rmd` | Publication recipes and reusable parameter presets | Missing |

## Function Coverage Matrix

Status definitions:

- `covered_current_shiny`: referenced by current Shiny app code.
- `manual_not_shiny`: used in the manual but not exposed in current Shiny.
- `api_not_manual_or_shiny`: exported or present in package API, but not currently
  used by either the manual or Shiny app.

| Function | Status | Manual evidence |
| --- | --- | --- |
| `build_graph_from_adj_mat` | covered_current_shiny | `01-create_graph_object.Rmd` |
| `build_graph_from_adj_mat_module` | manual_not_shiny | `01-create_graph_object.Rmd` |
| `build_graph_from_consensus` | manual_not_shiny | `01-create_graph_object.Rmd` |
| `build_graph_from_df` | covered_current_shiny | `01-create_graph_object.Rmd` |
| `build_graph_from_double_mat` | manual_not_shiny | `01-create_graph_object.Rmd` |
| `build_graph_from_igraph` | manual_not_shiny | `01-create_graph_object.Rmd` |
| `build_graph_from_mat` | covered_current_shiny | Chapters 01, 02, 03, 04, 05, 06, 10 |
| `build_graph_from_module` | manual_not_shiny | `01-create_graph_object.Rmd` |
| `build_graph_from_wgcna` | manual_not_shiny | Chapters 01, 10 |
| `get_graph_adjacency` | manual_not_shiny | `01-create_graph_object.Rmd` |
| `get_info_from_graph` | manual_not_shiny | `03-graph_info.Rmd` |
| `get_network_topology` | covered_current_shiny | `06-network_topology.Rmd` |
| `get_network_topology_parallel` | manual_not_shiny | `06-network_topology.Rmd` |
| `get_node_ivi` | manual_not_shiny | `06-network_topology.Rmd` |
| `get_sample_subgraph` | manual_not_shiny | Chapters 03, 04 |
| `get_sample_subgraph_topology` | manual_not_shiny | `06-network_topology.Rmd` |
| `get_subgraph` | manual_not_shiny | Chapters 03, 04, 05 |
| `gglink_heatmaps` | covered_current_shiny | `08-network_environment.Rmd` |
| `ggNetView` | covered_current_shiny | Chapters 01, 05, 10 |
| `ggNetView_multi_link` | manual_not_shiny | `07-network_compare.Rmd` |
| `ggNetView_RMT` | manual_not_shiny | `02-RMT.Rmd` |
| `trans_TOM_in_WGCNA` | manual_not_shiny | Chapters 01, 10 |

Notable exported APIs not yet used by the manual or current Shiny:

- `build_graph_from_node_edge()`
- `build_graph_from_stringdb()`
- `get_node_centrality()`
- `ggnetview_modularity_heatmaps()`
- `ggNetView_multi()`
- `build_graph_from_multi_mat()`
- `build_graph_from_pie()`
- `build_graph_from_enrichGO()`
- `gglink_heatmap_triple()`
- `gglink_heatmaps_2()`
- `mantel_pairwise()`, `mantel_between_blocks()`, `mantel_block_vs_col()`
- palette/theme/export helpers

These should not all become first-screen UI. They should be grouped into
advanced builders, advanced analyses, or developer/helper panels.

## Current Shiny App Gap

The current app calls only:

- `build_graph_from_mat()`
- `build_graph_from_adj_mat()`
- `build_graph_from_df()`
- `get_graph_nodes()`
- `ggNetView()`
- `get_network_topology()`
- `ggnetview_zipi()`
- `gglink_heatmaps()`

This means the current app is a useful prototype, but it is no longer aligned
with the new package/manual. The next app should not be framed as a five-tab
wrapper around old calls. It should be a guided workbench with persistent graph
objects and analysis modules.

## Recommended Rebuild Architecture

1. `Data Hub`
   - Built-in datasets, uploaded tables, RDS/RData graph objects.
   - Object typing: matrix, adjacency, edge list, node table, graph, sample metadata, environment/species table.
   - A shared object registry so later modules can reuse named objects.

2. `Graph Builder`
   - Matrix builder.
   - Edge-list builder.
   - Node+edge builder.
   - Adjacency builder.
   - Module-preserving builders.
   - WGCNA import builder.
   - Consensus builder.
   - STRINGDB/PPI builder.

3. `Threshold Lab`
   - RMT scan via `ggNetView_RMT()`.
   - Feed chosen threshold back into matrix builder.
   - Show threshold table and diagnostic plot outputs when available.

4. `Graph Explorer`
   - `get_info_from_graph()`.
   - `get_subgraph()` for module-level subgraphs.
   - `get_sample_subgraph()` for sample-level subgraphs.
   - Save selected subgraph into the object registry.

5. `Visual Lab`
   - Full `ggNetView()` parameter surface, but grouped into layout, node style,
     edge style, labels, outer boundaries, orientation, and export panels.
   - Include new label layout and `bandwidth_scale` controls.

6. `Topology and Influence`
   - `get_network_topology()` and parallel/list variants.
   - `get_sample_subgraph_topology()`.
   - `get_node_centrality()`.
   - `get_node_ivi()`.
   - `ggnetview_zipi()`.

7. `Multi-Network Compare`
   - `ggNetView_multi_link()` as the main workflow.
   - Support group metadata and pre-built graph object lists.
   - Expose link level, group layout, scaling, comparison pairs, and outer group controls.

8. `Environment Linkage`
   - Updated `gglink_heatmaps()` controls for correlation, Mantel, collapsed spec,
     multi-core/spec blocks, significance filtering, line color/width mapping,
     orientation, group angle, and layout.
   - Add `ggnetview_modularity_heatmaps()` as an advanced module-level variant.

9. `Gallery Recipes`
   - Store manual-derived presets for common figures.
   - Load example data, apply parameter bundles, and let users modify from there.

## Repository Hygiene Findings

The newly added `package/` tree is about 1.0 GB. `package/ggNetView-manual/`
is about 814 MB and includes rendered docs, generated figure outputs, PDF,
EPUB, `_book/`, and `.RData`. This is acceptable as a local drop-in reference
but too heavy as a direct dependency for the Shiny package.

Before integrating or committing broadly, decide one of:

- keep `package/` as a local, untracked reference source;
- promote only selected source files/manual chapters into this repository;
- use git submodules or separate repos for `ggNetView` and `ggNetView-manual`;
- vendor only the package source and keep rendered manual artifacts out.

Also note local/build artifacts in the new package tree:

- `.DS_Store`
- `.Rhistory`
- `.RData`
- `README.html`
- `src/*.o`
- `src/*.so`
- rendered manual docs/PDF/EPUB/_book outputs

## Immediate Next Steps

1. Decide how the authoritative new package will replace the root-level old
   package copy.
2. Build a Shiny object registry so modules can share matrices, graphs, and
   subgraphs without fragile global variables.
3. Add builder modules in this order: matrix, edge/adjacency, module-preserving,
   WGCNA, consensus, STRINGDB/node-edge.
4. Add RMT before advanced visualization because it feeds graph construction.
5. Add graph explorer before topology and plotting, so subgraphs become first-class objects.
6. Redesign environment linkage using the new `gglink_heatmaps()` surface rather
   than the current reduced parameter set.

