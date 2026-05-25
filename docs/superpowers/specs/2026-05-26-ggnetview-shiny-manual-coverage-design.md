# ggNetView Shiny Manual Coverage Design

Date: 2026-05-26

## Purpose

The current Shiny app has reached the first usable loop: users can load a matrix, build a graph, inspect graph tables, draw a basic `ggNetView()` plot, compute basic topology, and export selected objects. The manual and package source show a much broader ggNetView workflow surface. This design records the gap and defines a Phase 2 scope that is useful, testable, and not too large.

The goal is not to make every manual chapter into a tab immediately. The next valuable step is to complete the input and graph-construction layer so later layout, topology, comparison, environment, and multi-omics modules have the right objects to work with.

## Evidence Sources

- Current Shiny modules: `inst/app/modules/mod_data_hub.R`, `mod_graph_builder.R`, `mod_graph_explorer.R`, `mod_visual_lab.R`, `mod_topology_results.R`, `mod_export_center.R`.
- Current app helpers: `R/app_adapters.R`, `R/app_registry.R`, `R/app_validation.R`, `R/app_exports.R`.
- Current handoff: `README.md`, `docs/ggnetview-shiny-next-todos.md`.
- Manual chapters: `package/ggNetView-manual/01-create_graph_object.Rmd` through `10-Gallery_of_Reproducible_Examples.Rmd`.
- Package vignettes: `package/ggNetView/vignettes/*.Rmd`.
- Root ggNetView API source copied into `R/`, including graph builders, RMT, topology, layout, multi-network, environment, and IVI helpers.

## Current Shiny Baseline

The Shiny app currently exposes these user-facing modules:

- `Data Hub`: upload or load example data; current detection supports matrix-like, adjacency-like, and edge-table-like objects.
- `Graph Builder`: build graph from matrix, adjacency matrix, or edge table; current matrix parameters are limited to `method = "cor"`, Pearson/Spearman, `proc = "none"`, thresholds, and module method.
- `Graph Explorer`: inspect nodes and edges from a graph object.
- `Visual Lab`: render `ggNetView()` using a small layout set: `nicely`, `fr`, `kk`, and `circle`, plus label options.
- `Topology Results`: run `get_network_topology()` on a graph object and display result tables.
- `Export Center`: export registry tables, objects, params, and plot formats only when the selected object is a plot.

This is enough for the first loop, but the manual describes many more input types and graph-building routes than the app can currently express.

## Manual Coverage Matrix

| Manual area | Main ggNetView API | Current Shiny support | Missing UI / input / output | Priority |
| --- | --- | --- | --- | --- |
| Matrix graph construction | `build_graph_from_mat()` | Partial. One matrix source, `cor`, Pearson/Spearman, thresholds, module method. | Transform method, WGCNA/Hmisc/SPARCC/SpiecEasi choices, method-specific parameters, node annotation, better threshold guidance, p-adjustment choices. | P0 |
| RMT threshold workflow | `ggNetView_RMT()`, then `build_graph_from_mat()` | Not supported. | RMT scan input, threshold result preview, threshold handoff into builder params, export of RMT table/plot. | P0 |
| Edge table graph construction | `build_graph_from_df()`, `build_graph_from_module()` | Partial. Basic edge table builder path exists through `build_graph_from_df()`. | Explicit edge schema validation, optional module table, source/target/weight column mapping, output warnings for missing weights/modules. | P0 |
| Adjacency graph construction | `build_graph_from_adj_mat()`, `build_graph_from_adj_mat_module()` | Partial. Basic adjacency builder exists. | Module/annotation attachment, matrix symmetry/diagonal diagnostics, signed/weighted interpretation, adjacency-specific export. | P0 |
| Double matrix graph construction | `build_graph_from_double_mat()`, `build_graph_from_double_mat_with_module()` | Not supported. | Two-matrix source selection, paired threshold controls, module input, output graph metadata linking both matrices. | P1 |
| Multi-matrix graph construction | `build_graph_from_multi_mat()` | Not supported. | Multi-select matrix inputs, block names, per-block or shared parameters, combined graph preview. | P1 |
| WGCNA/TOM import | `trans_TOM_in_WGCNA()`, `build_graph_from_wgcna()` | Not supported. | TOM upload/input type, expression matrix pairing, threshold/top-k controls, module colors, WGCNA-specific graph naming. | P1 |
| Consensus networks | `build_graph_from_consensus()`, `get_graph_adjacency()` | Not supported. | Multiple graph/adjacency selection, consensus method controls, node handling, binarization threshold/top-k, output adjacency export. | P2 |
| Graph information | `get_info_from_graph()` | Partial through custom node/edge extraction. | Manual-compatible graph info tables, summary cards, module-aware info, consistent export names. | P1 |
| Subgraph extraction | `get_subgraph()`, `get_sample_subgraph()` | Not supported. | Module selector, sample metadata selector, subgraph object registration, subgraph topology/plot handoff. | P1 |
| Layout gallery | `ggNetView()`, many `create_layout_*()` helpers | Partial. Four layouts exposed. | Manual layout families, module layouts, multipartite layouts, WGCNA/circular module layouts, layout presets, richer color/fill/edge controls. | P2 |
| Topology analysis | `get_network_topology()`, `get_sample_subgraph_topology()`, parallel variants | Partial. Basic graph-level topology exists. | Matrix-aware robustness/cohesion inputs, sample-level topology, parallel controls, long-running progress, richer exports. | P2 |
| Node importance / Zi-Pi | `get_node_centrality()`, `get_node_ivi()`, `ggnetview_zipi()` | Not supported. | Centrality/IVI runner, Zi-Pi input mapping, keystone node table, visual handoff. | P2 |
| Multi-network comparison | `ggNetView_multi()`, `ggNetView_multi_link()` | Not supported. | Multi-network object selection, layout group controls, link level, scale toggle, comparison plot export. | P3 |
| Environment links | `gglink_heatmaps()`, `gglink_heatmaps_2()`, `gglink_heatmap_triple()`, Mantel helpers | Not supported. | Environment matrix upload, node/module mapping, Mantel/drop-nonsig controls, heatmap plot output, combined-core workflow. | P3 |
| Multi-omics workflows | Multi-matrix builders, comparison, environment helpers | Not supported as a workflow. | Typed omics blocks, block-to-block edges, omics-aware presets, integrated plot/report. | P3 |
| Gallery examples | Manual gallery scripts and bundled data | Not supported. | Preset workflows, example datasets, one-click reproduce buttons, saved parameter recipes. | P3 |

Priority meanings:

- `P0`: blocks broad manual-aligned graph creation and should be Phase 2.
- `P1`: natural extension after graph creation is stable; may share helper work with Phase 2.
- `P2`: important analysis/visual depth but depends on stable graph objects.
- `P3`: larger workflow modules; should come after the object model and core builders are stable.

## Proposed Information Architecture

The next Shiny should stay workflow-oriented. Top-level tabs should represent user tasks, not individual low-level functions.

### 1. Data Hub

Purpose: import, classify, preview, validate, and register objects.

Supported registry types should become:

- `matrix`: abundance, expression, OTU, feature-by-sample, or sample-by-feature data.
- `annotation`: taxonomy, node annotations, WGCNA module colors, or other node metadata.
- `edge_table`: source-target-weight table.
- `adjacency`: square weighted or binary matrix.
- `module_table`: node-to-module table.
- `wgcna_tom`: TOM or TOM-like matrix.
- `sample_metadata`: sample grouping or sample covariates.
- `env_matrix`: environmental variables.
- `graph`: igraph graph objects.
- `plot`: ggplot or plot-like objects.
- `result`: topology, RMT, IVI, Zi-Pi, Mantel, or other table results.

### 2. Build Networks

Purpose: turn registered inputs into graph objects.

This should be the main Phase 2 tab. It should use subpages or an accordion, not many top-level tabs:

- Matrix builder
- RMT-assisted matrix builder
- Edge table builder
- Adjacency builder
- Double/multi-matrix builder
- WGCNA/TOM builder
- Consensus builder

Advanced parameters belong inside the relevant builder panel. Examples: SPARCC iteration controls, SpiecEasi method controls, RMT scan range, consensus binarization, and WGCNA top-k thresholds.

### 3. Inspect & Subset

Purpose: understand and derive graph objects.

This should combine the current Graph Explorer with manual-compatible graph info and subgraph extraction:

- node and edge tables;
- `get_info_from_graph()` output;
- module selector for `get_subgraph()`;
- sample selector for `get_sample_subgraph()`;
- derived graph registration.

### 4. Visual Lab

Purpose: produce publication plots with `ggNetView()`.

Visual Lab should remain a top-level tab because users will iterate on it repeatedly. Layout families can be grouped as subcontrols:

- force-directed and general layouts;
- geometric layouts;
- circular/module layouts;
- multipartite layouts;
- WGCNA and consensus layouts.

Large styling controls should be accordions: labels, nodes, edges, modules, legends, and export size.

### 5. Topology & Keystone

Purpose: quantify network structure.

This can combine:

- global topology;
- matrix-aware topology and robustness;
- sample-level topology;
- IVI and centrality;
- Zi-Pi and keystone classification.

Parallel execution controls should be advanced settings, not a top-level page.

### 6. Compare Networks

Purpose: compare multiple graph objects after users can build several networks.

This should wait until Phase 3 or later because it depends on stable multi-object graph construction.

### 7. Environment / Multi-omics

Purpose: link networks with environmental variables and multi-omics blocks.

This should be a later top-level workflow, not mixed into Graph Builder. It needs its own input semantics and result interpretation.

### 8. Export & Reports

Purpose: centralize downloads and reproducibility.

Export should become object-aware:

- graph: RDS, node table CSV, edge table CSV, adjacency CSV;
- plot: PNG, PDF, RDS, params JSON;
- result: CSV, RDS, params JSON;
- input data: normalized CSV and validation report;
- workflow: manifest JSON.

## Phase 2 Scope

Phase 2 should focus on typed inputs and Graph Builder completion. This is the highest leverage because every later manual chapter assumes the user can create the right graph object first.

### Phase 2 In Scope

- Expand upload classification and registry metadata for `annotation`, `module_table`, `wgcna_tom`, `sample_metadata`, and `env_matrix`.
- Add explicit builder modes for:
  - matrix graph with full method choices;
  - RMT-assisted matrix graph;
  - edge table graph with optional module/annotation input;
  - adjacency graph with optional module input;
  - double matrix graph;
  - multi-matrix graph;
  - WGCNA/TOM graph;
  - consensus graph from existing graphs or adjacency matrices.
- Add builder adapters in `R/app_adapters.R` or a new focused helper file so UI modules do not know ggNetView function details.
- Add method-specific validation before calling ggNetView APIs.
- Register all derived graphs with source IDs, params, and warnings.
- Extend export semantics for graph, result, and params objects.
- Add focused unit tests and one browser smoke that runs the Phase 2 graph-building paths on small bundled fixtures.

### Phase 2 Out of Scope

- Full layout gallery implementation.
- Full multi-network comparison UI.
- Full environment and multi-omics analysis UI.
- Report generation.
- Project save/restore.
- Full gallery example reproduction.

These are not rejected features. They become much easier after Phase 2 creates a reliable typed object registry and graph builder layer.

## Recommended Phase 2 UX

The Build Networks tab should have a single source selector area and a builder-mode accordion. The app should prevent irrelevant controls from appearing when the chosen builder cannot use them.

Recommended builder mode order:

1. Matrix
2. Matrix + RMT
3. Edge Table
4. Adjacency
5. Double Matrix
6. Multi Matrix
7. WGCNA/TOM
8. Consensus

For each mode, the UI should show:

- required inputs;
- optional metadata inputs;
- core parameters;
- advanced parameters inside a collapsed accordion;
- a dry-run validation summary;
- build status;
- registered graph output name.

## Acceptance Criteria

Phase 2 is complete when the following can be demonstrated:

- A normal matrix can build a graph through `cor`, WGCNA-style, Hmisc-style, SPARCC, or SpiecEasi modes when dependencies are available.
- RMT can generate a threshold result and pass the selected threshold into graph building.
- An edge table and an optional module table can produce a graph.
- An adjacency matrix and an optional module table can produce a graph.
- Two matrices can produce a double-matrix graph.
- Multiple matrices can produce a multi-matrix graph.
- A TOM-like matrix can produce a WGCNA graph.
- Multiple graphs or adjacency matrices can produce a consensus graph.
- Every graph output appears in Graph Explorer, Visual Lab, Topology Results, and Export Center.
- Each builder path has at least one unit test with a small fixture.
- One browser smoke verifies the main Phase 2 workflow does not regress the first usable loop.

## Next Implementation Goal Text

Use this as the next `/goal` when ready to implement:

```text
/goal 基于 docs/superpowers/specs/2026-05-26-ggnetview-shiny-manual-coverage-design.md 和 docs/superpowers/plans/2026-05-26-ggnetview-shiny-phase2-graph-builder.md，实施 Phase 2：补齐 typed input registry 和 Graph Builder。优先完成 matrix/RMT/edge table/adjacency/double-matrix/multi-matrix/WGCNA/consensus 的 UI、adapter、validation、registry metadata、export 和测试。全程使用 /usr/local/bin/Rscript 验证，不做 Phase 3 的 layout gallery、environment、multi-network comparison。
```
