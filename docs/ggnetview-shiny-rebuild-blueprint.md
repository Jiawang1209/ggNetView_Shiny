# ggNetView Shiny Rebuild Blueprint

Date: 2026-05-25
Status: Draft for review

This blueprint translates the new package/manual audit into a rebuild shape for
`ggNetView.shiny`. It does not prescribe implementation details line-by-line;
it defines the product modules, shared state model, migration phases, and
verification gates needed before code changes begin.

## Why Rebuild Instead of Patch

The current Shiny app is a useful prototype with five workflow tabs:

- Data
- Build Network
- Visualize
- Topology / zi-pi
- Env-Spec Linkage

The server currently keeps one active raw object and one active graph in a
single `reactiveValues()` object. That model breaks down for the new manual
because many workflows need several named objects at once:

- source matrix plus graph object for topology metrics;
- multiple adjacency matrices for consensus construction;
- module subgraphs and sample subgraphs as reusable graph inputs;
- RMT threshold output feeding graph construction;
- multi-network group outputs;
- environment/species table pairs;
- plot recipes derived from the manual gallery.

The rebuild should therefore start with object management and module
boundaries, not with more controls inside the current `ui.R` and `server.R`.

## Product Goal

Turn the Shiny app into a guided network-analysis workbench that mirrors the
new ggNetView manual:

1. Load and classify data objects.
2. Build graph objects through all major constructors.
3. Explore graph, module, sample, and node-level information.
4. Produce publication-ready plots through `ggNetView()`.
5. Run topology, role, and influence analyses.
6. Compare networks across groups.
7. Link networks to environmental variables.
8. Reuse manual/gallery recipes as presets.

## Source of Truth

The authoritative package source for future behavior is:

- `package/ggNetView/`

The authoritative workflow reference is:

- `package/ggNetView-manual/`

The existing root package and `ggNetView.shiny/` implementation are useful
legacy references, but the rebuilt app should target the new package/manual
surface.

## Shared Object Registry

The first architectural component should be a registry that stores named
objects and metadata. It can initially be a Shiny-side `reactiveValues()` list,
but it should have helper functions so modules do not directly mutate arbitrary
state fields.

### Object Kinds

Supported object kinds:

| Kind | Examples | Required metadata |
| --- | --- | --- |
| `matrix` | abundance matrix, adjacency matrix | dimensions, rownames present, colnames present, numeric flag |
| `data_frame` | edge list, annotation, sample metadata | columns, row count, likely role |
| `graph` | `tbl_graph`, `igraph` | node count, edge count, source object IDs |
| `node_annotation` | taxonomy table, module table | ID column, matched graph/matrix IDs |
| `env_table` | `Envdf_4st` or uploads | row count, column groups |
| `spec_table` | `Spedf` or uploads | row count, column groups |
| `rmt_result` | `ggNetView_RMT()` output | chosen threshold, source matrix ID |
| `plot` | ggplot output | source graph/result ID, function call |
| `table` | topology, node info, edge info | source object ID |
| `recipe` | manual-derived preset | target function, parameters, required objects |

### Registry Operations

Minimal operations:

- `register_object(name, value, kind, role, source_ids, notes)`
- `get_object(id_or_name)`
- `list_objects(kind = NULL, role = NULL)`
- `summarize_object(id_or_name)`
- `set_active_object(kind, id_or_name)`
- `append_result(source_id, result_id)`

### Why This Matters

This registry solves several current app limitations:

- topology can pair a graph with its source matrix;
- RMT output can feed Graph Builder;
- subgraphs can be plotted or analyzed as graph objects;
- consensus can gather multiple adjacency matrices;
- multi-network compare can use either a matrix + metadata or named graph list;
- gallery recipes can load required objects and parameters predictably.

## Rebuilt Module Map

### 1. Data Hub

Purpose:
Load, classify, preview, and register objects.

Inputs:

- built-in ggNetView datasets;
- uploaded CSV/TSV/TXT/RDS/RData;
- optional row-name column;
- optional role override.

Outputs:

- registered object;
- preview table;
- object summary;
- role recommendations such as abundance matrix, adjacency matrix, edge list,
  annotation, sample metadata, env table, species table.

Migration from current app:

- Replace `state$raw`, `state$raw_kind`, and `state$anno` with registry entries.
- Preserve `read_user_table()`, but add richer object classification.

### 2. Graph Builder

Purpose:
Create graph objects through the constructors described in manual chapter 01.

Builder modes:

- Matrix: `build_graph_from_mat()`
- Edge list: `build_graph_from_df()`
- Node + edge: `build_graph_from_node_edge()`
- Adjacency: `build_graph_from_adj_mat()`
- Adjacency with modules: `build_graph_from_adj_mat_module()`
- Edge/module table: `build_graph_from_module()`
- Double matrix: `build_graph_from_double_mat()`
- Existing igraph: `build_graph_from_igraph()`
- WGCNA import: `trans_TOM_in_WGCNA()` + `build_graph_from_wgcna()`
- Consensus: `build_graph_from_consensus()`
- STRINGDB/PPI: `build_graph_from_stringdb()`

Outputs:

- registered graph object;
- node/edge/module summary;
- function call preview;
- download `.rds`.

Migration from current app:

- Keep current matrix/adjacency/edge-list builders as first submodes.
- Add advanced submodes after registry and Graph Explorer are stable.

### 3. Threshold Lab

Purpose:
Make RMT thresholding a first-class workflow.

Function:

- `ggNetView_RMT()`

Inputs:

- matrix object;
- transform method;
- network/correlation method;
- RMT scan parameters;
- save diagnostic plots flag.

Outputs:

- registered RMT result;
- chosen threshold;
- threshold scan table/plot if present;
- action to send threshold into Graph Builder.

Migration note:

- This should happen before advanced plotting because it affects how users
  build the graph.

### 4. Graph Explorer

Purpose:
Inspect graph internals and extract reusable subgraphs.

Functions:

- `get_info_from_graph()`
- `get_subgraph()`
- `get_sample_subgraph()`
- `get_graph_nodes()`
- `get_graph_adjacency()`

Features:

- node table;
- edge table;
- module size table;
- module subgraph extraction;
- sample subgraph extraction with `union` / `intersect`;
- register selected subgraph as a new graph object.

Migration from current app:

- Move current graph summary and module table here.
- Keep Build Network focused on construction, not exploration.

### 5. Visual Lab

Purpose:
Expose the new `ggNetView()` plotting surface without overwhelming users.

Function:

- `ggNetView()`

Control groups:

- graph selection;
- layout and module placement;
- node aesthetics;
- edge aesthetics;
- labels;
- outer module boundaries;
- group outer boundary;
- orientation and scaling;
- export.

New controls required by the new package:

- `label_layout`
- `label_wrap_width`
- `label_outer_pad`
- `bandwidth_scale`

Outputs:

- rendered plot;
- registered plot object;
- layout data if `return_layout = TRUE`;
- PDF/PNG export.

Migration from current app:

- Keep the current Visual tab as the seed.
- Split controls into collapsible sections.
- Add visual regression checks for old vs new defaults.

### 6. Topology and Influence

Purpose:
Compute graph-level, sample-level, and node-level metrics.

Functions:

- `get_network_topology()`
- `get_network_topology_parallel()`
- `get_sample_subgraph_topology()`
- `get_sample_subgraph_topology_parallel()`
- `get_node_centrality()`
- `get_node_ivi()`
- `ggnetview_zipi()`

Features:

- graph-only topology;
- graph + source matrix topology;
- clear warning when matrix-dependent metrics are unavailable;
- centrality table;
- IVI table and top node ranking;
- zi-pi plot/table;
- optional dependency detection for `influential`.

Migration from current app:

- Current topology and zi-pi code moves here.
- Add source-matrix pairing through the registry.

### 7. Multi-Network Compare

Purpose:
Support manual chapter 07.

Primary function:

- `ggNetView_multi_link()`

Secondary function:

- `ggNetView_multi()`

Inputs:

- matrix + sample metadata; or
- named list of graph objects.

Controls:

- group order;
- comparison pairs;
- group layout;
- link level;
- node/module link styling;
- scaling;
- group outer boundary;
- orientation and anchor distance.

Outputs:

- assembled comparison plot;
- `out$info` table;
- registered plot/result.

### 8. Environment Linkage

Purpose:
Upgrade current Env-Spec tab to the new `gglink_heatmaps()` surface.

Functions:

- `gglink_heatmaps()`
- `ggnetview_modularity_heatmaps()`

Modes:

- correlation links;
- Mantel block-vs-column;
- Mantel column-vs-column;
- collapsed species blocks;
- multi-core/spec blocks;
- module-environment heatmaps.

Controls:

- env/spec object selection;
- env/spec block selectors;
- relation method;
- Mantel distance methods;
- permutations;
- `spec_collapse`;
- significance threshold;
- significant/non-significant line styling;
- curated link color/width mappings;
- orientation;
- group layout and angle.

Migration from current app:

- Reuse the current block selector idea.
- Replace arbitrary-only text controls with a safer hybrid: presets plus
  advanced expression fields.
- Keep list-output handling because `gglink_heatmaps()` returns plot variants
  and link data.

### 9. Gallery Recipes

Purpose:
Make manual examples reusable rather than static documentation.

Inputs:

- manual recipe selection;
- required dataset/object checks.

Outputs:

- pre-filled module parameters;
- runnable function call;
- rendered plot/result.

Initial recipes:

- microbial community network from chapter 10;
- WGCNA network recipe from chapter 10;
- RMT threshold recipe from chapter 02;
- multi-network comparison from chapter 07;
- environment linkage from chapter 08.

## Suggested File Structure

Keep `ggNetView.shiny/inst/app/app.R` as the entry point, but split logic:

```text
ggNetView.shiny/
  R/
    launch_ggNetView.R
    utils_io.R
    registry.R
    object_summary.R
    recipe_catalog.R
  inst/app/
    app.R
    global.R
    ui.R
    server.R
    modules/
      mod_data_hub.R
      mod_graph_builder.R
      mod_threshold_lab.R
      mod_graph_explorer.R
      mod_visual_lab.R
      mod_topology_influence.R
      mod_multi_network.R
      mod_environment_linkage.R
      mod_gallery_recipes.R
```

`ui.R` and `server.R` should become composition files. Most behavior should
move into module files.

## Migration Phases

### Phase 0: Source-of-Truth Decision

Decide how `package/ggNetView/` replaces the root package copy:

- copy new source into root package;
- keep as sibling package and make Shiny depend on it;
- use git submodule;
- keep as temporary local reference while rebuilding Shiny.

Do this before implementing UI behavior, because installed package resolution
must be predictable.

### Phase 1: Registry and Data Hub

Build registry helpers and Data Hub module. Preserve current upload/built-in
dataset behavior while adding object names, roles, and summaries.

Verification:

- built-in datasets register correctly;
- uploaded CSV/RDS/RData register correctly;
- graph object RDS is detected as graph;
- matrix/adjacency/edge-list classification is stable.

### Phase 2: Graph Builder Parity

Recreate current builders through modules and then add missing manual chapter
01 builders.

Verification:

- matrix, adjacency, and edge-list flows match current behavior;
- node+edge preserves isolated nodes;
- consensus accepts at least two adjacency matrices;
- WGCNA path can be represented even if demo files are optional.

### Phase 3: Explorer and RMT

Add RMT and Graph Explorer before deeper plotting.

Verification:

- RMT chosen threshold can populate `r.threshold`;
- `get_info_from_graph()` tables render;
- module and sample subgraphs become reusable graph objects.

### Phase 4: Visual Lab

Port current plot controls, add new label/boundary controls, and support saved
plot objects.

Verification:

- representative built-in graph renders;
- label layouts do not error;
- outer boundary smoothing controls alter output;
- PDF/PNG exports work.

### Phase 5: Topology and Influence

Move current topology/zi-pi logic into the new module and add matrix-aware
topology, centrality, and IVI.

Verification:

- graph-only topology explains unavailable metrics;
- graph+matrix topology returns full metrics;
- centrality attaches expected columns;
- IVI handles missing `influential` gracefully.

### Phase 6: Multi-Network and Environment Linkage

Add chapter 07 and chapter 08 workflows.

Verification:

- `ggNetView_multi_link()` renders from built-in `otu_sample`;
- environment linkage supports correlation and Mantel modes;
- collapsed species blocks render;
- link table exports.

### Phase 7: Gallery Recipes

Add manual-derived presets.

Verification:

- each recipe declares required objects;
- missing data produces actionable messages;
- recipe parameters can be edited before running.

## Testing Strategy

R-level tests:

- registry add/get/list/update behavior;
- object classifier behavior;
- recipe catalog validation;
- server helper functions that build function-call argument lists.

Shiny smoke tests:

- app object can be constructed with `ggNetViewApp()`;
- Data Hub loads `otu_rare_relative`;
- Graph Builder creates a graph from built-in matrix;
- Visual Lab renders a basic plot;
- Topology module computes graph-only topology;
- Env linkage renders a basic built-in `Envdf_4st` + `Spedf` plot.

Visual checks:

- representative `ggNetView()` screenshot;
- `gglink_heatmaps()` correlation screenshot;
- multi-network comparison screenshot after module is added.

## Open Decisions

1. Should `package/ggNetView/` replace the root package immediately, or should
   the Shiny rebuild target an installed package from `package/ggNetView/`?
2. Should rendered manual artifacts stay in this repo, or should the manual be
   referenced as source only?
3. Should advanced expression fields in `gglink_heatmaps()` be exposed in v1,
   or hidden behind a developer/advanced toggle?
4. Should the first rebuild milestone prioritize graph construction breadth or
   polished visualization depth?

## Recommended First Milestone

The first implementation milestone should be:

> Data Hub + object registry + matrix/adjacency/edge-list Graph Builder +
> Graph Explorer + basic Visual Lab, all targeting the new package.

This keeps the first milestone close enough to the current app to verify
quickly, while laying the foundation needed for RMT, subgraphs, topology,
multi-network comparison, and environment linkage.

