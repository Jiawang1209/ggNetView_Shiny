# ggNetView Shiny Redesign Spec

Date: 2026-05-25

## Purpose

This project should become the primary Shiny application for ggNetView. It is not a separate `ggNetView.shiny` add-on package. The Shiny app should use the newer ggNetView API currently under `package/ggNetView/R/` as the authoritative implementation, and the old root-level `R/` API can be replaced during implementation.

The first implementation milestone is a core workflow, not full manual coverage: users should be able to upload data, build a network, inspect it, draw a `ggNetView()` plot, calculate topology results, and export outputs.

## Confirmed Decisions

- The authoritative API source is `package/ggNetView/R/`.
- The old root-level `R/` implementation does not need compatibility support.
- The Shiny project should not continue as a separate `ggNetView.shiny/` package.
- The project should support both direct app execution and a formal `launch_ggNetView()` entry point.
- The first version should implement the core workflow only.

## Recommended Architecture

The repository should keep an R package style skeleton while behaving as the Shiny application itself.

```text
ggNetView_Shiny/
├── DESCRIPTION
├── NAMESPACE
├── app.R
├── R/
│   ├── launch_ggNetView.R
│   ├── ggNetView core API files copied from package/ggNetView/R/
│   ├── app_registry.R
│   ├── app_validation.R
│   └── app_exports.R
├── inst/
│   └── app/
│       ├── app.R
│       ├── global.R
│       ├── ui.R
│       ├── server.R
│       └── modules/
├── www/
├── inst/extdata/
└── docs/
```

Responsibilities should be separated clearly:

- `R/` contains ggNetView computational and plotting capabilities, app adapters, launch helpers, validation, registry utilities, and export helpers.
- `inst/app/` contains Shiny UI, server logic, and modules.
- `app.R` provides a direct development and deployment entry point.
- `launch_ggNetView()` provides a formal user-facing entry point.

The old `ggNetView.shiny/` structure should be treated as migration input rather than the long-term home of the app.

## First Milestone Scope

The first version should implement a stable core loop:

```text
Upload data
  -> validate and register object
  -> build graph
  -> inspect graph
  -> draw ggNetView plot
  -> calculate topology
  -> export plot, tables, and parameters
```

This milestone should not try to cover all manual workflows. Functions such as consensus networks, STRINGDB import, Mantel workflows, IVI analysis, sample subgraphs, and multi-network comparison should be reserved for later phases unless they are needed as lightweight extension points.

## Modules

### Data Hub

Data Hub handles file upload, type detection, validation, and object registration. The first version should support species or OTU matrices, correlation matrices, adjacency matrices, edge tables, and node tables.

For each uploaded object, the app should show a readable summary: object name, object type, dimensions, row and column names when useful, validation status, and warnings.

### Graph Builder

Graph Builder lets users select registered data objects and build graph objects with the new ggNetView API. The first version should prioritize:

- `build_graph_from_mat()`
- `build_graph_from_adj_mat()`
- `build_graph_from_df()`

It should reserve extension points for:

- `build_graph_from_consensus()`
- `build_graph_from_node_edge()`

Graph Builder should not own raw upload state. It should only consume objects from the registry and produce graph objects back into the registry.

### Graph Explorer

Graph Explorer shows what a selected graph contains. It should display node tables, edge tables, basic network statistics, search, and simple filters by node name, module, degree, or edge weight where available.

Graph Explorer should focus on inspection rather than publication-quality styling.

### Visual Lab

Visual Lab calls `ggNetView()` to generate the main network plot. The first version should include the parameters already used by the current Shiny app, then add selected new parameters from the newer API where they directly improve usability:

- label layout
- label wrap width
- label outer padding
- bandwidth scaling
- basic color and layout controls

Visual Lab should produce plot objects that can be registered, reused, and exported.

### Topology Results

Topology Results should call `get_network_topology()` and display global topology metrics in a downloadable table. Node-level metrics can be shown when already available from the graph object or from low-risk helper calls.

Advanced metrics such as IVI and Zi-Pi should be treated as later expansion modules unless they are trivial to expose without changing the milestone scope.

### Export Center

Export Center centralizes downloads. The first version should support:

- node table as CSV
- edge table as CSV
- topology table as CSV
- graph object as RDS
- plot as PNG
- plot as PDF
- parameter configuration as a small text or JSON file

Exports should record enough context for users to understand how an output was produced.

## Object Registry

The app should use a session-level object registry as the central exchange layer between modules.

Each registry item should have this shape:

```text
id: stable unique ID
name: user-readable name
type: matrix / adjacency / edge_table / node_table / graph / plot / result
data: actual R object
summary: dimensions, column names, node count, edge count, or other useful metadata
created_at: creation time
source: upload path or source object IDs
params: parameters used to create the object
warnings: non-blocking validation messages
```

The first version can implement the registry with `reactiveValues()` and small helper functions. It does not need a database. The registry should make it easy to add project save and restore later.

## Error Handling

Core API calls should be wrapped by lightweight adapters instead of putting `tryCatch()` logic throughout the UI.

Recommended adapters:

- `safe_build_graph()`
- `safe_plot_ggnetview()`
- `safe_topology()`

Each adapter should:

- validate input object types;
- catch R errors;
- convert technical errors into user-readable messages;
- preserve original error details in a session log;
- return a consistent result structure with `ok`, `value`, `message`, `warnings`, and `trace`.

User-facing messages should be grouped into three classes:

- blocking errors, such as unsupported files, duplicate IDs, non-numeric matrices, failed graph builds, or empty networks;
- non-blocking warnings, such as missing values, strict thresholds, sparse networks, or dense labels;
- runtime status, such as reading files, building networks, generating plots, and exporting files.

## Testing And Acceptance

The first version is done when a real or example dataset can complete the core loop from upload to export.

Testing should cover four layers:

1. API migration tests: migrated files from `package/ggNetView/R/` parse and load, key functions exist, and old root-level functions no longer override them.
2. Registry tests: object creation, lookup, deletion, summary generation, type checks, warning storage, and parameter recording.
3. Core workflow tests: upload small matrix, build graph, inspect nodes and edges, draw plot, calculate topology, and export CSV, RDS, PNG, and PDF.
4. Shiny smoke tests: app starts through direct app execution and through `launch_ggNetView()`, main tabs render, and key buttons do not immediately error.

Acceptance criteria:

- `launch_ggNetView()` opens the app.
- `shiny::runApp("inst/app")` or root `app.R` opens the app.
- An example matrix can build a network.
- The network can be inspected as node and edge tables.
- `ggNetView()` can generate a visible plot.
- Topology results can be calculated and downloaded.
- Graph, tables, plot, and parameters can be exported.
- The app no longer depends on the old `ggNetView.shiny/` package structure.

## Deferred Work

The following should be planned after the first milestone:

- consensus network builder;
- node and edge table builder;
- STRINGDB network import;
- Mantel and `gglink_heatmaps()` workflows;
- modularity heatmaps;
- IVI and Zi-Pi analysis;
- sample subgraph workflows;
- multi-network comparison;
- project save and restore;
- richer visual presets based on manual examples.

## Implementation Notes

Implementation should proceed in migration phases:

1. copy the new API from `package/ggNetView/R/` into root `R/`;
2. remove or replace old root-level API files that conflict with the new implementation;
3. consolidate dependency declarations in `DESCRIPTION`;
4. create direct and formal Shiny launch paths;
5. implement the registry and validation layer;
6. rebuild Shiny modules around the core workflow;
7. add export helpers;
8. verify with API, registry, workflow, and Shiny startup tests.

No implementation should start until this design is reviewed and approved.
