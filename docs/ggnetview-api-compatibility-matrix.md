# ggNetView API Compatibility Matrix for Shiny Rebuild

Date: 2026-05-25

This document narrows the audit to API compatibility between the current
root-level package and the newly added authoritative package at
`package/ggNetView/`. It focuses on functions that keep the same public name
but changed their argument surface, plus current Shiny call sites that will
need migration attention.

## Evidence Commands

The matrix is based on:

- Parsing exported function signatures from both `NAMESPACE` files and `R/`
  source trees.
- Searching current Shiny call sites under `ggNetView.shiny/R/` and
  `ggNetView.shiny/inst/app/`.
- Diffing key source files under root `R/` and `package/ggNetView/R/`.
- Confirming all 127 R files in `package/ggNetView/R/` parse successfully.

## Same-Name Signature Changes

Only five exported functions changed their formal argument list while keeping
the same name.

| Function | Added arguments | Removed arguments | Compatibility impact |
| --- | --- | --- | --- |
| `ggNetView()` | `label_layout`, `label_wrap_width`, `label_outer_pad`, `bandwidth_scale` | None | Existing calls should still run, but the Shiny UI misses important label and outer-boundary controls used by the new gallery examples. |
| `gglink_heatmaps()` | `spec_dist_method`, `env_dist_method`, `mantel_kind`, `permutations`, `spec_collapse`, `SigLineMid`, `link_color_by`, `link_width_by`, `NonsigLineColor`, `NonsigLineType`, `sig_threshold`, `group_angle`, `group_arc_angle` | None | Existing simple calls should still run, but the current Env-Spec tab exposes only the older reduced workflow and cannot reproduce much of manual chapter 08. |
| `ggnetview_modularity_heatmaps()` | `mantel_kind`, `spec_dist_method`, `env_dist_method`, `permutations` | None | Not currently exposed in Shiny; should be added as an advanced environment/module linkage panel. |
| `ggNetView_multi()` | `bandwidth_scale` | None | Not currently exposed in Shiny; lower priority than `ggNetView_multi_link()`. |
| `ggNetView_multi_link()` | `bandwidth_scale` | None | Not currently exposed in Shiny; manual chapter 07 makes this a first-class workflow. |

Because no arguments were removed, most old Shiny calls are likely source
compatible. The problem is product coverage and output interpretation, not
immediate call failure.

## Current Shiny Call Sites

Current app server calls:

| File | Line area | Function | Migration note |
| --- | --- | --- | --- |
| `ggNetView.shiny/inst/app/server.R` | build network | `ggNetView::build_graph_from_mat()` | Still valid. Should add RMT threshold handoff and expose more builder paths. |
| `ggNetView.shiny/inst/app/server.R` | build network | `ggNetView::build_graph_from_adj_mat()` | Still valid. Needs companion module-preserving adjacency builder. |
| `ggNetView.shiny/inst/app/server.R` | build network | `ggNetView::build_graph_from_df()` | Still valid for simple edge lists. New node+edge builder should handle isolated nodes. |
| `ggNetView.shiny/inst/app/server.R` | graph summary / zi-pi | `ggNetView::get_graph_nodes()` | Still valid. Should move into a general Graph Explorer. |
| `ggNetView.shiny/inst/app/server.R` | visualize | `ggNetView::ggNetView()` | Add controls for `label_layout`, `label_wrap_width`, `label_outer_pad`, `bandwidth_scale`. Defaults changed for `outerwidth` and `outerlinetype` in the new source, so visual regression checks are needed. |
| `ggNetView.shiny/inst/app/server.R` | topology | `ggNetView::get_network_topology()` | Still valid. Current UI only passes `graph_obj`, so matrix-dependent cohesion/robustness metrics remain unavailable. |
| `ggNetView.shiny/inst/app/server.R` | zi-pi | `ggNetView::ggnetview_zipi()` | Still valid. Should live beside centrality and IVI. |
| `ggNetView.shiny/inst/app/server.R` | env-spec | `ggNetView::gglink_heatmaps()` | Needs major upgrade for new Mantel, collapse, significance, line-mapping, and group-angle controls. |

## Function-Specific Migration Notes

### `ggNetView()`

New behavior is mostly visual and publication-facing:

- `label_layout` supports `two_column`, `two_column_follow`, and
  `label_circle`.
- `label_wrap_width` wraps long module labels.
- `label_outer_pad` controls the outer label-anchor ellipse.
- `bandwidth_scale` controls KDE/HDR-style outer boundary smoothing.
- New source changes default `outerwidth` from `1.25` to `1` and
  `outerlinetype` from `2` to `1`.

Shiny implication:

- Current Visual tab can keep its basic controls.
- Add a collapsible "Labels" panel with label layout, wrap width, label pad.
- Add an "Outer boundaries" panel with `bandwidth_scale`, `q_outer`,
  `expand_outer`, `outerwidth`, `outerlinetype`, `outeralpha`.
- Add screenshot-based visual regression checks for representative layouts,
  because default boundary styling changed.

### `gglink_heatmaps()`

The new function is no longer just a simple "environment/species heatmap-link"
wrapper. It now includes:

- Two Mantel modes through `mantel_kind`: `block_vs_col` and `col_vs_col`.
- Distance method controls for species and environment tables.
- `permutations` control for Mantel tests.
- `spec_collapse`, where each species block becomes one labelled point.
- User expression strings for `link_color_by` and `link_width_by`, parsed with
  `rlang`.
- Explicit significant and non-significant link layers controlled by
  `sig_threshold`, `NonsigLineColor`, and `NonsigLineType`.
- Diverging link color support through `SigLineMid`.
- Group positioning controls through `group_angle` and `group_arc_angle`.

Shiny implication:

- The current Env-Spec tab is too narrow and should be redesigned.
- Do not expose `link_color_by` and `link_width_by` as arbitrary text first
  without guardrails. Offer curated choices such as `Correlation`,
  `-log10(Pvalue)`, `abs(Correlation)`, then optionally an advanced expression
  field.
- Add separate modes:
  - Correlation links.
  - Mantel block-vs-column.
  - Mantel column-vs-column.
  - Collapsed species blocks.
- Return handling must support the list output: plot variants and link table.

### `ggnetview_modularity_heatmaps()`

This function is not used by the current Shiny app, but it is now aligned with
the expanded environment-linkage Mantel API. It should be treated as an
advanced mode after `gglink_heatmaps()` is stable.

Shiny implication:

- Requires an existing graph object, an environment table, and the original
  abundance matrix.
- Good fit for a "Module Environment" subtab under Environment Linkage.

### `ggNetView_multi_link()`

The formal change is small (`bandwidth_scale`), but the manual uses this
workflow heavily. The missing Shiny support is more important than the
signature delta.

Shiny implication:

- Needs a new Multi-Network Compare module.
- Inputs: matrix + sample metadata or a named list of pre-built graph objects.
- Controls: group layout, comparison pairs, link level, group scaling,
  orientation, outer group styling, node/module link styling.
- Output: assembled plot and `out$info` table.

### `get_network_topology()`

The signature did not change compared with the root package, but current Shiny
uses it in a reduced way:

- Current call passes only `graph_obj`.
- Manual chapter 06 explains that when `mat = NULL`, matrix-dependent metrics
  such as cohesion and robustness are `NA`.

Shiny implication:

- The Topology module should allow pairing a graph with its source matrix.
- If no matrix is available, the UI should state which metrics are unavailable.
- Add `get_sample_subgraph_topology()` and `get_network_topology_parallel()`
  paths later.

## New API That Should Be Added Before UI Polish

These functions are new or newly important and should influence the first
rebuild plan:

| Function | Why it matters |
| --- | --- |
| `ggNetView_RMT()` | Converts subjective correlation thresholds into a data-driven workflow; should feed Graph Builder. |
| `build_graph_from_consensus()` | Manual chapter 01 describes multi-method network fusion. |
| `build_graph_from_node_edge()` | Preserves isolated nodes, unlike simple edge-list builders. |
| `build_graph_from_stringdb()` | Adds PPI/STRINGDB import support. |
| `get_info_from_graph()` | Foundation for Graph Explorer tables. |
| `get_subgraph()` | Module-level subgraph extraction. |
| `get_sample_subgraph()` | Sample-level graph exploration, new in the package. |
| `get_node_centrality()` | Per-node importance metrics. |
| `get_node_ivi()` | IVI influence score; requires suggested package `influential`. |
| `ggNetView_multi_link()` | Main multi-network comparison workflow from manual chapter 07. |

## Risk Register

| Risk | Impact | Mitigation |
| --- | --- | --- |
| Current app remains source-compatible but semantically stale | Users see a GUI that cannot reproduce the new manual | Rebuild around manual modules, not old tabs. |
| `gglink_heatmaps()` advanced expression inputs can fail at runtime | User-facing errors or unsafe-feeling controls | Curated choices first; advanced expression editor later with validation. |
| Matrix-dependent topology metrics are silently `NA` | Users misread incomplete topology output | Pair graph objects with source matrices in an object registry. |
| Node influence depends on suggested packages | IVI or RRA workflows may fail on fresh installs | Detect optional dependencies and show install guidance. |
| Large `package/` tree contains rendered artifacts | Repo bloat and confusing source of truth | Decide integration strategy before committing/publishing. |

## Migration Priority

1. Object registry and data typing.
2. Graph Builder parity with manual chapter 01.
3. RMT threshold lab.
4. Graph Explorer and subgraph extraction.
5. Updated Visual Lab for new `ggNetView()` arguments.
6. Topology and node influence.
7. Multi-network comparison.
8. Updated environment linkage.
9. Gallery recipes and presets.

