# ggNetView (development version)

## Shiny app: adaptation to the newer ggNetView core

* Vendored the newer ggNetView core, adding two exported functions and exposing
  them in the Shiny app:
  * `ggnetview_subgraph()` — a "local-magnification" figure (full network beside
    a magnified module subgraph). Surfaced as a **Magnified Subgraph** panel in
    the Graph Explorer (adapter `safe_magnified_subgraph()`).
  * `gglink_heatmaps_2()` — an adaptive-tile-sized variant of `gglink_heatmaps()`.
    Environment Links now renders the standard and adaptive heatmaps side by side
    (adapter `safe_link_heatmap_adaptive()`).
* Preserved the repo's `audit H1` `k_nn` clamps (in `ggnetview.R` and
  `get_geo_neighbors.R`) through the core merge; a source-level guard test
  (`test-core-merge-guard.R`) locks them against future merges.
* Breaking-change hardening for the newer core: multipartite layouts now require
  an exact module count and `build_graph_from_igraph()` errors on a missing
  `module_attr` — both surface as friendly in-app failures via existing
  `safe_call` wrappers. Module outlines (`add_outer`) are now drawn as 2D-KDE
  highest-density-region (HDR) contours; the Visual Lab bandwidth help text was
  updated to match.

# ggNetView 0.2.0

## Correctness fixes (may change results)

* `get_node_centrality(weighted = TRUE)` now passes raw connection-strength
  weights to the strength-semantic measures (Eigenvector, PageRank, Hub,
  Authority) and inverse-weight (distance) only to Betweenness, Closeness,
  and Harmonic. Previously a single inverse weight was applied to every
  measure, which reversed Eigenvector/PageRank/HITS rankings on weighted
  (e.g. correlation) networks.
* `gglink_heatmaps()` now honors the user-supplied `cor.use` and `cor.method`
  on the spec-vs-environment correlation path (previously it always used the
  `psych::corr.test` defaults).
* `get_network_perturbation()` now reports the Schneider robustness index as a
  grid-spacing-invariant trapezoidal area under the LCC-fraction curve, and
  only for the `random`/`targeted` strategies (`module`/`manual` return `NA`).
  `Natural_connectivity` is computed with a numerically stable log-sum-exp form
  to avoid `Inf` on large/dense subgraphs.
* `ggnetview_zipi()` derives the total degree used in the participation
  coefficient from the same binarized adjacency as the within-module degree,
  keeping Pi within `[0, 1]`.
* `get_graph_adjacency()` now returns a weighted adjacency matrix when a
  `weight` edge attribute is present, matching its documentation.

## Reproducibility (determinism)

* Mantel permutation tests (`mantel_pairwise()`, `mantel_block_vs_col()`, and
  their callers/adapters) now accept and apply a `seed`, making p-values
  reproducible.
* `ggNetView_multi_link()` seeds its jitter and module layouts (and applies
  jitter once per group), so repeated runs are identical.
* Zi-Pi result names in the Shiny app no longer use random suffixes.

## Robustness

* Layout neighbor search clamps `k_nn` to `n - 1`, eliminating the loud
  `ANN: ERROR` output on small networks.
* `get_node_centrality()` falls back to an unweighted computation (with a
  warning) on non-finite/NA edge weights, and coerces non-finite Closeness to
  `NA` on disconnected graphs.
* `get_subgraph()` handles character/factor/numeric Modularity columns, errors
  clearly on a missing Modularity column, and warns on empty module selections.
* `safe_zipi()` validates Zi/Pi thresholds; correlation heatmaps skip tiny
  (`< 4` rows) or constant blocks cleanly instead of emitting NaN tiles; Mantel
  requires `n >= 4`.

# ggNetView 0.1.0

## Initial CRAN release

* Initial submission of `ggNetView` to CRAN.
* Provides a unified, reproducible framework for analyzing and
  visualizing complex biological, ecological, and microbial association
  networks.
* Includes tools for building correlation and co-occurrence networks
  via `WGCNA`, `SpiecEasi`, `SparCC`, and standard correlation methods
  (Pearson, Spearman, Kendall).
* Computes node-level and network-level topological metrics, including
  robustness analyses.
* Supports module-level analyses (modularity detection, Zi-Pi
  classification, sample-level subgraph topology).
* Offers a large family of deterministic layout generators built on
  `ggraph` and `ggplot2` (bipartite, tripartite, quadripartite,
  pentapartite, circular-modules, petal, diamond, heart, star, and
  more), all with reproducible seeds.
* Ships 18 example datasets covering OTU tables, taxonomy tables,
  environmental metadata, PPI networks, and modularity examples.
