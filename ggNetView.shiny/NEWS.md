# ggNetView.shiny 0.1.0

* Initial release.
* Provides `launch_ggNetView()` to start a `shinydashboard`-based GUI
  on top of the `ggNetView` R package.
* Modules:
  - **Data**: load built-in `ggNetView` datasets or upload CSV/TSV/RDS.
  - **Build Network**: build graph objects via WGCNA / SparCC / SpiecEasi /
    `cor` / `Hmisc`, or directly from adjacency matrix / edge list.
  - **Visualize**: interactive parameter tuning of `ggNetView()` with live
    plot rendering and PDF/PNG export.
  - **Topology / zi-pi**: compute global topology metrics and node-role
    classification with downloadable tables and zi-pi scatter plot.
  - **Env-Spec Linkage**: render `gglink_heatmaps()` with multi-block
    selection.
