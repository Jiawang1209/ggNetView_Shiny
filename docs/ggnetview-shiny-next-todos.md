# ggNetView Shiny Next TODOs

Date: 2026-05-26

## Current Baseline

- The Shiny app runs from the root project with `/usr/local/bin/Rscript`.
- The example workflow can load the bundled matrix, build a graph, inspect objects, and reach export.
- Local source loading now keeps same-package helper dependencies available.
- Export no longer shows plot PNG/PDF buttons for non-plot objects.

## Next Priorities

1. Improve graph export semantics.
   - For graph objects, CSV export should not be a plain `str()` dump.
   - Add explicit node table and edge table exports, likely as separate buttons or a zip bundle.
   - Keep RDS as the full-fidelity graph export.

2. Add a full browser-level workflow smoke.
   - Use the local Shiny app in a real browser.
   - Click through: load example matrix, build graph, inspect graph, draw plot, calculate topology, export files.
   - Assert key UI text and download status codes.

3. Make Export Center object-aware.
   - Matrix/table: CSV, RDS, params.
   - Graph: RDS, node CSV, edge CSV, params.
   - Plot: RDS, PNG, PDF, params.
   - Result table: CSV, RDS, params.

4. Improve user feedback during long operations.
   - Add progress/status indicators for graph build, plot draw, and topology calculation.
   - Disable action buttons while a calculation is running.
   - Keep the last successful result visible but clearly mark failed attempts.

5. Polish layout and wording after real use.
   - Reduce ambiguity in Export Center buttons.
   - Make selected object type visible near the export controls.
   - Consider grouping controls into object-specific sections.

6. Extend example data and presets.
   - Add at least one edge-table example.
   - Add at least one adjacency-matrix example.
   - Provide sensible Graph Builder presets for matrix, adjacency, and edge workflows.

7. Add regression tests for issues found during manual use.
   - Local source dependencies for `build_graph_from_mat()`.
   - Empty source type in Graph Builder.
   - Plot downloads hidden for non-plot objects.
   - Future graph node/edge export behavior.

## Suggested Next Work Session

Start with Export Center, because it is the first area that still feels semantically wrong after the main workflow is usable.

Target outcome:

- Selecting a graph object exposes `Download Nodes CSV` and `Download Edges CSV`.
- Matrix and result-table CSV exports keep their current behavior.
- Plot PNG/PDF buttons remain hidden unless the selected object is a plot.
- Browser smoke confirms that non-plot export buttons do not return 500 responses.

