# ggNetView Shiny Multi-omics Gallery Presets Plan

Date: 2026-05-26

## Context

The Phase 2 graph-builder plan completed the core multi-matrix and double-matrix builder paths. The broader `/goal` also asks for manual-aligned Gallery/example workflows and richer multi-omics presets. The current Gallery exposes one `multi_omics_network` recipe, which proves the multi-matrix graph path but does not yet demonstrate the double-matrix route or the environment-link interpretation workflow that users will expect from a multi-omics example.

## Target Slice

Add two small, API-backed Gallery recipes without changing the top-level information architecture:

- `multi_omics_double_matrix`: build a two-omics graph from `gallery_matrix` and `gallery_matrix_b` using the real `build_graph_from_double_mat()` adapter, then register a plot.
- `multi_omics_environment_blocks`: combine the two omics matrices into named spec blocks and run the real `gglink_heatmaps()` adapter with environment/spec block selectors plus env/spec pair restrictions.

## Acceptance

- Both recipes appear in `gallery_recipe_manifest()`.
- `run_gallery_recipe()` registers reproducible graph/plot/result objects with recipe metadata.
- Focused Gallery tests cover the new recipes.
- Browser smoke confirms the new recipe controls remain reachable in the real Shiny flow.
- `docs/ggnetview-shiny-next-todos.md` reflects that Gallery has richer multi-omics-specific presets.
