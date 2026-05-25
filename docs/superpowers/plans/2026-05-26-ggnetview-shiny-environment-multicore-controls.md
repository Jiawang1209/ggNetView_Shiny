# ggNetView Shiny Environment Multi-core Controls Plan

Date: 2026-05-26

## Context

The manual's "Network Environment (multi-core)" examples use multiple `spec_select` blocks as multiple central networks. Shiny now exposes environment/spec block definitions and pair restrictions, but the user-facing controls still do not expose the multi-core geometry parameters that make these examples reproducible.

## Target Slice

Expose stable multi-core environment heatmap controls in the existing Compare & Environment workflow:

- spec block layout sequence (`spec_layout`)
- env quadrant orientation sequence (`orientation`)
- group layout (`group_layout`)
- anchor spacing (`anchor_dist`)
- row wrapping (`nrow`)
- heatmap distance (`distance`)
- network scaling (`scale_networks`)
- core point size (`CorePointSize`)

These should be passed to the real `gglink_heatmaps()` / `gglink_heatmaps_2()` adapters and preserved in registered params.

## Acceptance

- Focused tests prove parsing and pass-through of multi-core geometry parameters.
- Compare & Environment UI contains the new controls without adding a new top-level tab.
- Browser smoke confirms the controls are present in the real Shiny workflow.
- TODO docs move environment multi-core options out of the remaining gap list.
