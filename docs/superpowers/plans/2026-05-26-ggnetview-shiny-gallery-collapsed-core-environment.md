# ggNetView Shiny Gallery Collapsed-Core Environment Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add a Gallery recipe that reproduces the manual's Network Environment "combine-core to one point" workflow with real `gglink_heatmaps()` calls.

**Architecture:** Keep this as a Gallery/example workflow, not a new tab. Reuse existing phase2 matrices and `safe_environment_heatmap()` so the recipe registers a plot and stats result with reproducible params showing `spec_collapse`, multi-block env/spec selectors, pair restrictions, row layout, and collapsed-core point sizing.

**Tech Stack:** R, ggNetView local helpers, testthat, shinytest2, `/usr/local/bin/Rscript`.

---

## Task 1: Add Collapsed-Core Gallery Recipe

**Files:**
- Modify: `R/app_gallery_presets.R`
- Modify: `tests/testthat/test-app-gallery-presets.R`
- Modify: `tests/run_shiny_phase2_workflow_smoke.R`
- Modify: `docs/ggnetview-shiny-next-todos.md`

- [ ] **Step 1: Write failing tests**

Update `tests/testthat/test-app-gallery-presets.R` so `gallery_recipe_manifest()` must include `environment_collapsed_core`, `run_gallery_recipe()` must return ok for it, and the registry must include `gallery_recipe_environment_collapsed_core_heatmap` plus `gallery_recipe_environment_collapsed_core_stats`.

- [ ] **Step 2: Verify RED**

Run:

```bash
/usr/local/bin/Rscript -e 'testthat::test_file("tests/testthat/test-app-gallery-presets.R")'
```

Expected: FAIL because the new recipe is not present.

- [ ] **Step 3: Implement recipe manifest and runner**

Add `environment_collapsed_core` to `gallery_recipe_manifest()` with label `Collapsed-core environment heatmap`, output type `plot,result`, and manual area `Network-environment collapsed core`.

In `run_gallery_recipe()`, add a branch that:

- loads `gallery_matrix`;
- builds `fixture <- gallery_environment_fixture(matrix_item$data)`;
- adds a fourth environment variable so two env blocks each have two columns;
- uses spec blocks `Early = OTU1..OTU3` and `Late = OTU4..OTU6`;
- uses env/spec pairs `Climate,Early` and `Water,Late`;
- calls `safe_environment_heatmap()` with params:
  - `relation_method = "correlation"`
  - `cor.method = "pearson"`
  - `spec_collapse = TRUE`
  - `orientation = c("top_right", "bottom_right")`
  - `group_layout = "row"`
  - `anchor_dist = 4`
  - `distance = 2`
  - `r = 0.1`
  - `CorePointSize = 10`
  - `HeatmapPointSize = 4`
- registers plot and stats result objects.

- [ ] **Step 4: Verify GREEN**

Run:

```bash
/usr/local/bin/Rscript -e 'testthat::test_file("tests/testthat/test-app-gallery-presets.R")'
```

Expected: PASS.

## Task 2: Browser Smoke and Docs

**Files:**
- Modify: `tests/run_shiny_phase2_workflow_smoke.R`
- Modify: `docs/ggnetview-shiny-next-todos.md`

- [ ] **Step 1: Browser smoke coverage**

Add the new recipe selection after `multi_omics_environment_blocks`:

```r
set_input("data_hub-gallery_recipe", "environment_collapsed_core")
click("#data_hub-run_gallery_recipe")
wait_for_text("gallery_recipe_environment_collapsed_core_heatmap", timeout = 120000)
```

- [ ] **Step 2: TODO update**

Update the Environment and Gallery baseline to mention the collapsed-core environment recipe. Narrow the remaining gap to visual-regression coverage for all geometry variants.

## Task 3: Verification and Commit

- [ ] **Step 1: Run focused gallery tests**

```bash
/usr/local/bin/Rscript -e 'testthat::test_file("tests/testthat/test-app-gallery-presets.R")'
```

- [ ] **Step 2: Run startup smoke**

```bash
/usr/local/bin/Rscript tests/run_shiny_app_startup.R
```

- [ ] **Step 3: Run Phase2 browser smoke**

```bash
/usr/local/bin/Rscript tests/run_shiny_phase2_workflow_smoke.R
```

- [ ] **Step 4: Run whitespace check**

```bash
git diff --check
```

- [ ] **Step 5: Commit**

```bash
git add R/app_gallery_presets.R tests/testthat/test-app-gallery-presets.R tests/run_shiny_phase2_workflow_smoke.R docs/ggnetview-shiny-next-todos.md docs/superpowers/plans/2026-05-26-ggnetview-shiny-gallery-collapsed-core-environment.md
git commit -m "feat: add collapsed-core environment gallery"
```
