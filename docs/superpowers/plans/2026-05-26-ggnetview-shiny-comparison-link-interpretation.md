# ggNetView Shiny Comparison Link Interpretation Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Make `ggNetView_multi_link()` comparison output easier to interpret by adding normalized link detail fields and pair-level summary tables to the existing Compare & Environment workflow.

**Architecture:** Keep the existing Compare & Environment tab. Extend `R/app_compare_environment.R` with a pure helper that turns `link_info` into both a detail table and a compact summary table. The Shiny module will display the detail table in the existing Statistics card and add a dedicated Link Summary table, registering both result objects for export.

**Tech Stack:** R, Shiny, DT, testthat, ggNetView adapters, `/usr/local/bin/Rscript`.

---

## Task 1: Link Interpretation Helper

**Files:**
- Modify: `R/app_compare_environment.R`
- Test: `tests/testthat/test-app-compare-environment.R`

- [ ] **Step 1: Write a failing test**

Append a focused test that calls `interpret_multi_network_links()` with a small `link_info` data frame:

```r
test_that("multi-network link interpretation summarizes pair-level links", {
  link_info <- data.frame(
    link_level = c("module", "module", "node"),
    group_a = c("A", "A", "A"),
    group_b = c("B", "B", "B"),
    source = c("1", "2", "OTU1"),
    target = c("1", "3", "OTU1"),
    x = c(0, 0, 1),
    y = c(0, 2, 1),
    xend = c(3, 4, 1),
    yend = c(4, 2, 3),
    stringsAsFactors = FALSE
  )

  interpreted <- interpret_multi_network_links(link_info)

  expect_true(is.data.frame(interpreted$details))
  expect_true(is.data.frame(interpreted$summary))
  expect_true(all(c("pair", "link_label", "distance") %in% names(interpreted$details)))
  expect_equal(interpreted$details$pair, rep("A vs B", 3))
  expect_equal(interpreted$summary$link_count[interpreted$summary$link_level == "module"], 2L)
  expect_equal(interpreted$summary$link_count[interpreted$summary$link_level == "node"], 1L)
  expect_equal(interpreted$summary$unique_sources[interpreted$summary$link_level == "module"], 2L)
})
```

- [ ] **Step 2: Verify RED**

Run:

```bash
/usr/local/bin/Rscript -e 'testthat::test_file("tests/testthat/test-app-compare-environment.R")'
```

Expected: FAIL because `interpret_multi_network_links()` does not exist.

- [ ] **Step 3: Implement the helper**

Add `interpret_multi_network_links()` in `R/app_compare_environment.R` after `normalize_multi_network_link_table()`. The helper should:

- accept raw `link_info`;
- call `normalize_multi_network_link_table()`;
- return `list(details = data.frame(), summary = data.frame())` for empty inputs;
- add `pair`, `link_label`, and Euclidean `distance` columns when coordinate columns are present;
- aggregate by `pair`, `group_a`, `group_b`, and `link_level` with link count, unique source count, unique target count, and mean distance.

- [ ] **Step 4: Verify GREEN**

Run:

```bash
/usr/local/bin/Rscript -e 'testthat::test_file("tests/testthat/test-app-compare-environment.R")'
```

Expected: PASS.

## Task 2: Adapter and UI Wiring

**Files:**
- Modify: `R/app_compare_environment.R`
- Modify: `inst/app/modules/mod_compare_environment.R`
- Modify: `inst/app/global.R`
- Modify: `tests/run_shiny_phase2_workflow_smoke.R`
- Modify: `docs/ggnetview-shiny-next-todos.md`

- [ ] **Step 1: Extend adapter payload**

In `safe_multi_network_compare()`, compute:

```r
link_interpretation <- interpret_multi_network_links(link_info)
link_table <- link_interpretation$details
link_summary <- link_interpretation$summary
```

Return `link_summary` alongside `link_table`.

- [ ] **Step 2: Extend Shiny UI**

Add a new card in `mod_compare_environment_ui()`:

```r
bslib::card(
  bslib::card_header("Link Summary"),
  DT::DTOutput(ns("compare_links"))
)
```

Create a `compare_link_summary_table` reactive value, update it after `run_compare`, render it with `DT::renderDT()`, and register it as a result object when non-empty.

- [ ] **Step 3: Expose helper at app startup**

Add `interpret_multi_network_links` to `inst/app/global.R` helper loading list.

- [ ] **Step 4: Update browser smoke**

Add `wait_for_element("compare_environment-compare_links")` after the existing comparison/topology waits.

- [ ] **Step 5: Update TODO docs**

Move the TODO entry from "more detailed link interpretation tables" to current baseline text and leave only deeper biological/statistical interpretation as a future gap.

## Task 3: Verification and Commit

**Files:**
- All modified files from Tasks 1-2.

- [ ] **Step 1: Run focused test**

```bash
/usr/local/bin/Rscript -e 'testthat::test_file("tests/testthat/test-app-compare-environment.R")'
```

- [ ] **Step 2: Run startup smoke**

```bash
/usr/local/bin/Rscript tests/run_shiny_app_startup.R
```

- [ ] **Step 3: Run browser workflow smoke**

```bash
/usr/local/bin/Rscript tests/run_shiny_phase2_workflow_smoke.R
```

- [ ] **Step 4: Run whitespace check**

```bash
git diff --check
```

- [ ] **Step 5: Commit**

```bash
git add R/app_compare_environment.R inst/app/global.R inst/app/modules/mod_compare_environment.R tests/testthat/test-app-compare-environment.R tests/run_shiny_phase2_workflow_smoke.R docs/ggnetview-shiny-next-todos.md docs/superpowers/plans/2026-05-26-ggnetview-shiny-comparison-link-interpretation.md
git commit -m "feat: summarize comparison links"
```
