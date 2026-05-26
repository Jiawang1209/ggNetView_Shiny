# ggNetView Shiny Sample Topology Implementation Plan

Date: 2026-05-26

## Goal

Expose the manual's sample-level topology workflow in the existing Topology & Keystone tab. This covers `get_sample_subgraph_topology()` and the parallel variant without creating a separate function-per-tab UI.

## Scope

- Add a `safe_sample_topology()` adapter that calls real ggNetView APIs.
- Support serial and parallel sample topology modes with conservative defaults.
- Add Topology UI controls for selecting the matrix used to derive sample subgraphs.
- Register sample topology, robustness, and sample-stat result tables for export.
- Extend focused unit tests and browser smoke coverage.

## Verification

Run:

```bash
/usr/local/bin/Rscript -e 'testthat::test_file("tests/testthat/test-app-topology-adapters.R")'
/usr/local/bin/Rscript tests/run_shiny_app_startup.R
/usr/local/bin/Rscript tests/run_shiny_analysis_export_smoke.R
/usr/local/bin/Rscript tests/run_shiny_phase2_workflow_smoke.R
git diff --check
```

