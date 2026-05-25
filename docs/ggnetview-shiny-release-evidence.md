# ggNetView Shiny Release Evidence

Generated at: 2026-05-26T07:32:30+0800

## Manual Coverage

- Coverage ok: yes
- Manual areas covered: 10/10
- Missing required areas: none

| manual_area | workflow | evidence | smoke_script |
| --- | --- | --- | --- |
| 01 | Create graph object | Built matrix graph from gallery matrix and a second matrix fixture. | tests/run_shiny_manual_workflow_smoke.R |
| 02 | RMT | Ran ggNetView_RMT threshold workflow on the RMT fixture. | tests/run_shiny_manual_workflow_smoke.R |
| 03 | Graph info | Registered graph info tables from get_info_from_graph adapter. | tests/run_shiny_manual_workflow_smoke.R |
| 04 | Subgraph | Extracted module and sample subgraphs. | tests/run_shiny_manual_workflow_smoke.R |
| 05 | Layout | Rendered ggNetView layouts including manual layout families. | tests/run_shiny_manual_workflow_smoke.R |
| 06 | Network topology | Computed topology, centrality, Zi-Pi, and IVI boundary behavior. | tests/run_shiny_manual_workflow_smoke.R |
| 07 | Network compare | Ran ggNetView_multi_link comparison between two graph objects. | tests/run_shiny_manual_workflow_smoke.R |
| 07 | Network compare | Ran ggNetView_multi grouped matrix workflow with sample metadata. | tests/run_shiny_manual_workflow_smoke.R |
| 08 | Network environment | Ran environment link workflow from environment/spec matrices. | tests/run_shiny_manual_workflow_smoke.R |
| 08 | Network environment | Ran triple environment heatmap from graph-backed tables. | tests/run_shiny_manual_workflow_smoke.R |
| 08 | Network environment | Ran Mantel pairwise helper. | tests/run_shiny_manual_workflow_smoke.R |
| 09 | Multi-omics network | Built a multi-matrix graph from two omics-like matrix fixtures. | tests/run_shiny_manual_workflow_smoke.R |
| 10 | Gallery examples | Registered manual gallery starter objects. | tests/run_shiny_manual_workflow_smoke.R |

## Validation Commands

| status | command | result |
| --- | --- | --- |
| required | /usr/local/bin/Rscript -e 'testthat::test_file("tests/testthat/test-shiny-smoke-coverage.R")' | Focused coverage helper regression. |
| required | /usr/local/bin/Rscript -e 'testthat::test_file("tests/testthat/test-shiny-files.R")' | Static Shiny source/file regression. |
| required | /usr/local/bin/Rscript tests/run_shiny_manual_workflow_smoke.R | Manual-backed backend workflow and manual-area coverage. |
| required | /usr/local/bin/Rscript tests/run_shiny_phase2_workflow_smoke.R | Main browser workflow smoke. |
| required | /usr/local/bin/Rscript tests/run_shiny_graph_builder_modes_smoke.R | Graph Builder mode browser smoke. |
| required | /usr/local/bin/Rscript tests/run_shiny_analysis_export_smoke.R | Analysis/export browser smoke. |
| required | /usr/local/bin/Rscript tests/run_shiny_visual_layouts_smoke.R | Visual Lab layout browser smoke. |
| required | /usr/local/bin/Rscript tests/run_shiny_environment_geometry_smoke.R | Environment geometry browser smoke. |
| required | /usr/local/bin/Rscript tests/run_shiny_mobile_layout_smoke.R | Mobile navigation/overflow browser smoke. |
| required | /usr/local/bin/Rscript tests/run_shiny_task_feedback_smoke.R | Long-running action feedback browser smoke. |

## Recent Commits

| hash | subject |
| --- | --- |
| bf1229e | test: record manual smoke coverage |
| 0532db6 | feat: enrich comparison report narratives |
| dad6f91 | test: add task feedback browser smoke |
| e1899bb | feat: improve workflow restore replay UX |
| e00629b | feat: expand comparison layout controls |
| 91869e2 | feat: restore unreplayable workflow objects |
| a03bf5d | feat: expose visual lab manual controls |
| 84999a4 | feat: add comparison report presets |
| 34b9fae | feat: add environment report presets |
| 9cb3662 | feat: restore workflow manifest inputs |
| 065a18f | fix: stabilize environment plot exports |
| c672cac | feat: add environment interpretation summaries |
| 636ab11 | feat: add direct mantel controls |
| 3b1f597 | feat: add environment heatmap style controls |
| 93dfd38 | feat: add parallel network topology path |
| 0e6058a | feat: add stringdb graph builder |
| cac175d | feat: add igraph graph builder |
| 88d901a | feat: add node edge graph builder |
| 6c6ff50 | feat: add module environment heatmap |
| 3495087 | feat: replay graph builder workflows |

## Remaining Limits

- Project-specific biological/statistical report wording still needs refinement after longer real-use sessions.
- Cross-session restore merge review and editable restored-object summaries remain future workflow polish.
- Future long-running buttons should receive targeted busy-state browser assertions as they are added.
- A final continuous all-smoke pass should be run immediately before release or handoff.

## Next Release Steps

1. Run the full validation command list sequentially with /usr/local/bin/Rscript.
2. Launch the Shiny app and inspect the main tabs against the generated evidence report.
3. Review remaining limits with the user and decide whether they block release.
4. Create the final release/readiness commit after the full pass is green.

