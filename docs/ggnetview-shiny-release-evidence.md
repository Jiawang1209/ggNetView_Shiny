# ggNetView Shiny Release Evidence

Generated at: 2026-05-26T07:38:56+0800

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
| passed | /usr/local/bin/Rscript -e 'testthat::test_file("tests/testthat/test-shiny-smoke-coverage.R")' | Passed in final audit: 11 coverage-helper assertions. |
| passed | /usr/local/bin/Rscript -e 'testthat::test_file("tests/testthat/test-shiny-files.R")' | Passed in final audit: 25 Shiny source/file assertions. |
| passed | /usr/local/bin/Rscript tests/run_shiny_manual_workflow_smoke.R | Passed in final audit: manual workflow smoke passed and regenerated 10/10 manual coverage JSON. |
| passed | /usr/local/bin/Rscript tests/run_shiny_phase2_workflow_smoke.R | Passed in final audit: phase2 browser workflow smoke passed. |
| passed | /usr/local/bin/Rscript tests/run_shiny_graph_builder_modes_smoke.R | Passed in final audit: graph builder modes browser smoke passed. |
| passed | /usr/local/bin/Rscript tests/run_shiny_analysis_export_smoke.R | Passed in final audit: analysis/export browser smoke passed. |
| passed | /usr/local/bin/Rscript tests/run_shiny_visual_layouts_smoke.R | Passed in final audit: visual layouts browser smoke passed for 57/57 layouts. |
| passed | /usr/local/bin/Rscript tests/run_shiny_environment_geometry_smoke.R | Passed in final audit: environment geometry browser smoke passed for 4/4 recipes. |
| passed | /usr/local/bin/Rscript tests/run_shiny_mobile_layout_smoke.R | Passed in final audit: mobile layout browser smoke passed. |
| passed | /usr/local/bin/Rscript tests/run_shiny_task_feedback_smoke.R | Passed in final audit: task feedback browser smoke passed. |

## Recent Commits

| hash | subject |
| --- | --- |
| 5fda78b | test: add release evidence report |
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

## Remaining Limits

- Project-specific biological/statistical report wording still needs refinement after longer real-use sessions.
- Cross-session restore merge review and editable restored-object summaries remain future workflow polish.
- Future long-running buttons should receive targeted busy-state browser assertions as they are added.

## Next Release Steps

1. Open the Shiny app for a short human visual review if desired.
2. Decide with the user whether the remaining polish items block the current release.
3. Prepare push or release packaging only when the user is ready.

