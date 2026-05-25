test_that("first milestone Shiny module files exist", {
  files <- c(
    "inst/app/modules/mod_data_hub.R",
    "inst/app/modules/mod_graph_builder.R",
    "inst/app/modules/mod_graph_explorer.R",
    "inst/app/modules/mod_visual_lab.R",
    "inst/app/modules/mod_topology_results.R",
    "inst/app/modules/mod_export_center.R",
    "inst/app/ui.R",
    "inst/app/server.R",
    "inst/app/www/styles.css"
  )

  expect_true(all(file.exists(test_path("../../", files))))
})
