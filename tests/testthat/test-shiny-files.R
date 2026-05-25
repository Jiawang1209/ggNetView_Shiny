test_that("first milestone Shiny module files exist", {
  files <- c(
    "inst/app/modules/mod_data_hub.R",
    "inst/app/modules/mod_graph_builder.R",
    "inst/app/modules/mod_graph_explorer.R",
    "inst/app/modules/mod_visual_lab.R",
    "inst/app/modules/mod_topology_results.R",
    "inst/app/modules/mod_compare_environment.R",
    "inst/app/modules/mod_export_center.R",
    "inst/app/ui.R",
    "inst/app/server.R",
    "inst/app/www/styles.css"
  )

  expect_true(all(file.exists(test_path("../../", files))))
})

test_that("mobile layout browser smoke exists and checks overflow", {
  path <- test_path("../../tests/run_shiny_mobile_layout_smoke.R")
  expect_true(file.exists(path))

  source_text <- paste(readLines(path, warn = FALSE), collapse = "\n")
  expect_match(source_text, "width = 390", fixed = TRUE)
  expect_match(source_text, "assert_no_horizontal_overflow", fixed = TRUE)
  expect_match(source_text, "Data Hub", fixed = TRUE)
  expect_match(source_text, "Export", fixed = TRUE)
})
