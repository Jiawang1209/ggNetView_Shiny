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

test_that("visual layout browser smoke exists and covers layout families", {
  path <- test_path("../../tests/run_shiny_visual_layouts_smoke.R")
  expect_true(file.exists(path))

  source_text <- paste(readLines(path, warn = FALSE), collapse = "\n")
  expect_match(source_text, "circle_outline", fixed = TRUE)
  expect_match(source_text, "circular_modules_equal_petal_layout", fixed = TRUE)
  expect_match(source_text, "bipartite_layout", fixed = TRUE)
  expect_match(source_text, "WGCNA", fixed = TRUE)
})

test_that("package/manual audit reflects the current Shiny coverage", {
  path <- test_path("../../docs/ggnetview-new-package-shiny-audit.md")
  expect_true(file.exists(path))

  source_text <- paste(readLines(path, warn = FALSE), collapse = "\n")
  expect_match(source_text, "Status refreshed: 2026-05-26", fixed = TRUE)
  expect_match(source_text, "Current Evidence", fixed = TRUE)
  expect_match(source_text, "tests/run_shiny_graph_builder_modes_smoke.R", fixed = TRUE)
  expect_match(source_text, "tests/run_shiny_visual_layouts_smoke.R", fixed = TRUE)

  expect_false(grepl("| `02-RMT.Rmd` | RMT threshold scan, then build graph with chosen threshold | Missing |", source_text, fixed = TRUE))
  expect_false(grepl("| `10-Gallery_of_Reproducible_Examples.Rmd` | Publication recipes and reusable parameter presets | Missing |", source_text, fixed = TRUE))
})
