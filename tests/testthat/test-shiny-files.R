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

test_that("Shiny UI exposes an Introduction tab backed by README markdown", {
  ui_text <- paste(readLines(test_path("../../inst/app/ui.R"), warn = FALSE), collapse = "\n")
  expect_match(ui_text, "Introduction", fixed = TRUE)
  expect_match(ui_text, "includeMarkdown", fixed = TRUE)
  expect_match(ui_text, "README.md", fixed = TRUE)
})

test_that("Shiny UI exposes the bundled manual as a resource-backed tab", {
  ui_text <- paste(readLines(test_path("../../inst/app/ui.R"), warn = FALSE), collapse = "\n")
  global_text <- paste(readLines(test_path("../../inst/app/global.R"), warn = FALSE), collapse = "\n")

  expect_true(file.exists(test_path("../../package/ggNetView-manual/docs/index.html")))
  expect_match(global_text, "addResourcePath", fixed = TRUE)
  expect_match(global_text, "\"manual\"", fixed = TRUE)
  expect_match(global_text, "package/ggNetView-manual/docs", fixed = TRUE)
  expect_match(ui_text, "Manual", fixed = TRUE)
  expect_match(ui_text, "iframe", fixed = TRUE)
  expect_match(ui_text, "manual/index.html", fixed = TRUE)

  tab_positions <- vapply(
    c(
      'nav_panel(\n    "Introduction"',
      'nav_panel(\n    "Manual"',
      'nav_panel("Data Hub"',
      'nav_panel("Export"'
    ),
    function(pattern) regexpr(pattern, ui_text, fixed = TRUE)[[1]],
    integer(1)
  )
  expect_true(all(tab_positions > 0))
  expect_true(tab_positions[[1]] < tab_positions[[2]])
  expect_true(tab_positions[[2]] < tab_positions[[3]])
  expect_true(tab_positions[[3]] < tab_positions[[4]])
})

test_that("Graph Explorer gives subgraph controls enough layout space", {
  ui_text <- paste(readLines(test_path("../../inst/app/modules/mod_graph_explorer.R"), warn = FALSE), collapse = "\n")

  expect_match(ui_text, "layout_sidebar", fixed = TRUE)
  expect_match(ui_text, "accordion_panel", fixed = TRUE)
  expect_match(ui_text, "\"Module subgraph\"", fixed = TRUE)
  expect_match(ui_text, "\"Sample subgraph\"", fixed = TRUE)
  expect_match(ui_text, "col_widths = c(12, 12, 6, 6, 6, 6)", fixed = TRUE)
  expect_false(grepl("col_widths = c(4, 8, 4, 4, 4, 4, 4)", ui_text, fixed = TRUE))
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
  expect_match(source_text, "source(file.path(repo_root, \"inst\", \"app\", \"modules\", \"mod_visual_lab.R\"))", fixed = TRUE)
  expect_match(source_text, "visual_lab_layout_choices()", fixed = TRUE)
  expect_match(source_text, "visual_layout_smoke_cases", fixed = TRUE)

  source(test_path("../../inst/app/modules/mod_visual_lab.R"))
  choices <- unlist(visual_lab_layout_choices(), use.names = FALSE)
  expect_gt(length(choices), 40)
  expect_true(all(choices %in% visual_layout_smoke_cases()$layout))
})

test_that("environment geometry browser smoke covers gallery geometry presets", {
  path <- test_path("../../tests/run_shiny_environment_geometry_smoke.R")
  expect_true(file.exists(path))

  source_text <- paste(readLines(path, warn = FALSE), collapse = "\n")
  expect_match(source_text, "environment_heatmap", fixed = TRUE)
  expect_match(source_text, "multi_omics_environment_blocks", fixed = TRUE)
  expect_match(source_text, "environment_collapsed_core", fixed = TRUE)
  expect_match(source_text, "environment_arc_collapsed_core", fixed = TRUE)
  expect_match(source_text, "assert_export_plot_available", fixed = TRUE)
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
