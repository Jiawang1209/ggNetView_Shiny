test_that("first milestone Shiny module files exist", {
  files <- c(
    "inst/app/modules/mod_data_hub.R",
    "inst/app/modules/mod_graph_builder.R",
    "inst/app/modules/mod_rmt_builder.R",
    "inst/app/modules/mod_graph_explorer.R",
    "inst/app/modules/mod_visual_lab.R",
    "inst/app/modules/mod_topology_results.R",
    "inst/app/modules/mod_zipi_results.R",
    "inst/app/modules/mod_perturbation.R",
    "inst/app/modules/mod_network_compare.R",
    "inst/app/modules/mod_environment_links.R",
    "inst/app/modules/mod_compare_environment.R",
    "inst/app/modules/mod_export_center.R",
    "inst/app/ui.R",
    "inst/app/server.R",
    "inst/app/www/styles.css"
  )

  expect_true(all(file.exists(test_path("../../", files))))
})

test_that("Shiny navigation separates optional RMT, network compare, and environment workflows", {
  ui_text <- paste(readLines(test_path("../../inst/app/ui.R"), warn = FALSE), collapse = "\n")
  global_text <- paste(readLines(test_path("../../inst/app/global.R"), warn = FALSE), collapse = "\n")
  server_text <- paste(readLines(test_path("../../inst/app/server.R"), warn = FALSE), collapse = "\n")

  labels <- c(
    "Introduction",
    "Manual",
    "Data Hub",
    "Graph Builder",
    "RMT Builder",
    "Graph Explorer",
    "Visual Lab",
    "Analysis",
    "Topology",
    "Zi-Pi",
    "Perturbation",
    "Network Compare",
    "Environment Links",
    "Export"
  )
  positions <- vapply(labels, function(label) regexpr(label, ui_text, fixed = TRUE)[[1]], integer(1))

  expect_true(all(positions > 0))
  expect_true(all(diff(positions) > 0))
  expect_match(ui_text, "mod_rmt_builder_ui", fixed = TRUE)
  expect_match(ui_text, "mod_network_compare_ui", fixed = TRUE)
  expect_match(ui_text, "mod_environment_links_ui", fixed = TRUE)
  expect_match(global_text, "mod_rmt_builder.R", fixed = TRUE)
  expect_match(global_text, "mod_network_compare.R", fixed = TRUE)
  expect_match(global_text, "mod_environment_links.R", fixed = TRUE)
  expect_match(global_text, "mod_zipi_results.R", fixed = TRUE)
  expect_match(ui_text, "mod_perturbation_ui", fixed = TRUE)
  expect_match(global_text, "mod_perturbation.R", fixed = TRUE)
  expect_match(server_text, "mod_perturbation_server", fixed = TRUE)
  # Analysis pages are consolidated under a single nav_menu.
  expect_match(ui_text, "nav_menu(", fixed = TRUE)
  expect_match(ui_text, "\"Analysis\"", fixed = TRUE)
  expect_match(server_text, "mod_rmt_builder_server", fixed = TRUE)
  expect_match(server_text, "mod_network_compare_server", fixed = TRUE)
  expect_match(server_text, "mod_environment_links_server", fixed = TRUE)
  expect_match(server_text, "mod_zipi_results_server", fixed = TRUE)
})

test_that("standard Graph Builder no longer exposes RMT-only controls", {
  source_text <- paste(readLines(test_path("../../inst/app/modules/mod_graph_builder.R"), warn = FALSE), collapse = "\n")

  expect_false(grepl("Matrix + RMT", source_text, fixed = TRUE))
  expect_false(grepl("run_rmt", source_text, fixed = TRUE))
  expect_false(grepl("\"matrix_rmt\"", source_text, fixed = TRUE))
})

test_that("Visual Lab groups parameters into Basics/Appearance/Advanced tiers with layout-aware visibility", {
  source_text <- paste(readLines(test_path("../../inst/app/modules/mod_visual_lab.R"), warn = FALSE), collapse = "\n")

  expect_match(source_text, "\"Basics\"", fixed = TRUE)
  expect_match(source_text, "\"Appearance\"", fixed = TRUE)
  expect_match(source_text, "\"Advanced layout\"", fixed = TRUE)
  # Module/ring controls are gated behind a layout-aware conditionalPanel.
  expect_match(source_text, "conditionalPanel", fixed = TRUE)
  expect_match(source_text, "module_layout_js", fixed = TRUE)
  expect_match(source_text, "adjacent_module_js", fixed = TRUE)
  # Parameter help is surfaced via tooltips.
  expect_match(source_text, "visual_lab_tip", fixed = TRUE)
  # All original parameter inputs are still present after the reorganization.
  for (input_id in c("shrink", "inner_shrink", "k_nn", "push_others_delta", "ring_n",
                     "node_add", "plot_width", "plot_height", "seed", "linecolor")) {
    expect_match(source_text, sprintf("ns(\"%s\")", input_id), fixed = TRUE)
  }
})

test_that("Graph Builder arranges build parameters in two-column rows", {
  source_text <- paste(readLines(test_path("../../inst/app/modules/mod_graph_builder.R"), warn = FALSE), collapse = "\n")
  css_text <- paste(readLines(test_path("../../inst/app/www/styles.css"), warn = FALSE), collapse = "\n")

  expect_match(source_text, "class = \"graph-builder-params\"", fixed = TRUE)
  expect_match(source_text, "class = \"graph-builder-actions\"", fixed = TRUE)
  expect_match(source_text, "class = \"graph-builder-two-column\"", fixed = TRUE)
  expect_match(source_text, "class = \"graph-builder-card\"", fixed = TRUE)
  expect_false(grepl("col_widths = c(8, 4)", source_text, fixed = TRUE))
  expect_false(grepl("col_widths = c(6, 6", source_text, fixed = TRUE))
  expect_match(css_text, ".graph-builder-two-column", fixed = TRUE)
  expect_match(css_text, "grid-template-columns: minmax(0, 2fr) minmax(320px, 1fr);", fixed = TRUE)
  expect_match(css_text, ".graph-builder-params", fixed = TRUE)
  expect_match(css_text, "grid-template-columns: repeat(2, minmax(280px, 1fr));", fixed = TRUE)
  expect_match(css_text, ".graph-builder-card > .card-body", fixed = TRUE)
  expect_match(css_text, "overflow: visible !important;", fixed = TRUE)
  expect_match(css_text, ".graph-builder-params .shiny-input-container", fixed = TRUE)
  expect_match(css_text, "width: 100% !important;", fixed = TRUE)
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

test_that("Graph Explorer keeps parameter-heavy subgraph controls in the sidebar", {
  ui_text <- paste(readLines(test_path("../../inst/app/modules/mod_graph_explorer.R"), warn = FALSE), collapse = "\n")

  expect_match(ui_text, "layout_sidebar", fixed = TRUE)
  expect_match(ui_text, "title = \"Graph Controls\"", fixed = TRUE)
  expect_match(ui_text, "width = 400", fixed = TRUE)
  expect_match(ui_text, "bslib::card_header(\"Summary\")", fixed = TRUE)
  expect_match(ui_text, "accordion_panel", fixed = TRUE)
  expect_match(ui_text, "\"Module subgraph\"", fixed = TRUE)
  expect_match(ui_text, "\"Sample subgraph\"", fixed = TRUE)
  expect_match(ui_text, "col_widths = c(12, 6, 6, 6, 6)", fixed = TRUE)
  expect_false(grepl("bslib::card_header(\"Subgraph\")", ui_text, fixed = TRUE))
  expect_false(grepl("col_widths = c(4, 8, 4, 4, 4, 4, 4)", ui_text, fixed = TRUE))
})

test_that("Topology keeps calculate controls readable in a sidebar accordion", {
  ui_text <- paste(readLines(test_path("../../inst/app/modules/mod_topology_results.R"), warn = FALSE), collapse = "\n")
  css_text <- paste(readLines(test_path("../../inst/app/www/styles.css"), warn = FALSE), collapse = "\n")

  expect_match(ui_text, "layout_sidebar", fixed = TRUE)
  expect_match(ui_text, "title = \"Calculate\"", fixed = TRUE)
  expect_match(ui_text, "width = 420", fixed = TRUE)
  expect_match(ui_text, "accordion", fixed = TRUE)
  expect_match(ui_text, "\"Network topology\"", fixed = TRUE)
  expect_match(ui_text, "\"Sample topology\"", fixed = TRUE)
  expect_match(ui_text, "\"Node centrality\"", fixed = TRUE)
  expect_match(ui_text, "\"IVI\"", fixed = TRUE)
  expect_match(ui_text, "class = \"topology-control-grid\"", fixed = TRUE)
  expect_match(css_text, ".topology-control-grid", fixed = TRUE)
  expect_match(css_text, "grid-template-columns: repeat(2, minmax(0, 1fr));", fixed = TRUE)
  expect_false(grepl("\"Zi-Pi\"", ui_text, fixed = TRUE))
  expect_false(grepl("zi_threshold", ui_text, fixed = TRUE))
  expect_false(grepl("pi_threshold", ui_text, fixed = TRUE))
  expect_false(grepl("calculate_zipi", ui_text, fixed = TRUE))
  expect_false(grepl("safe_zipi", ui_text, fixed = TRUE))
  expect_false(grepl("col_widths = c(4, 8, 6, 6, 6, 6, 6)", ui_text, fixed = TRUE))
})

test_that("Zi-Pi has a dedicated results page", {
  ui_text <- paste(readLines(test_path("../../inst/app/ui.R"), warn = FALSE), collapse = "\n")
  global_text <- paste(readLines(test_path("../../inst/app/global.R"), warn = FALSE), collapse = "\n")
  server_text <- paste(readLines(test_path("../../inst/app/server.R"), warn = FALSE), collapse = "\n")
  source_text <- paste(readLines(test_path("../../inst/app/modules/mod_zipi_results.R"), warn = FALSE), collapse = "\n")
  css_text <- paste(readLines(test_path("../../inst/app/www/styles.css"), warn = FALSE), collapse = "\n")

  expect_match(ui_text, "nav_panel(\"Zi-Pi\", mod_zipi_results_ui(\"zipi_results\"))", fixed = TRUE)
  expect_match(global_text, "mod_zipi_results.R", fixed = TRUE)
  expect_match(server_text, "mod_zipi_results_server(\"zipi_results\", registry)", fixed = TRUE)

  expect_match(source_text, "mod_zipi_results_ui", fixed = TRUE)
  expect_match(source_text, "mod_zipi_results_server", fixed = TRUE)
  expect_match(source_text, "Calculate Zi-Pi", fixed = TRUE)
  expect_match(source_text, "class = \"zipi-page\"", fixed = TRUE)
  expect_match(source_text, "class = \"zipi-control-grid\"", fixed = TRUE)
  expect_match(source_text, "DT::DTOutput(ns(\"zipi\"))", fixed = TRUE)
  expect_match(source_text, "download_zipi", fixed = TRUE)
  expect_match(source_text, "safe_zipi", fixed = TRUE)
  expect_match(css_text, ".zipi-page", fixed = TRUE)
  expect_match(css_text, ".zipi-control-grid", fixed = TRUE)
})

test_that("split comparison pages provide every shared server output binding", {
  network_text <- paste(readLines(test_path("../../inst/app/modules/mod_network_compare.R"), warn = FALSE), collapse = "\n")
  environment_text <- paste(readLines(test_path("../../inst/app/modules/mod_environment_links.R"), warn = FALSE), collapse = "\n")
  server_text <- paste(readLines(test_path("../../inst/app/modules/mod_compare_environment.R"), warn = FALSE), collapse = "\n")

  shared_outputs <- unique(sub(
    ".*output\\$([A-Za-z0-9_]+).*",
    "\\1",
    regmatches(server_text, gregexpr("output\\$[A-Za-z0-9_]+", server_text))[[1]]
  ))

  for (output_id in shared_outputs) {
    expect_match(network_text, sprintf('ns("%s")', output_id), fixed = TRUE)
    expect_match(environment_text, sprintf('ns("%s")', output_id), fixed = TRUE)
  }
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
