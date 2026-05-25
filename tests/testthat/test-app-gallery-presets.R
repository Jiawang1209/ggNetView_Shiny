source(testthat::test_path("../../R/app_validation.R"))
source(testthat::test_path("../../R/app_registry.R"))
source(testthat::test_path("../../R/app_adapters.R"))
source(testthat::test_path("../../R/app_graph_builders.R"))
source(testthat::test_path("../../R/app_compare_environment.R"))
source(testthat::test_path("../../R/app_gallery_presets.R"))

test_that("gallery workflow manifest maps manual examples", {
  manifest <- gallery_workflow_manifest()

  expect_true(all(c("workflow", "manual_area") %in% names(manifest)))
  expect_true("matrix_graph" %in% manifest$workflow)
  expect_true("consensus_graph" %in% manifest$workflow)
})

test_that("gallery examples register typed inputs and starter graphs", {
  registry <- registry_new()
  register_gallery_examples(registry, root = testthat::test_path("../.."))
  listed <- shiny::isolate(registry_list(registry))

  expect_true(all(c("matrix", "edge_table", "module_table", "adjacency", "wgcna_tom", "result") %in% listed$type))
  expect_true("graph" %in% listed$type)
  expect_true(any(listed$name == "gallery_workflows"))
})

test_that("gallery recipe manifest exposes one-click workflows", {
  recipes <- gallery_recipe_manifest()

  expect_true(all(c("recipe", "label", "output_type") %in% names(recipes)))
  expect_true(all(c("network_plot_circle", "grouped_network_plot") %in% recipes$recipe))
})

test_that("gallery recipes register reproducible outputs", {
  registry <- registry_new()
  register_gallery_examples(registry, root = testthat::test_path("../.."))

  result <- run_gallery_recipe(registry, "network_plot_circle")
  expect_true(isTRUE(result$ok), info = result$trace %||% result$message)
  expect_true(length(result$value$items) >= 1L)

  grouped <- run_gallery_recipe(registry, "grouped_network_plot")
  expect_true(isTRUE(grouped$ok), info = grouped$trace %||% grouped$message)

  listed <- shiny::isolate(registry_list(registry))
  expect_true(any(listed$name == "gallery_recipe_circle_plot"))
  expect_true(any(listed$name == "gallery_recipe_grouped_network_plot"))
  expect_true(any(listed$name == "gallery_recipe_grouped_network_groups"))
})
