source(testthat::test_path("../../R/app_validation.R"))
source(testthat::test_path("../../R/app_registry.R"))
source(testthat::test_path("../../R/app_adapters.R"))
source(testthat::test_path("../../R/app_graph_builders.R"))
source(testthat::test_path("../../R/app_graph_inspect.R"))
source(testthat::test_path("../../R/app_topology_adapters.R"))
source(testthat::test_path("../../R/app_compare_environment.R"))
source(testthat::test_path("../../R/app_gallery_presets.R"))
source(testthat::test_path("../../inst/app/modules/mod_visual_lab.R"))

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
  expect_true(all(c(
    "gallery_tripartite_graph",
    "gallery_quadripartite_graph",
    "gallery_pentapartite_graph",
    "gallery_directed_tree_graph"
  ) %in% listed$name))

  tripartite <- gallery_registry_item_by_name(registry, "gallery_tripartite_graph")
  dendrogram <- gallery_registry_item_by_name(registry, "gallery_directed_tree_graph")
  expect_equal(length(unique(igraph::vertex_attr(tripartite$data, "Modularity"))), 3)
  expect_true(igraph::is_directed(dendrogram$data))
  expect_true(all(c("node", "type", "node_size") %in% igraph::vertex_attr_names(dendrogram$data)))
})

test_that("gallery directed starter renders the dendrogram layout", {
  registry <- registry_new()
  register_gallery_examples(registry, root = testthat::test_path("../.."))
  graph_item <- gallery_registry_item_by_name(registry, "gallery_directed_tree_graph")

  params <- visual_lab_params("dendrogram", "order", FALSE, "two_column", 18, 0.4, 1, 1, 10, FALSE, FALSE, 1115)
  result <- safe_plot_ggnetview(graph_item$data, params = params)

  expect_true(isTRUE(result$ok), info = result$trace %||% result$message)
  expect_silent(ggplot2::ggplot_build(result$value))
})

test_that("gallery recipe manifest exposes one-click workflows", {
  recipes <- gallery_recipe_manifest()

  expect_true(all(c("recipe", "label", "output_type") %in% names(recipes)))
  expect_true(all(c(
    "network_plot_circle",
    "grouped_network_plot",
    "graph_info_topology",
    "environment_heatmap",
    "mantel_pairwise",
    "multi_network_compare",
    "triple_environment_heatmap",
    "multi_omics_network",
    "multi_omics_double_matrix",
    "multi_omics_environment_blocks",
    "environment_collapsed_core",
    "environment_arc_collapsed_core"
  ) %in% recipes$recipe))
})

test_that("gallery recipes register reproducible outputs", {
  registry <- registry_new()
  register_gallery_examples(registry, root = testthat::test_path("../.."))

  result <- run_gallery_recipe(registry, "network_plot_circle")
  expect_true(isTRUE(result$ok), info = result$trace %||% result$message)
  expect_true(length(result$value$items) >= 1L)

  grouped <- run_gallery_recipe(registry, "grouped_network_plot")
  expect_true(isTRUE(grouped$ok), info = grouped$trace %||% grouped$message)

  graph_info <- run_gallery_recipe(registry, "graph_info_topology")
  expect_true(isTRUE(graph_info$ok), info = graph_info$trace %||% graph_info$message)

  environment <- run_gallery_recipe(registry, "environment_heatmap")
  expect_true(isTRUE(environment$ok), info = environment$trace %||% environment$message)

  mantel <- run_gallery_recipe(registry, "mantel_pairwise")
  expect_true(isTRUE(mantel$ok), info = mantel$trace %||% mantel$message)

  compare <- run_gallery_recipe(registry, "multi_network_compare")
  expect_true(isTRUE(compare$ok), info = compare$trace %||% compare$message)

  triple <- run_gallery_recipe(registry, "triple_environment_heatmap")
  expect_true(isTRUE(triple$ok), info = triple$trace %||% triple$message)

  multi_omics <- run_gallery_recipe(registry, "multi_omics_network")
  expect_true(isTRUE(multi_omics$ok), info = multi_omics$trace %||% multi_omics$message)

  double_omics <- run_gallery_recipe(registry, "multi_omics_double_matrix")
  expect_true(isTRUE(double_omics$ok), info = double_omics$trace %||% double_omics$message)

  block_omics <- run_gallery_recipe(registry, "multi_omics_environment_blocks")
  expect_true(isTRUE(block_omics$ok), info = block_omics$trace %||% block_omics$message)

  collapsed_core <- run_gallery_recipe(registry, "environment_collapsed_core")
  expect_true(isTRUE(collapsed_core$ok), info = collapsed_core$trace %||% collapsed_core$message)

  arc_collapsed_core <- run_gallery_recipe(registry, "environment_arc_collapsed_core")
  expect_true(isTRUE(arc_collapsed_core$ok), info = arc_collapsed_core$trace %||% arc_collapsed_core$message)

  listed <- shiny::isolate(registry_list(registry))
  expect_true(any(listed$name == "gallery_recipe_circle_plot"))
  expect_true(any(listed$name == "gallery_recipe_grouped_network_plot"))
  expect_true(any(listed$name == "gallery_recipe_grouped_network_groups"))
  expect_true(any(listed$name == "gallery_recipe_graph_info"))
  expect_true(any(listed$name == "gallery_recipe_network_topology"))
  expect_true(any(listed$name == "gallery_recipe_environment_heatmap"))
  expect_true(any(listed$name == "gallery_recipe_environment_stats"))
  expect_true(any(listed$name == "gallery_recipe_mantel_pairwise"))
  expect_true(any(listed$name == "gallery_recipe_multi_network_compare"))
  expect_true(any(listed$name == "gallery_recipe_multi_network_links"))
  expect_true(any(listed$name == "gallery_recipe_triple_environment_heatmap"))
  expect_true(any(listed$name == "gallery_recipe_triple_environment_nodes"))
  expect_true(any(listed$name == "gallery_recipe_multi_omics_graph"))
  expect_true(any(listed$name == "gallery_recipe_multi_omics_plot"))
  expect_true(any(listed$name == "gallery_recipe_multi_omics_double_graph"))
  expect_true(any(listed$name == "gallery_recipe_multi_omics_double_plot"))
  expect_true(any(listed$name == "gallery_recipe_multi_omics_environment_heatmap"))
  expect_true(any(listed$name == "gallery_recipe_multi_omics_environment_stats"))
  expect_true(any(listed$name == "gallery_recipe_multi_omics_environment_report"))
  expect_true(any(listed$name == "gallery_recipe_environment_collapsed_core_heatmap"))
  expect_true(any(listed$name == "gallery_recipe_environment_collapsed_core_stats"))
  expect_true(any(listed$name == "gallery_recipe_environment_arc_collapsed_core_heatmap"))
  expect_true(any(listed$name == "gallery_recipe_environment_arc_collapsed_core_stats"))
})
