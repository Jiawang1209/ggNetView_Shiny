source(testthat::test_path("../../R/app_validation.R"))
source(testthat::test_path("../../R/app_registry.R"))
source(testthat::test_path("../../R/app_adapters.R"))
source(testthat::test_path("../../R/app_graph_builders.R"))
source(testthat::test_path("../../R/app_exports.R"))

test_that("graph export formats include graph-specific artifacts", {
  formats <- export_formats_for_type("graph")
  expect_true(all(c("rds", "nodes_csv", "edges_csv", "adjacency_csv", "params_json") %in% formats))
  expect_false(any(c("png", "pdf") %in% formats))
})

test_that("result export formats include table artifacts", {
  formats <- export_formats_for_type("result")
  expect_true(all(c("csv", "rds", "params_json") %in% formats))
})

test_that("plot export formats remain plot-only for images", {
  formats <- export_formats_for_type("plot")
  expect_true(all(c("png", "pdf") %in% formats))
})

test_that("graph CSV writers export nodes, edges, and adjacency", {
  graph <- igraph::make_ring(3)
  graph <- igraph::set_vertex_attr(graph, "name", value = c("A", "B", "C"))
  graph <- igraph::set_edge_attr(graph, "weight", value = c(0.2, 0.4, 0.6))

  nodes_path <- tempfile(fileext = ".csv")
  edges_path <- tempfile(fileext = ".csv")
  adjacency_path <- tempfile(fileext = ".csv")

  write_graph_nodes_csv(graph, nodes_path)
  write_graph_edges_csv(graph, edges_path)
  write_graph_adjacency_csv(graph, adjacency_path)

  expect_equal(nrow(utils::read.csv(nodes_path)), 3L)
  expect_equal(nrow(utils::read.csv(edges_path)), 3L)
  expect_equal(nrow(utils::read.csv(adjacency_path)), 3L)
})

test_that("workflow replay plan identifies graph builder outputs", {
  manifest <- list(
    app = "ggNetView Shiny",
    items = list(
      list(id = "obj_0001", name = "matrix_a", type = "matrix", source = "", params = list()),
      list(
        id = "obj_0002",
        name = "matrix_graph",
        type = "graph",
        source = "obj_0001",
        params = list(builder = "matrix", source_ids = "obj_0001")
      )
    )
  )

  plan <- workflow_replay_plan(manifest)

  expect_true(all(c("builder", "replay_reason") %in% names(plan)))
  expect_equal(plan$status[plan$id == "obj_0002"], "builder-output-needs-rerun")
  expect_equal(plan$builder[plan$id == "obj_0002"], "matrix")
})

test_that("workflow replay reruns graph builders when sources are present", {
  registry <- registry_new()
  mat <- utils::read.csv(
    testthat::test_path("../../inst/extdata/phase2_example_matrix.csv"),
    row.names = 1,
    check.names = FALSE
  )
  source_item <- registry_add(registry, name = "matrix_a", type = "matrix", data = mat)
  manifest_item <- list(
    id = "obj_9999",
    name = "replayed_matrix_graph",
    type = "graph",
    source = source_item$id,
    params = list(
      builder = "matrix",
      source_ids = source_item$id,
      method = "cor",
      cor.method = "pearson",
      r.threshold = 0.2,
      p.threshold = 1
    )
  )

  results <- workflow_replay_graph_builders(registry, list(manifest_item))

  expect_length(results, 1L)
  expect_true(isTRUE(results[[1]]$ok), info = results[[1]]$trace %||% results[[1]]$message)
  expect_equal(shiny::isolate(registry_count(registry)), 2L)
  replayed <- shiny::isolate(registry$items[[results[[1]]$value$id]])
  expect_s3_class(replayed$data, "igraph")
  expect_equal(replayed$params$builder, "matrix")
  expect_equal(replayed$params$source_ids, source_item$id)
})

test_that("workflow replay explains missing graph builder sources", {
  registry <- registry_new()
  manifest_item <- list(
    id = "obj_9999",
    name = "missing_source_graph",
    type = "graph",
    source = "obj_missing",
    params = list(builder = "matrix", source_ids = "obj_missing")
  )

  results <- workflow_replay_graph_builders(registry, list(manifest_item))

  expect_length(results, 1L)
  expect_false(isTRUE(results[[1]]$ok))
  expect_match(results[[1]]$message, "source object is not available")
})

test_that("workflow manifest restores snapshotted inputs for graph-builder replay", {
  registry <- registry_new()
  mat <- utils::read.csv(
    testthat::test_path("../../inst/extdata/phase2_example_matrix.csv"),
    row.names = 1,
    check.names = FALSE
  )
  source_item <- registry_add(registry, name = "matrix_a", type = "matrix", data = mat)
  graph <- safe_graph_builder(
    mode = "matrix",
    inputs = list(matrix = mat),
    params = list(method = "cor", cor.method = "pearson", r.threshold = 0.2, p.threshold = 1)
  )$value
  registry_add(
    registry,
    name = "matrix_graph",
    type = "graph",
    data = graph,
    source = source_item$id,
    params = list(
      builder = "matrix",
      source_ids = source_item$id,
      method = "cor",
      cor.method = "pearson",
      r.threshold = 0.2,
      p.threshold = 1
    )
  )

  path <- tempfile(fileext = ".json")
  write_workflow_manifest(registry, path)
  manifest <- read_workflow_manifest(path)

  empty_registry <- registry_new()
  restored <- workflow_restore_manifest_inputs(empty_registry, manifest)
  replay_results <- workflow_replay_graph_builders(empty_registry, manifest)

  expect_true(isTRUE(restored$ok), info = restored$message)
  expect_equal(restored$value$restored, 1L)
  expect_false(is.null(shiny::isolate(registry_get(empty_registry, source_item$id))))
  expect_length(replay_results, 1L)
  expect_true(isTRUE(replay_results[[1]]$ok), info = replay_results[[1]]$trace %||% replay_results[[1]]$message)
  expect_equal(shiny::isolate(registry_count(empty_registry)), 2L)
})
