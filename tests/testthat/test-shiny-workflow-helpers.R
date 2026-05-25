source(test_path("../../R/app_validation.R"))
source(test_path("../../R/app_registry.R"))
source(test_path("../../inst/app/modules/mod_data_hub.R"))

test_that("example matrix path resolves", {
  path <- app_example_matrix_path()
  expect_true(file.exists(path))
  expect_match(path, "example_matrix[.]csv$")
})

test_that("preview table limits rows and columns", {
  x <- data.frame(a = 1:20, b = 21:40, c = 41:60)
  preview <- preview_table(x, max_rows = 5, max_cols = 2)
  expect_equal(nrow(preview), 5L)
  expect_equal(ncol(preview), 2L)
  expect_equal(names(preview), c("a", "b"))
})

test_that("object names are normalized and made unique", {
  registry <- registry_new()
  registry_add(registry, name = "example_matrix", type = "matrix", data = matrix(1, nrow = 1))
  registry_add(registry, name = "example_matrix_2", type = "matrix", data = matrix(1, nrow = 1))

  expect_equal(normalize_object_name("  ", fallback = "uploaded_matrix"), "uploaded_matrix")
  expect_equal(normalize_object_name("bad name.csv"), "bad_name.csv")
  expect_equal(unique_registry_name(registry, "example_matrix"), "example_matrix_3")
})

test_that("validated upload values reject invalid matrix previews", {
  invalid <- matrix(1:4, nrow = 2, dimnames = list(c("sample", "sample"), c("a", "b")))
  prepared <- validated_upload_value(invalid)

  expect_equal(prepared$type, "matrix")
  expect_false(prepared$validation$ok)
})

source(test_path("../../inst/app/modules/mod_graph_builder.R"))

test_that("graph builder params match ggNetView matrix workflow", {
  params <- graph_builder_params(
    builder = "matrix",
    method = "cor",
    cor_method = "pearson",
    proc = "none",
    r_threshold = 0.1,
    p_threshold = 1,
    module_method = "Fast_greedy"
  )

  expect_equal(params$method, "cor")
  expect_equal(params$cor.method, "pearson")
  expect_equal(params$proc, "none")
  expect_equal(params$r.threshold, 0.1)
  expect_equal(params$p.threshold, 1)
  expect_equal(params$module.method, "Fast_greedy")
})

test_that("graph builder params are empty for edge table builder", {
  params <- graph_builder_params(builder = "edge_table")
  expect_equal(params, list())
})

test_that("graph builder validates source type matches", {
  expect_equal(
    unname(builder_choices_for_type(NULL)),
    c("matrix", "adjacency", "edge_table")
  )
  expect_true(builder_matches_source_type("matrix", "matrix"))
  expect_true(builder_matches_source_type("adjacency", "adjacency"))
  expect_true(builder_matches_source_type("edge_table", "edge_table"))
  expect_false(builder_matches_source_type("matrix", "edge_table"))
  expect_false(builder_matches_source_type("edge_table", "matrix"))
  expect_false(builder_matches_source_type("Louvain", "matrix"))
})

source(test_path("../../inst/app/modules/mod_topology_results.R"))

test_that("topology robustness table extracts optional robustness payload", {
  robustness <- data.frame(step = 1:3, score = c(1, 0.7, 0.2))
  payload <- list(
    topology = data.frame(metric = "nodes", value = 3),
    Robustness = robustness
  )

  expect_equal(topology_robustness_table(payload), robustness)
  expect_equal(topology_robustness_table(data.frame(a = 1)), data.frame())
})

test_that("empty result table is safe for stale topology clearing", {
  empty <- empty_result_table()
  expect_s3_class(empty, "data.frame")
  expect_equal(nrow(empty), 0L)
})

source(test_path("../../inst/app/modules/mod_visual_lab.R"))

test_that("visual lab params are stable and JSON-friendly", {
  params <- visual_lab_params(
    layout = "nicely",
    show_labels = TRUE,
    label_layout = "two_column",
    label_wrap_width = 18,
    bandwidth_scale = 1
  )

  expect_equal(params$layout, "nicely")
  expect_true(params$label)
  expect_equal(params$label_layout, "two_column")
  expect_equal(params$label_wrap_width, 18)
  expect_equal(params$bandwidth_scale, 1)
})

test_that("visual lab params normalize invalid numeric inputs", {
  params <- visual_lab_params(
    layout = NULL,
    show_labels = FALSE,
    label_layout = NULL,
    label_wrap_width = NULL,
    bandwidth_scale = "not-a-number"
  )

  expect_equal(params$layout, "nicely")
  expect_equal(params$label_layout, "two_column")
  expect_equal(params$label_wrap_width, 18)
  expect_equal(params$bandwidth_scale, 1)

  clamped <- visual_lab_params("fr", FALSE, "label_circle", 999, -1)
  expect_equal(clamped$label_wrap_width, 80)
  expect_equal(clamped$bandwidth_scale, 1)
})

test_that("visual lab params JSON is stable before drawing", {
  params <- visual_lab_params("nicely", FALSE, "two_column", 18, 1)
  json <- visual_lab_params_json(params)

  expect_match(json, '"layout": "nicely"', fixed = TRUE)
  expect_match(json, '"bandwidth_scale": 1', fixed = TRUE)
})

source(test_path("../../inst/app/modules/mod_export_center.R"))

test_that("registry manifest captures export metadata", {
  registry <- registry_new()
  registry_add(registry, name = "m", type = "matrix", data = matrix(1, nrow = 1), source = "unit")
  registry_add(registry, name = "g", type = "graph", data = list(), source = "obj_1", params = list(builder = "matrix"))

  manifest <- registry_manifest(registry)

  expect_true(all(c("id", "name", "type", "source", "created_at") %in% names(manifest)))
  expect_equal(nrow(manifest), 2L)
})

test_that("registry manifest keeps empty schema", {
  manifest <- registry_manifest(registry_new())

  expect_equal(names(manifest), c("id", "name", "type", "source", "created_at"))
  expect_equal(nrow(manifest), 0L)
})

test_that("plot downloads are limited to plot objects", {
  expect_true(is_plot_item(list(type = "plot")))
  expect_false(is_plot_item(list(type = "matrix")))
  expect_false(is_plot_item(NULL))
})
