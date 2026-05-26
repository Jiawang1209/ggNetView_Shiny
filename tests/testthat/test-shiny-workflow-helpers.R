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

source(test_path("../../R/app_graph_builders.R"))
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

  expect_equal(params$transfrom.method, "none")
  expect_equal(params$method, "cor")
  expect_equal(params$cor.method, "pearson")
  expect_equal(params$proc, "none")
  expect_equal(params$r.threshold, 0.1)
  expect_equal(params$p.threshold, 1)
  expect_equal(params$module.method, "Fast_greedy")
})

test_that("graph builder params are empty for edge table builder", {
  params <- graph_builder_params(builder = "edge_table")
  expect_equal(params$module.method, "Fast_greedy")
})

test_that("graph builder params keep module method for supported builders", {
  supported <- c("edge_table", "adjacency", "double_matrix", "multi_matrix", "consensus")

  for (builder in supported) {
    params <- graph_builder_params(builder = builder, module_method = "Walktrap")
    expect_equal(params$module.method, "Walktrap", info = builder)
  }
})

test_that("graph builder registry params keep replay metadata", {
  params <- graph_builder_registry_params(
    builder = "double_matrix",
    params = list(method = "cor"),
    source_ids = c("obj_0001", "obj_0002"),
    module_id = "obj_0003"
  )

  expect_equal(params$builder, "double_matrix")
  expect_equal(params$source_ids, c("obj_0001", "obj_0002"))
  expect_equal(params$module_id, "obj_0003")
  expect_equal(params$method, "cor")
})

test_that("graph builder validates source type matches", {
  expect_equal(
    unname(builder_choices_for_type(NULL)),
    unname(graph_builder_modes())
  )
  expect_true(builder_matches_source_type("matrix", "matrix"))
  expect_true(builder_matches_source_type("matrix_rmt", "matrix"))
  expect_true(builder_matches_source_type("double_matrix", "matrix"))
  expect_true(builder_matches_source_type("multi_matrix", "matrix"))
  expect_true(builder_matches_source_type("adjacency", "adjacency"))
  expect_true(builder_matches_source_type("edge_table", "edge_table"))
  expect_true(builder_matches_source_type("wgcna_tom", "wgcna_tom"))
  expect_true(builder_matches_source_type("consensus", "graph"))
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
    layout_module = "adjacent",
    show_labels = TRUE,
    label_layout = "two_column",
    label_wrap_width = 18,
    label_outer_pad = 0.4,
    bandwidth_scale = 1,
    point_size_min = 1,
    point_size_max = 10,
    add_group_outer = TRUE,
    drop_others = FALSE,
    seed = 1115
  )

  expect_equal(params$layout, "nicely")
  expect_equal(params$layout.module, "adjacent")
  expect_true(params$label)
  expect_equal(params$label_layout, "two_column")
  expect_equal(params$label_wrap_width, 18)
  expect_equal(params$label_outer_pad, 0.4)
  expect_equal(params$bandwidth_scale, 1)
  expect_equal(params$pointsize, c(1, 10))
  expect_true(params$add_group_outer)
  expect_false(params$dropOthers)
  expect_equal(params$seed, 1115L)
})

test_that("visual lab params expose manual layout and rendering controls", {
  params <- visual_lab_params(
    layout = "WGCNA",
    layout_module = "order",
    show_labels = FALSE,
    label_layout = "label_circle",
    label_wrap_width = 24,
    label_outer_pad = 0.6,
    bandwidth_scale = 1.2,
    point_size_min = 2,
    point_size_max = 12,
    add_group_outer = TRUE,
    drop_others = TRUE,
    seed = 42,
    node_add = 9,
    ring_n = 5,
    r = 1.5,
    center = FALSE,
    shrink = 0.8,
    inner_shrink = 0.65,
    k_nn = 6,
    push_others_delta = 0.25,
    jitter = TRUE,
    jitter_sd = 0.03,
    plot_line = FALSE,
    curve = TRUE,
    curvature = 0.35,
    linealpha = 0.6,
    linecolor = "#123456",
    pointlabel = "top3",
    pointlabelsize = 4
  )

  expect_equal(params$node_add, 9)
  expect_equal(params$ring_n, 5)
  expect_equal(params$r, 1.5)
  expect_false(params$center)
  expect_equal(params$shrink, 0.8)
  expect_equal(params$inner_shrink, 0.65)
  expect_equal(params$k_nn, 6)
  expect_equal(params$push_others_delta, 0.25)
  expect_true(params$jitter)
  expect_equal(params$jitter_sd, 0.03)
  expect_false(params$plot_line)
  expect_true(params$curve)
  expect_equal(params$curvature, 0.35)
  expect_equal(params$linealpha, 0.6)
  expect_equal(params$linecolor, "#123456")
  expect_equal(params$pointlabel, "top3")
  expect_equal(params$pointlabelsize, 4)
})

test_that("visual lab params normalize invalid numeric inputs", {
  params <- visual_lab_params(NULL, NULL, FALSE, NULL, NULL, NULL, "not-a-number", NULL, NULL, FALSE, FALSE, NULL)

  expect_equal(params$layout, "nicely")
  expect_equal(params$layout.module, "adjacent")
  expect_equal(params$label_layout, "two_column")
  expect_equal(params$label_wrap_width, 18)
  expect_equal(params$label_outer_pad, 0.4)
  expect_equal(params$bandwidth_scale, 1)

  clamped <- visual_lab_params("fr", "order", FALSE, "label_circle", 999, -1, -1, -5, 999, FALSE, FALSE, -1)
  expect_equal(clamped$label_wrap_width, 80)
  expect_equal(clamped$label_outer_pad, 0.4)
  expect_equal(clamped$bandwidth_scale, 1)
  expect_equal(clamped$pointsize, c(1, 50))
  expect_equal(clamped$seed, 1115L)

  advanced <- visual_lab_params(
    "fr", "adjacent", FALSE, "two_column", 18, 0.4, 1, 1, 10, FALSE, FALSE, 1115,
    node_add = -1,
    ring_n = -1,
    r = -1,
    shrink = -1,
    inner_shrink = -1,
    k_nn = -1,
    push_others_delta = NA,
    jitter_sd = -1,
    curvature = -1,
    linealpha = 9,
    pointlabelsize = -1
  )
  expect_equal(advanced$node_add, 7L)
  expect_null(advanced$ring_n)
  expect_equal(advanced$r, 1)
  expect_equal(advanced$shrink, 1)
  expect_equal(advanced$inner_shrink, 1)
  expect_equal(advanced$k_nn, 8L)
  expect_equal(advanced$push_others_delta, 0)
  expect_equal(advanced$jitter_sd, 0.1)
  expect_equal(advanced$curvature, 0.25)
  expect_equal(advanced$linealpha, 1)
  expect_equal(advanced$pointlabelsize, 5)
})

test_that("visual lab params JSON is stable before drawing", {
  params <- visual_lab_params("nicely", "adjacent", FALSE, "two_column", 18, 0.4, 1, 1, 10, FALSE, FALSE, 1115)
  json <- visual_lab_params_json(params)

  expect_match(json, '"layout": "nicely"', fixed = TRUE)
  expect_match(json, '"layout.module": "adjacent"', fixed = TRUE)
  expect_match(json, '"bandwidth_scale": 1', fixed = TRUE)
})

test_that("visual lab exposes manual layout families", {
  choices <- visual_lab_layout_choices()
  flattened <- unname(unlist(choices))

  expect_true(all(c(
    "gephi",
    "circle_outline",
    "square2",
    "rectangle_outline",
    "rightiso_layers",
    "diamond",
    "circular_modules_gephi_layout",
    "circular_modules_equal_petal_layout",
    "circular_modules_star_layout",
    "consensus_module_equal_gephi",
    "bipartite_gephi_layout",
    "tripartite_equal_gephi_layout",
    "WGCNA"
  ) %in% flattened))
})

source(test_path("../../R/app_exports.R"))
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

  expect_null(plot_download_controls(list(type = "matrix")))
  expect_s3_class(plot_download_controls(list(type = "plot")), "shiny.tag.list")
})

test_that("object download controls use explicit selected-object labels", {
  controls <- object_download_controls()
  text <- paste(capture.output(print(controls)), collapse = "\n")

  expect_s3_class(controls, "shiny.tag.list")
  expect_match(text, "Selected Object Downloads")
  expect_match(text, "download_rds")
  expect_match(text, "Download Object RDS")
  expect_match(text, "download_csv")
  expect_match(text, "Download Object CSV")
  expect_match(text, "download_params")
  expect_match(text, "Download Parameters JSON")
})

test_that("type download controls expose graph exports", {
  controls <- type_download_controls(list(type = "graph"))
  text <- paste(capture.output(print(controls)), collapse = "\n")

  expect_s3_class(controls, "shiny.tag.list")
  expect_match(text, "Graph Downloads")
  expect_match(text, "download_nodes_csv")
  expect_match(text, "Download Nodes CSV")
  expect_match(text, "download_edges_csv")
  expect_match(text, "Download Edges CSV")
  expect_match(text, "download_adjacency_csv")
  expect_match(text, "Download Adjacency CSV")
})

test_that("type download controls group plot exports separately", {
  controls <- type_download_controls(list(type = "plot"))
  text <- paste(capture.output(print(controls)), collapse = "\n")

  expect_s3_class(controls, "shiny.tag.list")
  expect_match(text, "Plot Downloads")
  expect_match(text, "download_png")
  expect_match(text, "Download Plot PNG")
  expect_match(text, "download_pdf")
  expect_match(text, "Download Plot PDF")
})

test_that("workflow download controls use workflow-level labels", {
  controls <- workflow_download_controls()
  text <- paste(capture.output(print(controls)), collapse = "\n")

  expect_s3_class(controls, "shiny.tag.list")
  expect_match(text, "Session (&|&amp;) Workflow Downloads")
  expect_match(text, "download_manifest")
  expect_match(text, "Download Session Manifest CSV")
  expect_match(text, "download_workflow_manifest")
  expect_match(text, "Download Workflow Manifest JSON")
})

test_that("export center summarizes the selected object and formats", {
  item <- list(
    id = "obj_0009",
    name = "gallery_matrix_graph",
    type = "graph",
    source = "gallery_matrix",
    summary = list(nodes = 6L, edges = 4L),
    params = list(builder = "matrix", recipe = "network_plot_circle")
  )

  summary <- export_object_summary(item)
  expect_true(all(c("field", "value") %in% names(summary)))
  expect_true(any(summary$field == "Type" & summary$value == "graph"))
  expect_true(any(summary$field == "Source" & summary$value == "gallery_matrix"))
  expect_true(any(summary$field == "Formats" & grepl("Nodes CSV", summary$value, fixed = TRUE)))
  expect_true(any(summary$field == "Parameters" & grepl("builder", summary$value, fixed = TRUE)))

  empty_summary <- export_object_summary(NULL)
  expect_equal(nrow(empty_summary), 0L)
})
