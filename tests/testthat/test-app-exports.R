source(testthat::test_path("../../R/app_registry.R"))
source(testthat::test_path("../../R/app_exports.R"))

test_that("write_registry_table writes CSV", {
  path <- tempfile(fileext = ".csv")
  write_registry_table(data.frame(a = 1, b = 2), path)

  expect_true(file.exists(path))
  expect_equal(nrow(utils::read.csv(path)), 1L)
})

test_that("write_registry_table preserves matrix row names in id column", {
  path <- tempfile(fileext = ".csv")
  mat <- matrix(c(1, 2, 3, 4), nrow = 2, dimnames = list(c("sample_a", "sample_b"), c("x", "y")))
  write_registry_table(mat, path)

  exported <- utils::read.csv(path, check.names = FALSE)
  expect_equal(names(exported)[1], "id")
  expect_equal(exported$id, c("sample_a", "sample_b"))
  expect_equal(exported$x, c(1, 2))
})

test_that("write_registry_object writes RDS round trip", {
  path <- tempfile(fileext = ".rds")
  obj <- list(alpha = 0.05, values = c("a", "b"))
  write_registry_object(obj, path)

  expect_true(file.exists(path))
  expect_equal(readRDS(path), obj)
})

test_that("write_registry_params writes JSON", {
  path <- tempfile(fileext = ".json")
  write_registry_params(list(alpha = 0.05, method = "spearman"), path)

  expect_true(file.exists(path))
  txt <- readLines(path, warn = FALSE)
  expect_true(any(grepl("alpha", txt)))
})

test_that("write_registry_params preserves nested and NULL values", {
  path <- tempfile(fileext = ".json")
  params <- list(alpha = 0.05, nested = list(method = "spearman", missing = NULL))
  write_registry_params(params, path)

  parsed <- jsonlite::read_json(path, simplifyVector = TRUE)
  expect_equal(parsed$nested$method, "spearman")
  expect_null(parsed$nested$missing)
})

test_that("write_workflow_manifest exports registry provenance as JSON", {
  registry <- registry_new()
  registry_add(
    registry,
    name = "gallery_matrix",
    type = "matrix",
    data = matrix(1:4, nrow = 2),
    source = "phase2_example_matrix.csv",
    params = list(recipe = "manual_starter")
  )
  registry_add(
    registry,
    name = "gallery_recipe_circle_plot",
    type = "plot",
    data = list(class = "plot-placeholder"),
    source = "obj_0001",
    params = list(recipe = "network_plot_circle", layout = "circle"),
    warnings = "small example"
  )

  path <- tempfile(fileext = ".json")
  write_workflow_manifest(registry, path)

  manifest <- jsonlite::read_json(path, simplifyVector = TRUE)
  expect_equal(manifest$app, "ggNetView Shiny")
  expect_true("generated_at" %in% names(manifest))
  expect_equal(manifest$item_count, 2L)
  expect_equal(nrow(manifest$items), 2L)
  expect_true(all(c("id", "name", "type", "source", "summary", "params", "warnings") %in% names(manifest$items)))
  expect_equal(manifest$items$params$recipe[[2]], "network_plot_circle")
  expect_equal(manifest$items$params$layout[[2]], "circle")
  expect_equal(manifest$items$warnings[[2]], "small example")
})

test_that("write_plot_png writes extensionless download path", {
  path <- tempfile()
  plot <- ggplot2::ggplot(data.frame(x = 1:3, y = 1:3), ggplot2::aes(x, y)) +
    ggplot2::geom_point()
  write_plot_png(plot, path, width = 2, height = 2, dpi = 72)

  expect_true(file.exists(path))
  expect_gt(file.info(path)$size, 0)
})

test_that("write_plot_pdf writes extensionless download path", {
  path <- tempfile()
  plot <- ggplot2::ggplot(data.frame(x = 1:3, y = 1:3), ggplot2::aes(x, y)) +
    ggplot2::geom_point()
  write_plot_pdf(plot, path, width = 2, height = 2)

  expect_true(file.exists(path))
  expect_gt(file.info(path)$size, 0)
})

test_that("global helper bridge includes export helpers", {
  global_r <- readLines(test_path("../../inst/app/global.R"), warn = FALSE)
  expected <- c(
    "write_registry_table",
    "write_registry_object",
    "write_registry_params",
    "write_workflow_manifest",
    "write_plot_png",
    "write_plot_pdf"
  )

  expect_true(all(vapply(expected, function(name) any(grepl(name, global_r, fixed = TRUE)), logical(1))))
})
