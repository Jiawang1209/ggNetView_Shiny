test_that("registry can add, list, get, and delete objects", {
  registry <- registry_new()
  item <- registry_add(
    registry,
    name = "example matrix",
    type = "matrix",
    data = matrix(1:4, nrow = 2),
    source = "unit-test",
    params = list(alpha = 0.05),
    warnings = "small example"
  )

  expect_match(item$id, "^obj_")
  expect_equal(item$name, "example matrix")
  shiny::isolate({
    expect_equal(registry_count(registry), 1L)
    expect_equal(registry_get(registry, item$id)$type, "matrix")
    expect_equal(nrow(registry_list(registry)), 1L)
  })

  registry_delete(registry, item$id)
  shiny::isolate(expect_equal(registry_count(registry), 0L))
})

test_that("registry summary records matrix dimensions", {
  registry <- registry_new()
  item <- registry_add(
    registry,
    name = "m",
    type = "matrix",
    data = matrix(1:9, nrow = 3)
  )

  expect_equal(item$summary$rows, 3L)
  expect_equal(item$summary$cols, 3L)
})

test_that("app_success returns successful app result", {
  result <- app_success(value = list(answer = 42), warnings = "kept")

  expect_s3_class(result, "ggnetview_app_result")
  expect_true(result$ok)
  expect_equal(result$value, list(answer = 42))
  expect_equal(result$warnings, "kept")
})

test_that("app_failure returns failed app result", {
  trace <- list(call = "unit-test")
  result <- app_failure("failed", trace = trace)

  expect_s3_class(result, "ggnetview_app_result")
  expect_false(result$ok)
  expect_equal(result$message, "failed")
  expect_equal(result$trace, trace)
})

test_that("registry list filters by type", {
  registry <- registry_new()
  registry_add(registry, name = "m", type = "matrix", data = matrix(1, nrow = 1))
  graph_item <- registry_add(registry, name = "g", type = "graph", data = list(nodes = 1))

  shiny::isolate({
    listed <- registry_list(registry, type = "graph")
    expect_equal(nrow(listed), 1L)
    expect_equal(listed$id, graph_item$id)
    expect_equal(listed$type, "graph")
  })
})

test_that("registry choices returns named ids and filters by type", {
  registry <- registry_new()
  matrix_item <- registry_add(registry, name = "m", type = "matrix", data = matrix(1, nrow = 1))
  graph_item <- registry_add(registry, name = "g", type = "graph", data = list(nodes = 1))

  shiny::isolate({
    choices <- registry_choices(registry)
    expect_equal(unname(choices), c(matrix_item$id, graph_item$id))
    expect_equal(names(choices), c("m [matrix]", "g [graph]"))

    graph_choices <- registry_choices(registry, type = "graph")
    expect_equal(unname(graph_choices), graph_item$id)
    expect_equal(names(graph_choices), "g [graph]")
  })
})

test_that("registry choices by type returns only requested types", {
  registry <- registry_new()
  matrix_item <- registry_add(registry, name = "m", type = "matrix", data = matrix(1, nrow = 1))
  adjacency_item <- registry_add(registry, name = "a", type = "adjacency", data = matrix(1, nrow = 1))
  registry_add(registry, name = "g", type = "graph", data = list(nodes = 1))

  shiny::isolate({
    choices <- registry_choices_by_type(registry, c("matrix", "adjacency"))
    expect_equal(unname(choices), c(matrix_item$id, adjacency_item$id))
    expect_equal(names(choices), c("m [matrix]", "a [adjacency]"))

    expect_equal(registry_choices_by_type(registry, "missing"), stats::setNames(character(), character()))
  })
})

test_that("registry_count is reactive-aware", {
  server <- function(input, output, session) {
    registry <- registry_new()
    count <- shiny::reactive(registry_count(registry))
  }

  shiny::testServer(server, {
    expect_equal(count(), 0L)
    registry_add(registry, name = "m", type = "matrix", data = matrix(1, nrow = 1))
    expect_equal(count(), 1L)
  })
})
