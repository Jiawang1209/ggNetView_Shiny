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
  expect_equal(registry_count(registry), 1L)
  expect_equal(registry_get(registry, item$id)$type, "matrix")
  expect_equal(nrow(registry_list(registry)), 1L)

  registry_delete(registry, item$id)
  expect_equal(registry_count(registry), 0L)
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
