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
