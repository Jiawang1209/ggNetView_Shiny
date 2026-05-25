test_that("read_user_table reads first column as row names", {
  path <- test_path("../../inst/extdata/example_matrix.csv")
  tbl <- read_user_table(path)

  expect_true(is.data.frame(tbl))
  expect_equal(rownames(tbl)[1], "Taxon_A")
  expect_equal(ncol(tbl), 5L)
})

test_that("detect_upload_type identifies numeric matrix", {
  mat <- data.frame(S1 = c(1, 2), S2 = c(3, 4), row.names = c("A", "B"))
  expect_equal(detect_upload_type(mat), "matrix")
})

test_that("validate_matrix_like rejects non-numeric cells", {
  mat <- data.frame(S1 = c("x", "y"), S2 = c("1", "2"), row.names = c("A", "B"))
  result <- validate_matrix_like(mat)

  expect_false(result$ok)
  expect_match(result$message, "numeric")
})

test_that("validate_matrix_like converts numeric data frames to matrix", {
  mat <- data.frame(S1 = c(1, 2), S2 = c(3, 4), row.names = c("A", "B"))
  result <- validate_matrix_like(mat)

  expect_true(result$ok)
  expect_true(is.matrix(result$value))
  expect_equal(storage.mode(result$value), "double")
})
