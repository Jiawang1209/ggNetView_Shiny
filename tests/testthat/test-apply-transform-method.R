test_that("rrarefy_relative uses a single rarefaction result for normalization", {
  skip_if_not_installed("vegan")

  mat <- matrix(
    c(
      10, 7, 5,
      4, 8, 6,
      3, 2, 9
    ),
    nrow = 3,
    byrow = TRUE
  )

  colnames(mat) <- c("S1", "S2", "S3")
  rownames(mat) <- c("A", "B", "C")

  set.seed(123)
  transformed <- ggNetView:::apply_transform_method(mat, "rrarefy_relative")

  expect_equal(unname(colSums(transformed)), rep(1, ncol(transformed)))
  expect_true(all(is.finite(transformed)))
  expect_true(all(transformed >= 0))
})

test_that("log transforms fail fast for invalid values", {
  mat <- matrix(c(-2, 1, 3, 4), nrow = 2)

  expect_error(
    ggNetView:::apply_transform_method(mat, "log2"),
    "greater than -1"
  )

  expect_error(
    ggNetView:::apply_transform_method(mat, "log10"),
    "greater than -1"
  )

  expect_error(
    ggNetView:::apply_transform_method(mat, "ln"),
    "greater than -1"
  )
})
