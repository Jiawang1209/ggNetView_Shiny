test_that("write_registry_table writes CSV", {
  path <- tempfile(fileext = ".csv")
  write_registry_table(data.frame(a = 1, b = 2), path)

  expect_true(file.exists(path))
  expect_equal(nrow(utils::read.csv(path)), 1L)
})

test_that("write_registry_params writes JSON", {
  path <- tempfile(fileext = ".json")
  write_registry_params(list(alpha = 0.05, method = "spearman"), path)

  expect_true(file.exists(path))
  txt <- readLines(path, warn = FALSE)
  expect_true(any(grepl("alpha", txt)))
})

test_that("global helper bridge includes export helpers", {
  global_r <- readLines(test_path("../../inst/app/global.R"), warn = FALSE)
  expected <- c(
    "write_registry_table",
    "write_registry_object",
    "write_registry_params",
    "write_plot_png",
    "write_plot_pdf"
  )

  expect_true(all(vapply(expected, function(name) any(grepl(name, global_r, fixed = TRUE)), logical(1))))
})
