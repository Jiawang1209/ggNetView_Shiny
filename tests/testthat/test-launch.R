test_that("launch_ggNetView exists and points at bundled app", {
  expect_true(exists("launch_ggNetView", mode = "function"))
  app_dir <- system.file("app", package = "ggNetView")
  expect_true(nzchar(app_dir))
})

test_that("root and bundled app files exist", {
  expect_true(file.exists(test_path("../../app.R")))
  expect_true(file.exists(test_path("../../inst/app/app.R")))
  expect_true(file.exists(test_path("../../inst/app/global.R")))
})
