test_that("launch_ggNetView exists and points at bundled app", {
  expect_true(exists("launch_ggNetView", mode = "function"))
  app_dir <- system.file("app", package = "ggNetView")
  if (!nzchar(app_dir)) {
    app_dir <- test_path("../../inst/app")
  }
  expect_true(nzchar(app_dir))
  expect_true(dir.exists(app_dir))
})

test_that("root and bundled app files exist", {
  expect_true(file.exists(test_path("../../app.R")))
  expect_true(file.exists(test_path("../../inst/app/app.R")))
  expect_true(file.exists(test_path("../../inst/app/global.R")))
  expect_true(file.exists(test_path("../../inst/app/ui.R")))
  expect_true(file.exists(test_path("../../inst/app/server.R")))
})

test_that("bundled app shell can instantiate", {
  app_dir <- test_path("../../inst/app")
  old_wd <- setwd(app_dir)
  on.exit(setwd(old_wd), add = TRUE)

  source("global.R", local = TRUE)
  source("ui.R", local = TRUE)
  source("server.R", local = TRUE)

  expect_s3_class(shiny::shinyApp(ui, server), "shiny.appobj")
})
