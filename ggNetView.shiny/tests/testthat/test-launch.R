test_that("launch_ggNetView is exported and is a function", {
  expect_true(is.function(launch_ggNetView))
})

test_that("ggNetViewApp can be located", {
  expect_true(nzchar(system.file("app", package = "ggNetView.shiny")))
})

test_that("inst/app contains required files", {
  app_dir <- system.file("app", package = "ggNetView.shiny")
  for (f in c("global.R", "ui.R", "server.R", "app.R")) {
    expect_true(file.exists(file.path(app_dir, f)),
                info = paste("missing", f))
  }
})
