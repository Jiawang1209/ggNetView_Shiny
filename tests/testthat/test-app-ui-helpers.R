test_that("ui_empty_state renders a tagged empty-state card", {
  source(test_path("../../R/app_ui_helpers.R"))
  tag <- ui_empty_state(icon = "inbox", title = "No data", hint = "Load something first")
  html <- as.character(tag)
  expect_match(html, "ggnv-empty-state", fixed = TRUE)
  expect_match(html, "No data", fixed = TRUE)
  expect_match(html, "Load something first", fixed = TRUE)
})

test_that("ggnv_value_box returns a bslib value_box", {
  source(test_path("../../R/app_ui_helpers.R"))
  vb <- ggnv_value_box("Schneider R", "0.62", icon = "shield-check")
  html <- as.character(vb)
  expect_match(html, "Schneider R", fixed = TRUE)
  expect_match(html, "0.62", fixed = TRUE)
})

test_that("dt_table builds a datatable with export buttons", {
  source(test_path("../../R/app_ui_helpers.R"))
  dt <- dt_table(data.frame(a = 1:3, b = c(1.111, 2.222, 3.333)))
  expect_s3_class(dt, "datatables")
  expect_true(!is.null(dt$x$options$buttons))
  expect_true(all(c("copy", "csv", "excel") %in% unlist(dt$x$options$buttons)))
})
