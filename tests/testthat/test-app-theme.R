test_that("app_module_palette returns 8 named hex colors", {
  source(test_path("../../R/app_theme.R"))
  pal <- app_module_palette()
  expect_length(pal, 8)
  expect_true(!is.null(names(pal)))
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", pal)))
  expect_true(all(c("red", "teal", "purple") %in% names(pal)))
})

test_that("app_bs_theme returns a bs_theme using the brand magenta", {
  source(test_path("../../R/app_theme.R"))
  theme <- app_bs_theme()
  expect_s3_class(theme, "bs_theme")
  vars <- bslib::bs_get_variables(theme, "primary")
  expect_equal(tolower(unname(vars[["primary"]])), "#ae017e")
})
