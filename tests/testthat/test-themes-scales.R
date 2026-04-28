test_that("theme_ggnetview returns a ggplot theme", {
  thm <- theme_ggnetview()

  expect_s3_class(thm, "theme")
})

test_that("scale_color_ggnetview returns a ggplot scale", {
  sc <- scale_color_ggnetview(classes = c("M1", "M2", "M3"))

  expect_true(inherits(sc, "Scale") || inherits(sc, "ggproto"))
})

test_that("scale_fill_ggnetview returns a ggplot scale", {
  sf <- scale_fill_ggnetview(classes = c("M1", "M2", "M3"))

  expect_true(inherits(sf, "Scale") || inherits(sf, "ggproto"))
})

test_that("get_palette returns a named character vector of colours", {
  classes <- c("Module1", "Module2", "Module3")
  pal <- get_palette(classes)

  expect_true(is.character(pal))
  expect_equal(length(pal), 3)
  expect_equal(names(pal), classes)
  expect_true(all(grepl("^#", pal)))
})

test_that("get_palette assigns grey to Others", {
  classes <- c("M1", "M2", "Others")
  pal <- get_palette(classes)

  expect_true("Others" %in% names(pal))
})
