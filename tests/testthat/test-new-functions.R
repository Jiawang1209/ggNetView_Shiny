source(testthat::test_path("../../R/app_validation.R"))
source(testthat::test_path("../../R/app_adapters.R"))
source(testthat::test_path("../../R/app_compare_environment.R"))

test_that("safe_link_heatmap_adaptive resolves gglink_heatmaps_2 and returns a result object", {
  set.seed(1)
  spec <- as.data.frame(matrix(runif(6 * 8), nrow = 6,
                               dimnames = list(paste0("OTU", 1:6), paste0("S", 1:8))))
  env <- as.data.frame(matrix(runif(2 * 8), nrow = 2,
                              dimnames = list(c("pH", "temp"), paste0("S", 1:8))))

  result <- safe_link_heatmap_adaptive(env = env, spec = spec, params = list())

  expect_s3_class(result, "ggnetview_app_result")
  if (result$ok) {
    expect_true(is.list(result$value))
  } else {
    expect_true(is.character(result$message) && nzchar(result$message))
  }
})
