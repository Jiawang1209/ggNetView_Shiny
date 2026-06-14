source(testthat::test_path("../../R/app_validation.R"))
source(testthat::test_path("../../R/app_adapters.R"))

perturbation_fns <- c(
  "get_network_perturbation",
  "get_node_influence",
  "ggnetview_perturbation_curve",
  "press_perturbation"
)

test_that("new perturbation function source files exist in R/", {
  files <- file.path("R", paste0(perturbation_fns, ".R"))
  expect_true(all(file.exists(testthat::test_path("../../", files))))
})

test_that("new perturbation functions are exported in NAMESPACE", {
  ns <- readLines(testthat::test_path("../../NAMESPACE"), warn = FALSE)
  for (fn in perturbation_fns) {
    expect_true(
      any(ns == sprintf("export(%s)", fn)),
      info = sprintf("NAMESPACE is missing export(%s)", fn)
    )
  }
})

test_that("perturbation man pages are present", {
  rd <- file.path("man", paste0(perturbation_fns, ".Rd"))
  expect_true(all(file.exists(testthat::test_path("../../", rd))))
})

test_that("resolve_ggnetview_function finds the new perturbation functions", {
  withr::with_dir(testthat::test_path("../../"), {
    for (fn in perturbation_fns) {
      resolved <- resolve_ggnetview_function(fn)
      expect_true(
        is.function(resolved),
        info = sprintf("resolve_ggnetview_function('%s') did not return a function", fn)
      )
    }
  })
})

test_that("perturbation functions expose their documented entry points", {
  withr::with_dir(testthat::test_path("../../"), {
    attack <- resolve_ggnetview_function("get_network_perturbation")
    expect_true(all(c("graph_obj", "strategy", "bootstrap") %in% names(formals(attack))))

    influence <- resolve_ggnetview_function("get_node_influence")
    expect_true(all(c("graph_obj", "source", "alpha") %in% names(formals(influence))))

    curve <- resolve_ggnetview_function("ggnetview_perturbation_curve")
    expect_true(all(c("curve", "metric") %in% names(formals(curve))))

    press <- resolve_ggnetview_function("press_perturbation")
    expect_true(all(c("graph_obj", "cor_mat") %in% names(formals(press))))
  })
})
