test_that("safe_build_graph returns failure for unknown builder", {
  result <- safe_build_graph(
    data = matrix(1:4, nrow = 2, dimnames = list(c("A", "B"), c("S1", "S2"))),
    builder = "missing_builder",
    params = list()
  )

  expect_false(result$ok)
  expect_match(result$message, "Unsupported graph builder")
})

test_that("safe_build_graph rejects invalid builder shape", {
  mat <- matrix(1:4, nrow = 2, dimnames = list(c("A", "B"), c("S1", "S2")))

  for (builder in list(NULL, character(0), c("matrix", "adjacency"))) {
    result <- safe_build_graph(data = mat, builder = builder, params = list())
    expect_false(result$ok)
    expect_match(result$message, "Unsupported graph builder")
  }
})

test_that("safe_call catches delayed-expression errors", {
  result <- safe_call(stop("boom", call. = FALSE), "User safe message")

  expect_false(result$ok)
  expect_equal(result$message, "User safe message")
  expect_match(result$trace, "boom")
})

test_that("resolve_ggnetview_function resolves source fallback functions", {
  tmp <- tempfile("ggnetview-adapter-")
  dir.create(file.path(tmp, "R"), recursive = TRUE)
  writeLines(
    "temporary_adapter_fn <- function() 'resolved'",
    file.path(tmp, "R", "temporary_adapter_fn.R")
  )

  if (requireNamespace("withr", quietly = TRUE)) {
    withr::local_dir(tmp)
  } else {
    old <- setwd(tmp)
    on.exit(setwd(old), add = TRUE)
  }

  fn <- resolve_ggnetview_function("temporary_adapter_fn")

  expect_true(is.function(fn))
  expect_equal(fn(), "resolved")
})

test_that("safe_build_graph uses source fallback for valid builder", {
  tmp <- tempfile("ggnetview-builder-")
  dir.create(file.path(tmp, "R"), recursive = TRUE)
  writeLines(
    "build_graph_from_mat <- function(data, marker = NULL) list(data = data, marker = marker)",
    file.path(tmp, "R", "build_graph_from_mat.R")
  )

  if (requireNamespace("withr", quietly = TRUE)) {
    withr::local_dir(tmp)
  } else {
    old <- setwd(tmp)
    on.exit(setwd(old), add = TRUE)
  }

  mat <- matrix(1:4, nrow = 2, dimnames = list(c("A", "B"), c("S1", "S2")))
  result <- safe_build_graph(mat, "matrix", params = list(marker = "ok"))

  expect_true(result$ok)
  expect_equal(result$value$marker, "ok")
  expect_equal(result$value$data, mat)
})

test_that("safe_plot_ggnetview rejects non-graph input", {
  result <- safe_plot_ggnetview(graph = data.frame(x = 1), params = list())

  expect_false(result$ok)
  expect_match(result$message, "graph")
})

test_that("safe_topology rejects non-graph input", {
  result <- safe_topology(graph = data.frame(x = 1))

  expect_false(result$ok)
  expect_match(result$message, "graph")
})
