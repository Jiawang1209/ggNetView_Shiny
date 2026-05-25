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
