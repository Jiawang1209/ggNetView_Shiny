source(test_path("../../R/app_task_feedback.R"))

test_that("task feedback messages are consistent", {
  expect_equal(task_feedback_message("Build graph", "running"), "Running Build graph...")
  expect_equal(task_feedback_message("Build graph", "success"), "Finished Build graph.")
  expect_equal(task_feedback_message("Build graph", "failure"), "Failed Build graph.")
})

test_that("task busy payloads target a single button", {
  expect_equal(task_busy_payload("graph_builder-build", TRUE), list(id = "graph_builder-build", busy = TRUE))
  expect_equal(task_busy_payload("graph_builder-build", FALSE), list(id = "graph_builder-build", busy = FALSE))
})

test_that("task feedback script registers the client handler", {
  script <- paste(capture.output(print(app_task_feedback_script())), collapse = "\n")

  expect_match(script, "ggnetview-task-busy", fixed = TRUE)
  expect_match(script, "disabled", fixed = TRUE)
})

test_that("with_task_feedback works without an active Shiny session", {
  value <- with_task_feedback(NULL, "Unit task", "unit-button", {
    40 + 2
  })

  expect_equal(value, 42)
})
