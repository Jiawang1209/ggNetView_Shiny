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

test_that("task feedback test delay is opt-in for browser smoke", {
  old <- Sys.getenv("GGNV_TASK_FEEDBACK_TEST_DELAY", unset = NA)
  on.exit({
    if (is.na(old)) {
      Sys.unsetenv("GGNV_TASK_FEEDBACK_TEST_DELAY")
    } else {
      Sys.setenv(GGNV_TASK_FEEDBACK_TEST_DELAY = old)
    }
  }, add = TRUE)

  Sys.setenv(GGNV_TASK_FEEDBACK_TEST_DELAY = "0")
  expect_false(task_feedback_test_delay())
})

test_that("long-running Shiny actions use shared task feedback", {
  module_sources <- c(
    paste(readLines(test_path("../../inst/app/modules/mod_data_hub.R"), warn = FALSE), collapse = "\n"),
    paste(readLines(test_path("../../inst/app/modules/mod_compare_environment.R"), warn = FALSE), collapse = "\n")
  )
  source_text <- paste(module_sources, collapse = "\n")

  expected_buttons <- c(
    "load_gallery",
    "run_gallery_recipe",
    "run_compare",
    "run_multi_group",
    "run_environment",
    "run_environment_manual",
    "run_environment_triple",
    "run_mantel"
  )

  for (button in expected_buttons) {
    expect_match(source_text, paste0("session\\$ns\\(\"", button, "\"\\)"))
  }
  expect_true(length(gregexpr("with_task_feedback", source_text, fixed = TRUE)[[1]]) >= length(expected_buttons))
})
