source(testthat::test_path("../../R/app_smoke_coverage.R"))
source(testthat::test_path("../../R/app_release_evidence.R"))

testthat::test_that("release evidence summarizes manual coverage and commits", {
  coverage <- smoke_coverage_new("manual_workflow")
  for (area in sprintf("%02d", 1:10)) {
    coverage <- smoke_coverage_record(
      coverage,
      manual_area = area,
      workflow = paste("workflow", area),
      evidence = paste("evidence", area),
      smoke_script = "tests/run_shiny_manual_workflow_smoke.R"
    )
  }
  commits <- data.frame(
    hash = c("abc1234", "def5678"),
    subject = c("feat: first slice", "test: verification slice"),
    stringsAsFactors = FALSE
  )
  validation <- data.frame(
    command = c("/usr/local/bin/Rscript tests/run_shiny_manual_workflow_smoke.R"),
    result = c("manual workflow smoke passed"),
    stringsAsFactors = FALSE
  )

  evidence <- release_evidence_summary(
    coverage = coverage,
    commits = commits,
    validation = validation,
    remaining_limits = c("Project-specific report language still needs real-use refinement.")
  )

  testthat::expect_true(evidence$coverage_ok)
  testthat::expect_equal(evidence$manual_area_count, 10L)
  testthat::expect_equal(nrow(evidence$commits), 2L)
})

testthat::test_that("release evidence markdown includes required final handoff sections", {
  coverage <- smoke_coverage_new("manual_workflow")
  for (area in sprintf("%02d", 1:10)) {
    coverage <- smoke_coverage_record(
      coverage,
      manual_area = area,
      workflow = paste("workflow", area),
      evidence = paste("evidence", area),
      smoke_script = "tests/run_shiny_manual_workflow_smoke.R"
    )
  }
  evidence <- release_evidence_summary(coverage = coverage)
  markdown <- render_release_evidence_markdown(evidence)

  testthat::expect_match(markdown, "# ggNetView Shiny Release Evidence", fixed = TRUE)
  testthat::expect_match(markdown, "Manual Coverage", fixed = TRUE)
  testthat::expect_match(markdown, "Validation Commands", fixed = TRUE)
  testthat::expect_match(markdown, "Recent Commits", fixed = TRUE)
  testthat::expect_match(markdown, "Remaining Limits", fixed = TRUE)
  testthat::expect_match(markdown, "Next Release Steps", fixed = TRUE)
  testthat::expect_match(markdown, "01", fixed = TRUE)
  testthat::expect_match(markdown, "10", fixed = TRUE)
})

testthat::test_that("write_release_evidence_report creates a markdown artifact", {
  coverage <- smoke_coverage_new("manual_workflow")
  for (area in sprintf("%02d", 1:10)) {
    coverage <- smoke_coverage_record(
      coverage,
      manual_area = area,
      workflow = paste("workflow", area),
      evidence = paste("evidence", area),
      smoke_script = "tests/run_shiny_manual_workflow_smoke.R"
    )
  }
  path <- tempfile(fileext = ".md")

  write_release_evidence_report(release_evidence_summary(coverage = coverage), path)

  testthat::expect_true(file.exists(path))
  text <- paste(readLines(path, warn = FALSE), collapse = "\n")
  testthat::expect_match(text, "Manual Coverage", fixed = TRUE)
})
