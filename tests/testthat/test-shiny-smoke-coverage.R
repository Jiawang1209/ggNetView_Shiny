source(testthat::test_path("../../R/app_smoke_coverage.R"))

testthat::test_that("smoke coverage manifest lists manual areas", {
  manifest <- smoke_manual_area_manifest()

  testthat::expect_true(all(c("manual_area", "chapter", "required") %in% names(manifest)))
  testthat::expect_true(all(sprintf("%02d", 1:10) %in% manifest$manual_area))
  testthat::expect_true(all(manifest$required))
})

testthat::test_that("smoke coverage logger writes and validates required areas", {
  coverage <- smoke_coverage_new("manual_workflow")
  coverage <- smoke_coverage_record(
    coverage,
    manual_area = "01",
    workflow = "graph builders",
    evidence = "matrix graph built",
    smoke_script = "tests/run_shiny_manual_workflow_smoke.R"
  )
  path <- tempfile(fileext = ".json")
  smoke_coverage_write(coverage, path)
  read_back <- smoke_coverage_read(path)

  testthat::expect_true(file.exists(path))
  testthat::expect_equal(read_back$smoke_name, "manual_workflow")
  testthat::expect_equal(read_back$records[[1]]$manual_area, "01")

  audit <- smoke_coverage_audit(read_back)
  testthat::expect_true("10" %in% audit$missing_required)
  testthat::expect_false(audit$ok)
})

testthat::test_that("manual smoke coverage includes every manual area", {
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

  audit <- smoke_coverage_audit(coverage)

  testthat::expect_true(audit$ok)
  testthat::expect_length(audit$missing_required, 0L)
  testthat::expect_equal(nrow(audit$covered), 10L)
})
