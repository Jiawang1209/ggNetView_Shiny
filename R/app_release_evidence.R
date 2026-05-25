if (!exists("%||%", mode = "function")) {
  `%||%` <- function(x, y) {
    if (is.null(x)) y else x
  }
}

release_default_validation_commands <- function() {
  data.frame(
    command = c(
      "/usr/local/bin/Rscript -e 'testthat::test_file(\"tests/testthat/test-shiny-smoke-coverage.R\")'",
      "/usr/local/bin/Rscript -e 'testthat::test_file(\"tests/testthat/test-shiny-files.R\")'",
      "/usr/local/bin/Rscript tests/run_shiny_manual_workflow_smoke.R",
      "/usr/local/bin/Rscript tests/run_shiny_phase2_workflow_smoke.R",
      "/usr/local/bin/Rscript tests/run_shiny_graph_builder_modes_smoke.R",
      "/usr/local/bin/Rscript tests/run_shiny_analysis_export_smoke.R",
      "/usr/local/bin/Rscript tests/run_shiny_visual_layouts_smoke.R",
      "/usr/local/bin/Rscript tests/run_shiny_environment_geometry_smoke.R",
      "/usr/local/bin/Rscript tests/run_shiny_mobile_layout_smoke.R",
      "/usr/local/bin/Rscript tests/run_shiny_task_feedback_smoke.R"
    ),
    result = c(
      "Focused coverage helper regression.",
      "Static Shiny source/file regression.",
      "Manual-backed backend workflow and manual-area coverage.",
      "Main browser workflow smoke.",
      "Graph Builder mode browser smoke.",
      "Analysis/export browser smoke.",
      "Visual Lab layout browser smoke.",
      "Environment geometry browser smoke.",
      "Mobile navigation/overflow browser smoke.",
      "Long-running action feedback browser smoke."
    ),
    status = c(
      "required",
      "required",
      "required",
      "required",
      "required",
      "required",
      "required",
      "required",
      "required",
      "required"
    ),
    stringsAsFactors = FALSE
  )
}

release_default_remaining_limits <- function() {
  c(
    "Project-specific biological/statistical report wording still needs refinement after longer real-use sessions.",
    "Cross-session restore merge review and editable restored-object summaries remain future workflow polish.",
    "Future long-running buttons should receive targeted busy-state browser assertions as they are added.",
    "A final continuous all-smoke pass should be run immediately before release or handoff."
  )
}

release_default_next_steps <- function() {
  c(
    "Run the full validation command list sequentially with /usr/local/bin/Rscript.",
    "Launch the Shiny app and inspect the main tabs against the generated evidence report.",
    "Review remaining limits with the user and decide whether they block release.",
    "Create the final release/readiness commit after the full pass is green."
  )
}

release_git_commits <- function(n = 20L) {
  output <- tryCatch(
    system2("git", c("log", "--oneline", paste0("-", as.integer(n))), stdout = TRUE, stderr = TRUE),
    warning = function(w) character(),
    error = function(e) character()
  )
  output <- output[nzchar(output)]
  if (!length(output)) {
    return(data.frame(hash = character(), subject = character(), stringsAsFactors = FALSE))
  }

  pieces <- regmatches(output, regexec("^([^ ]+)\\s+(.*)$", output))
  rows <- lapply(pieces, function(piece) {
    if (length(piece) < 3L) {
      return(c(hash = "", subject = piece[[1]] %||% ""))
    }
    c(hash = piece[[2]], subject = piece[[3]])
  })
  data.frame(
    hash = vapply(rows, `[[`, character(1), "hash"),
    subject = vapply(rows, `[[`, character(1), "subject"),
    stringsAsFactors = FALSE
  )
}

release_read_coverage <- function(path = "tests/_smoke_coverage/manual_workflow_coverage.json") {
  if (!file.exists(path)) {
    return(NULL)
  }
  smoke_coverage_read(path)
}

release_evidence_summary <- function(
    coverage = NULL,
    commits = NULL,
    validation = NULL,
    remaining_limits = NULL,
    next_steps = NULL,
    generated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS%z")) {
  if (is.null(coverage)) {
    coverage <- release_read_coverage()
  }

  audit <- NULL
  coverage_records <- data.frame(
    manual_area = character(),
    workflow = character(),
    evidence = character(),
    smoke_script = character(),
    stringsAsFactors = FALSE
  )
  covered_manifest <- data.frame(
    manual_area = character(),
    chapter = character(),
    required = logical(),
    smoke_script = character(),
    stringsAsFactors = FALSE
  )
  missing_required <- smoke_manual_area_manifest()$manual_area
  coverage_ok <- FALSE

  if (!is.null(coverage)) {
    audit <- smoke_coverage_audit(coverage)
    coverage_records <- audit$records
    covered_manifest <- audit$covered
    missing_required <- audit$missing_required
    coverage_ok <- isTRUE(audit$ok)
  }

  list(
    generated_at = generated_at,
    coverage_ok = coverage_ok,
    manual_area_count = length(unique(coverage_records$manual_area)),
    missing_required = missing_required,
    coverage_manifest = covered_manifest,
    coverage_records = coverage_records,
    commits = commits %||% release_git_commits(),
    validation = validation %||% release_default_validation_commands(),
    remaining_limits = remaining_limits %||% release_default_remaining_limits(),
    next_steps = next_steps %||% release_default_next_steps()
  )
}

release_markdown_escape <- function(x) {
  gsub("\\|", "\\\\|", as.character(x %||% ""), fixed = FALSE)
}

release_markdown_table <- function(df, columns) {
  if (!nrow(df)) {
    return("- None recorded.")
  }
  header <- paste0("| ", paste(columns, collapse = " | "), " |")
  separator <- paste0("| ", paste(rep("---", length(columns)), collapse = " | "), " |")
  rows <- apply(df[columns], 1, function(row) {
    paste0("| ", paste(release_markdown_escape(row), collapse = " | "), " |")
  })
  c(header, separator, rows)
}

render_release_evidence_markdown <- function(evidence) {
  records <- evidence$coverage_records
  if (nrow(records)) {
    records <- records[order(records$manual_area, records$workflow), , drop = FALSE]
  }
  commits <- evidence$commits
  validation <- evidence$validation

  lines <- c(
    "# ggNetView Shiny Release Evidence",
    "",
    paste0("Generated at: ", evidence$generated_at),
    "",
    "## Manual Coverage",
    "",
    paste0("- Coverage ok: ", if (isTRUE(evidence$coverage_ok)) "yes" else "no"),
    paste0("- Manual areas covered: ", evidence$manual_area_count, "/10"),
    paste0("- Missing required areas: ", if (length(evidence$missing_required)) paste(evidence$missing_required, collapse = ", ") else "none"),
    "",
    release_markdown_table(records, c("manual_area", "workflow", "evidence", "smoke_script")),
    "",
    "## Validation Commands",
    "",
    release_markdown_table(validation, intersect(c("status", "command", "result"), names(validation))),
    "",
    "## Recent Commits",
    "",
    release_markdown_table(commits, intersect(c("hash", "subject"), names(commits))),
    "",
    "## Remaining Limits",
    "",
    paste0("- ", evidence$remaining_limits),
    "",
    "## Next Release Steps",
    "",
    paste0(seq_along(evidence$next_steps), ". ", evidence$next_steps),
    ""
  )
  paste(lines, collapse = "\n")
}

write_release_evidence_report <- function(evidence, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  writeLines(render_release_evidence_markdown(evidence), path, useBytes = TRUE)
  invisible(path)
}

generate_release_evidence_report <- function(
    path = "docs/ggnetview-shiny-release-evidence.md",
    coverage_path = "tests/_smoke_coverage/manual_workflow_coverage.json") {
  coverage <- release_read_coverage(coverage_path)
  evidence <- release_evidence_summary(coverage = coverage)
  write_release_evidence_report(evidence, path)
}
