if (!exists("%||%", mode = "function")) {
  `%||%` <- function(x, y) {
    if (is.null(x)) y else x
  }
}

smoke_manual_area_manifest <- function() {
  data.frame(
    manual_area = sprintf("%02d", 1:10),
    chapter = c(
      "Create graph object",
      "RMT",
      "Graph info",
      "Subgraph",
      "Layout",
      "Network topology",
      "Network compare",
      "Network environment",
      "Multi-omics network",
      "Gallery of reproducible examples"
    ),
    required = rep(TRUE, 10),
    stringsAsFactors = FALSE
  )
}

smoke_coverage_new <- function(smoke_name) {
  list(
    smoke_name = as.character(smoke_name),
    generated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS%z"),
    records = list()
  )
}

smoke_coverage_record <- function(coverage, manual_area, workflow, evidence, smoke_script) {
  stopifnot(is.list(coverage))
  record <- list(
    manual_area = as.character(manual_area),
    workflow = as.character(workflow),
    evidence = as.character(evidence),
    smoke_script = as.character(smoke_script)
  )
  coverage$records[[length(coverage$records) + 1L]] <- record
  coverage
}

smoke_coverage_records_df <- function(coverage) {
  records <- coverage$records %||% list()
  if (!length(records)) {
    return(data.frame(
      manual_area = character(),
      workflow = character(),
      evidence = character(),
      smoke_script = character(),
      stringsAsFactors = FALSE
    ))
  }
  do.call(rbind, lapply(records, function(record) {
    data.frame(
      manual_area = record$manual_area %||% "",
      workflow = record$workflow %||% "",
      evidence = record$evidence %||% "",
      smoke_script = record$smoke_script %||% "",
      stringsAsFactors = FALSE
    )
  }))
}

smoke_coverage_audit <- function(coverage, manifest = smoke_manual_area_manifest()) {
  records <- smoke_coverage_records_df(coverage)
  covered_areas <- unique(records$manual_area)
  required <- manifest$manual_area[isTRUE(manifest$required) | manifest$required]
  missing_required <- setdiff(required, covered_areas)
  covered <- merge(
    manifest,
    unique(records[c("manual_area", "smoke_script")]),
    by = "manual_area",
    all.x = FALSE,
    all.y = FALSE
  )
  list(
    ok = length(missing_required) == 0L,
    missing_required = missing_required,
    covered = covered,
    records = records
  )
}

smoke_coverage_write <- function(coverage, path) {
  audit <- smoke_coverage_audit(coverage)
  payload <- coverage
  payload$audit <- list(
    ok = audit$ok,
    missing_required = audit$missing_required,
    covered_count = nrow(audit$covered)
  )
  jsonlite::write_json(payload, path, pretty = TRUE, auto_unbox = TRUE, null = "null")
  invisible(path)
}

smoke_coverage_read <- function(path) {
  jsonlite::read_json(path, simplifyVector = FALSE)
}
