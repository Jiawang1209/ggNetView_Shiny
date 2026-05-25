app_result <- function(ok, value = NULL, message = NULL, warnings = character(), trace = NULL) {
  structure(
    list(
      ok = isTRUE(ok),
      value = value,
      message = message,
      warnings = warnings,
      trace = trace
    ),
    class = "ggnetview_app_result"
  )
}

app_success <- function(value = NULL, message = NULL, warnings = character()) {
  app_result(TRUE, value = value, message = message, warnings = warnings)
}

app_failure <- function(message, trace = NULL, warnings = character()) {
  app_result(FALSE, value = NULL, message = message, warnings = warnings, trace = trace)
}
