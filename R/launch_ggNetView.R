#' Launch ggNetView Shiny
#'
#' Opens the bundled ggNetView Shiny application.
#'
#' @param launch.browser Logical. Open the app in a browser.
#' @param ... Additional arguments passed to [shiny::runApp()].
#'
#' @return The return value from [shiny::runApp()].
#' @export
launch_ggNetView <- function(launch.browser = TRUE, ...) {
  app_dir <- system.file("app", package = "ggNetView")

  if (!nzchar(app_dir)) {
    candidates <- c(
      file.path(getwd(), "inst", "app"),
      file.path(dirname(getwd()), "inst", "app")
    )

    if (
      requireNamespace("rstudioapi", quietly = TRUE) &&
        rstudioapi::hasFun("getActiveProject")
    ) {
      active_project <- tryCatch(
        rstudioapi::getActiveProject(),
        error = function(e) NULL
      )
      if (!is.null(active_project) && nzchar(active_project)) {
        candidates <- c(candidates, file.path(active_project, "inst", "app"))
      }
    }

    existing <- candidates[dir.exists(candidates)]
    if (length(existing) > 0) {
      app_dir <- existing[[1]]
    }
  }

  if (!dir.exists(app_dir)) {
    stop("Cannot find ggNetView Shiny app directory.", call. = FALSE)
  }

  shiny::runApp(app_dir, launch.browser = launch.browser, ...)
}
