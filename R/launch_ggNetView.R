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
    app_dir <- file.path(getwd(), "inst", "app")
  }

  if (!dir.exists(app_dir)) {
    stop("Cannot find ggNetView Shiny app directory.", call. = FALSE)
  }

  shiny::runApp(app_dir, launch.browser = launch.browser, ...)
}
