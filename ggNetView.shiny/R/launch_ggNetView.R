#' Launch the ggNetView Shiny application
#'
#' Starts the interactive `shinydashboard` GUI for the
#' \pkg{ggNetView} package. The interface is shipped as a self-contained
#' Shiny app located in `inst/app/` of this package.
#'
#' @param host Character. Host address. Defaults to `"127.0.0.1"`.
#' @param port Integer. TCP port. Defaults to a random free port chosen
#'   by Shiny.
#' @param launch.browser Logical. If `TRUE` (default in interactive
#'   sessions), automatically open a browser window.
#' @param max_upload_size_mb Numeric. Maximum upload size in megabytes
#'   for user-uploaded files. Defaults to `200`.
#' @param ... Additional arguments forwarded to [shiny::runApp()].
#'
#' @return Invisibly returns `NULL`. Called for its side-effect of
#'   running the Shiny application.
#'
#' @examples
#' \dontrun{
#'   launch_ggNetView()
#' }
#'
#' @export
launch_ggNetView <- function(host = "127.0.0.1",
                             port = NULL,
                             launch.browser = interactive(),
                             max_upload_size_mb = 200,
                             ...) {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("Package 'shiny' is required. Please install it first.",
         call. = FALSE)
  }
  if (!requireNamespace("shinydashboard", quietly = TRUE)) {
    stop("Package 'shinydashboard' is required. Please install it first.",
         call. = FALSE)
  }
  if (!requireNamespace("ggNetView", quietly = TRUE)) {
    stop("Package 'ggNetView' is required. Please install it first.",
         call. = FALSE)
  }

  app_dir <- system.file("app", package = "ggNetView.shiny")
  if (!nzchar(app_dir)) {
    stop("Could not find Shiny app directory. ",
         "Try re-installing 'ggNetView.shiny'.", call. = FALSE)
  }

  old_max <- getOption("shiny.maxRequestSize")
  options(shiny.maxRequestSize = max_upload_size_mb * 1024^2)
  on.exit(options(shiny.maxRequestSize = old_max), add = TRUE)

  shiny::runApp(
    appDir         = app_dir,
    host           = host,
    port           = port,
    launch.browser = launch.browser,
    ...
  )
  invisible(NULL)
}

#' Build the ggNetView Shiny app object
#'
#' Returns a [shiny::shinyApp()] object suitable for hosting on
#' shinyapps.io / Shiny Server / Posit Connect, instead of running the
#' app directly. Internally sources `inst/app/global.R`, `inst/app/ui.R`
#' and `inst/app/server.R`.
#'
#' @return A `shiny.appobj` object.
#' @export
ggNetViewApp <- function() {
  app_dir <- system.file("app", package = "ggNetView.shiny")
  if (!nzchar(app_dir)) {
    stop("Could not find Shiny app directory.", call. = FALSE)
  }
  global_env <- new.env(parent = globalenv())
  sys.source(file.path(app_dir, "global.R"), envir = global_env)
  ui_env <- new.env(parent = global_env)
  sys.source(file.path(app_dir, "ui.R"), envir = ui_env)
  server_env <- new.env(parent = global_env)
  sys.source(file.path(app_dir, "server.R"), envir = server_env)
  shiny::shinyApp(ui = ui_env$ui, server = server_env$server)
}
