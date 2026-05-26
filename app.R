app_dir <- file.path("inst", "app")

if (!dir.exists(app_dir)) {
  stop("Cannot find Shiny app directory: ", normalizePath(app_dir, mustWork = FALSE), call. = FALSE)
}

shiny::runApp(app_dir, launch.browser = TRUE)
