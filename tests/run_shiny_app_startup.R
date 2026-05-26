args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
script_path <- if (length(file_arg)) {
  normalizePath(sub("^--file=", "", file_arg[[1]]), mustWork = TRUE)
} else {
  normalizePath(file.path("tests", "run_shiny_app_startup.R"), mustWork = TRUE)
}
repo_root <- normalizePath(file.path(dirname(script_path), ".."), mustWork = TRUE)
setwd(file.path(repo_root, "inst", "app"))

source("global.R")
source("ui.R")
source("server.R")

app <- shiny::shinyApp(ui, server)
stopifnot(inherits(app, "shiny.appobj"))
stopifnot(is.numeric(getOption("shiny.maxRequestSize")))
stopifnot(getOption("shiny.maxRequestSize") >= 500 * 1024^2)
cat("shiny app startup passed\n")
