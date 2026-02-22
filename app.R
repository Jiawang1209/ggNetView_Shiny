required_pkgs <- c(
  "shiny",
  "bs4Dash",
  "plotly",
  "markdown",
  "DT",
  "dplyr",
  "tidyr",
  "tibble",
  "readr",
  "tidygraph",
  "igraph",
  "ggplot2"
)

missing_pkgs <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_pkgs) > 0) {
  stop(
    sprintf(
      "Please install required packages before running the app: %s",
      paste(missing_pkgs, collapse = ", ")
    ),
    call. = FALSE
  )
}

for (pkg in required_pkgs) {
  suppressPackageStartupMessages(
    library(pkg, character.only = TRUE)
  )
}

r_scripts <- list.files("R", pattern = "\\.R$", full.names = TRUE)
invisible(lapply(r_scripts, source))

shiny::addResourcePath("man", "man")
shiny::addResourcePath("inst", "inst")
shiny::addResourcePath("docs", "docs")

source("ui.R", local = TRUE)
source("server.R", local = TRUE)

shinyApp(ui = ui, server = server, options = "launch.browser")
