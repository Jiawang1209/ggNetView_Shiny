repo_root <- normalizePath(file.path("..", ".."), mustWork = TRUE)
setwd(file.path(repo_root, "inst", "app"))

source("global.R")
source("ui.R")
source("server.R")

app <- shiny::shinyApp(ui, server)
stopifnot(inherits(app, "shiny.appobj"))
cat("shiny app startup passed\n")
