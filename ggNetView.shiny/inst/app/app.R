# app.R — entry point used by shiny::runApp(); re-uses global / ui / server.
local({
  app_dir <- getwd()
  source(file.path(app_dir, "global.R"), local = TRUE)
  source(file.path(app_dir, "ui.R"),     local = TRUE)
  source(file.path(app_dir, "server.R"), local = TRUE)
  shiny::shinyApp(ui = ui, server = server)
})
