source("global.R", local = TRUE)
source("ui.R", local = TRUE)
source("server.R", local = TRUE)

shiny::shinyApp(ui = ui, server = server)
