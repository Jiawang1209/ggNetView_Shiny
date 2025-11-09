source("R/global.R")
source("./ui.R")
source("./server.R")


# Run the application 
shinyApp(ui = ui, server = server)


# rmarkdown::render("README.md", output_file = "README.html", clean = TRUE)

