# Basic settings
Sys.setenv(LANGUAGE = 'en')
options(shiny.maxRequestSize = 1000 * 1024^2)

# Load required packages
library(shiny)
library(shinyBS)
library(bs4Dash)
library(shinymeta)
library(shinyWidgets)
library(shinycustomloader)
library(tidyverse)
library(gapminder)
library(scales)
library(plotly)
library(viridis)
library(ggthemes)
library(hrbrthemes)
library(ggprism)
library(colourpicker)
library(ggsci)
library(readxl)
library(xts)
library(dygraphs)
library(lubridate)
library(FactoMineR)
library(factoextra)
library(ggforce)
library(ape)
library(vegan)
library(Rtsne)
library(umap)
library(ggrepel)
library(ggsignif)
library(ggbeeswarm)
library(gghalves)
library(cowplot)
library(htmltools)
library(highcharter)
library(base64enc)
library(slickR)
library(waiter)
library(ggalt)
library(ggridges)
library(ggradar)
library(ggradar2)
library(treemap)
library(markdown)
library(datasets)
library(fresh)
# library(ggNetView)
library(readr)
library(vroom)
library(DT)
library(tools)
library(shinyjs)

# General file reader
read_any <- function(path){
  ext <- tolower(file_ext(path))
  switch(ext,
         "csv"  = vroom::vroom(path, delim = ",", show_col_types = FALSE),
         "tsv"  = vroom::vroom(path, delim = "\t", show_col_types = FALSE),
         "txt"  = vroom::vroom(path, delim = "\t", show_col_types = FALSE),
         "rds"  = readRDS(path),
         "xlsx" = readxl::read_excel(path),
         { suppressWarnings(vroom::vroom(path, delim = ",", show_col_types = FALSE)) }
  )
}

# ---- UI: separate Upload and Preview steps ----
uploadBoxUI <- function(id, title = "Upload Data", help_text = NULL){
  ns <- NS(id)
  bs4Card(
    title = title, status = "primary", width = 12,
    fluidRow(
      column(
        width = 8,
        fileInput(
          ns("file"),
          label  = "Select file (.csv / .tsv / .txt / .xlsx / .rds)",
          accept = c(".csv", ".tsv", ".txt", ".xlsx", ".rds"),
          multiple = FALSE
        )
      ),
      column(
        width = 4, style = "display:flex;align-items:flex-end;gap:8px;flex-wrap:wrap;",
        actionButton(ns("do_upload"),  "Upload",  class = "btn btn-primary"),
        actionButton(ns("do_preview"), "Preview", class = "btn btn-outline-primary"),
        actionButton(ns("do_reset"),   "Reset",   class = "btn btn-outline-secondary")
      )
    ),
    if(!is.null(help_text)) div(class = "text-muted", help_text),
    hr(),
    h5("Preview (first 20 rows)"),
    DTOutput(ns("preview")),
    div(class = "text-muted", style = "margin-top:6px;",
        textOutput(ns("status"), inline = TRUE))
  )
}

# ---- Server: 'Upload' reads with progress; 'Preview' renders table ----
uploadBoxServer <- function(id){
  moduleServer(id, function(input, output, session){
    dat <- reactiveVal(NULL)
    output$status <- renderText("No file uploaded yet.")
    
    # Reset
    observeEvent(input$do_reset, {
      dat(NULL)
      output$preview <- renderDT(NULL)
      output$status  <- renderText("Reset completed.")
      shinyjs::reset(id = session$ns("file"))
    }, ignoreInit = TRUE)
    
    # File is not read automatically; only after clicking "Upload"
    observeEvent(input$do_upload, {
      req(input$file)
      output$status <- renderText("Preparing to read...")
      
      withProgress(message = "Reading file...", value = 0, {
        incProgress(0.2, detail = "Checking file...")
        path <- input$file$datapath
        ex   <- tolower(tools::file_ext(input$file$name))
        ok_ext <- c("csv","tsv","txt","xlsx","rds")
        validate(need(ex %in% ok_ext, "Unsupported file type. Please upload csv/tsv/txt/xlsx/rds."))
        
        incProgress(0.55, detail = "Parsing data...")
        df <- NULL
        try({
          df <- read_any(path)
        }, silent = TRUE)
        validate(need(!is.null(df), "Read failed: check delimiter, worksheet, or file integrity."))
        
        dat(df)
        output$status <- renderText(sprintf("Uploaded: %s (%d rows × %d columns). Click 'Preview' to view the first 20 rows.",
                                            input$file$name, nrow(df), ncol(df)))
        incProgress(1)
      })
    }, ignoreInit = TRUE)
    
    # Only render the table after clicking "Preview"
    observeEvent(input$do_preview, {
      validate(need(!is.null(dat()), "Please upload a file before previewing."))
      output$preview <- renderDT({
        df <- dat()
        DT::datatable(head(df, 20), options = list(pageLength = 5, scrollX = TRUE))
      })
      output$status <- renderText("Preview displayed (first 20 rows).")
    }, ignoreInit = TRUE)
    
    # Reactive data output
    return(reactive(dat()))
  })
}
