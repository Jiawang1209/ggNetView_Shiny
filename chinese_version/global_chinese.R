# Basic setting
Sys.setenv(LANGUAGE = 'en')
options(shiny.maxRequestSize = 1000 * 1024^2)

# Load R Package
library(shiny)
# library(shinydashboard)
# library(dashboardthemes)
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
# library(ggcor)
library(ggrepel)
library(ggsignif)
library(ggbeeswarm)
library(gghalves)
library(gapminder)
library(cowplot)
library(htmltools)
# library(sf)
# library(ggspatial)
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
library(ggNetView)
library(readr)
library(vroom)
library(DT)
library(tools)
library(shinyjs)


# 通用读取
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

# ---- UI：分离 上传 / 预览 两步 ----
uploadBoxUI <- function(id, title = "Upload data", help_text = NULL){
  ns <- NS(id)
  bs4Card(
    title = title, status = "primary", width = 12,
    fluidRow(
      column(
        width = 8,
        fileInput(
          ns("file"),
          label  = "选择文件（.csv / .tsv / .txt / .xlsx / .rds）",
          accept = c(".csv",".tsv",".txt",".xlsx",".rds"),
          multiple = FALSE
        )
      ),
      column(
        width = 4, style = "display:flex;align-items:flex-end;gap:8px;flex-wrap:wrap;",
        actionButton(ns("do_upload"),  "读取",  class = "btn btn-primary"),
        actionButton(ns("do_preview"), "预览",  class = "btn btn-outline-primary"),
        actionButton(ns("do_reset"),   "清空",  class = "btn btn-outline-secondary")
        
      )
    ),
    if(!is.null(help_text)) div(class="text-muted", help_text),
    hr(),
    h5("预览（前20行）"),
    DTOutput(ns("preview")),
    div(class = "text-muted", style="margin-top:6px;",
        textOutput(ns("status"), inline = TRUE))
  )
}

# ---- Server：点击【上传】只读取+显示进度；点击【预览】才渲染表 ----
uploadBoxServer <- function(id){
  moduleServer(id, function(input, output, session){
    dat <- reactiveVal(NULL)
    output$status <- renderText("尚未上传文件。")
    
    # 清空
    observeEvent(input$do_reset, {
      dat(NULL)
      output$preview <- renderDT(NULL)
      output$status  <- renderText("已清空。")
      # 同时把 fileInput 重置（可选）
      shinyjs::reset(id = session$ns("file"))
    }, ignoreInit = TRUE)
    
    # 选择文件后并不会自动读取；只有点击【上传】才读取
    observeEvent(input$do_upload, {
      req(input$file)
      output$status <- renderText("准备读取…")
      
      withProgress(message = "读取文件中…", value = 0, {
        incProgress(0.2, detail = "检查文件…")
        path <- input$file$datapath
        ex   <- tolower(tools::file_ext(input$file$name))
        ok_ext <- c("csv","tsv","txt","xlsx","rds")
        validate(need(ex %in% ok_ext, "不支持的文件类型，请上传 csv/tsv/txt/xlsx/rds"))
        
        incProgress(0.55, detail = "解析数据…")
        df <- NULL
        try({
          df <- read_any(path)
        }, silent = TRUE)
        validate(need(!is.null(df), "读取失败：请检查分隔符、工作表或文件是否损坏。"))
        
        dat(df)  # 只把数据载入内存，不渲染表
        output$status <- renderText(sprintf("已上传：%s（%d 行 × %d 列）。点击『预览』查看前20行。",
                                            input$file$name, nrow(df), ncol(df)))
        incProgress(1)
      })
    }, ignoreInit = TRUE)
    
    # 只有点击【预览】才渲染表格
    observeEvent(input$do_preview, {
      validate(need(!is.null(dat()), "请先上传文件，再点击『预览』。"))
      output$preview <- renderDT({
        df <- dat()
        DT::datatable(head(df, 20), options = list(pageLength = 5, scrollX = TRUE))
      })
      output$status <- renderText("已显示预览（前20行）。")
    }, ignoreInit = TRUE)
    
    # 对外暴露：reactive 数据
    return(reactive(dat()))
  })
}
