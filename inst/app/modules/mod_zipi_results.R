mod_zipi_results_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(
    class = "zipi-page",
    bslib::card(
      class = "zipi-control-card",
      bslib::card_header("Calculate Zi-Pi"),
      shiny::selectInput(ns("graph_id"), "Graph object", choices = character()),
      shiny::div(
        class = "zipi-control-grid",
        shiny::numericInput(ns("zi_threshold"), "Zi threshold", value = 2.5, min = 0, step = 0.1),
        shiny::numericInput(ns("pi_threshold"), "Pi threshold", value = 0.62, min = 0, max = 1, step = 0.01)
      ),
      shiny::actionButton(ns("calculate_zipi"), "Calculate Zi-Pi", class = "w-100")
    ),
    bslib::card(
      class = "zipi-result-card",
      bslib::card_header("Zi-Pi"),
      DT::DTOutput(ns("zipi")),
      shiny::downloadButton(ns("download_zipi"), "Download Zi-Pi CSV"),
      shiny::verbatimTextOutput(ns("status"))
    )
  )
}

zipi_download_handler <- function(table_fn, filename) {
  shiny::downloadHandler(
    filename = function() filename,
    content = function(file) write_registry_table(table_fn(), file)
  )
}

mod_zipi_results_server <- function(id, registry) {
  shiny::moduleServer(id, function(input, output, session) {
    unique_output_name <- function(base) {
      suffix <- paste0(format(Sys.time(), "%Y%m%d_%H%M%S"), "_", sprintf("%04d", sample.int(9999, 1)))
      paste0(base, "_", suffix)
    }

    zipi_table <- shiny::reactiveVal(data.frame())
    status <- shiny::reactiveVal("No Zi-Pi result calculated yet.")

    shiny::observe({
      graph_choices <- registry_choices(registry, type = "graph")
      graph_selected <- shiny::isolate(input$graph_id)
      if (is.null(graph_selected) || !graph_selected %in% unname(graph_choices)) {
        graph_selected <- if (length(graph_choices)) unname(graph_choices[[1]]) else character()
      }
      shiny::updateSelectInput(session, "graph_id", choices = graph_choices, selected = graph_selected)
    })

    shiny::observeEvent(input$calculate_zipi, {
      shiny::req(input$graph_id)
      graph_item <- registry_get(registry, input$graph_id)
      shiny::req(graph_item)

      params <- list(
        zi_threshold = input$zi_threshold,
        pi_threshold = input$pi_threshold
      )
      status(task_feedback_message("Zi-Pi classification", "running"))
      result <- with_task_feedback(
        session,
        "Zi-Pi classification",
        session$ns("calculate_zipi"),
        safe_zipi(
          graph_item$data,
          zi_threshold = input$zi_threshold,
          pi_threshold = input$pi_threshold
        )
      )
      if (!result$ok) {
        zipi_table(data.frame())
        detail <- if (!is.null(result$trace)) paste(result$message, result$trace, sep = "\n") else result$message
        status(detail)
        shiny::showNotification(result$message, type = "error")
        return()
      }

      table <- as.data.frame(result$value, check.names = FALSE)
      result_name <- unique_output_name(paste0(graph_item$name, "_zipi"))
      zipi_table(table)
      registry_add(
        registry,
        name = result_name,
        type = "result",
        data = table,
        source = graph_item$id,
        params = c(list(metric = "zipi"), params)
      )
      status(paste("Registered Zi-Pi:", result_name))
      shiny::showNotification(paste("Registered Zi-Pi:", result_name), type = "message")
    })

    output$zipi <- DT::renderDT(zipi_table(), rownames = FALSE)
    output$status <- shiny::renderText(status())
    output$download_zipi <- zipi_download_handler(zipi_table, "ggnetview_zipi.csv")
  })
}
