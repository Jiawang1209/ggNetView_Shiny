mod_topology_results_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_columns(
    bslib::card(
      bslib::card_header("Calculate"),
      shiny::selectInput(ns("graph_id"), "Graph object", choices = character()),
      shiny::actionButton(ns("calculate"), "Calculate topology")
    ),
    bslib::card(
      bslib::card_header("Topology"),
      DT::DTOutput(ns("topology")),
      shiny::verbatimTextOutput(ns("status"))
    ),
    col_widths = c(4, 8)
  )
}

topology_result_table <- function(value) {
  if (is.data.frame(value)) {
    return(value)
  }

  if (is.list(value) && is.data.frame(value$topology)) {
    return(value$topology)
  }

  as.data.frame(value)
}

mod_topology_results_server <- function(id, registry) {
  shiny::moduleServer(id, function(input, output, session) {
    unique_output_name <- function(base) {
      suffix <- paste0(format(Sys.time(), "%Y%m%d_%H%M%S"), "_", sprintf("%04d", sample.int(9999, 1)))
      paste0(base, "_", suffix)
    }

    topology_table <- shiny::reactiveVal(data.frame())
    status <- shiny::reactiveVal("No topology calculated yet.")

    shiny::observe({
      shiny::updateSelectInput(session, "graph_id", choices = registry_choices(registry, type = "graph"))
    })

    shiny::observeEvent(input$calculate, {
      shiny::req(input$graph_id)
      graph_item <- registry_get(registry, input$graph_id)
      shiny::req(graph_item)

      result <- safe_topology(graph_item$data)
      if (!result$ok) {
        detail <- if (!is.null(result$trace)) paste(result$message, result$trace, sep = "\n") else result$message
        status(detail)
        shiny::showNotification(result$message, type = "error")
        return()
      }

      table <- topology_result_table(result$value)
      topology_name <- unique_output_name(paste0(graph_item$name, "_topology"))
      topology_table(table)
      registry_add(
        registry,
        name = topology_name,
        type = "result",
        data = table,
        source = graph_item$id,
        params = list(metric = "network_topology")
      )
      status(paste("Registered topology:", topology_name))
    })

    output$topology <- DT::renderDT(topology_table(), rownames = FALSE)
    output$status <- shiny::renderText(status())
  })
}
