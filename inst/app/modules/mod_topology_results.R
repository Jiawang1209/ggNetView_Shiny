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
      DT::DTOutput(ns("topology"))
    ),
    col_widths = c(4, 8)
  )
}

mod_topology_results_server <- function(id, registry) {
  shiny::moduleServer(id, function(input, output, session) {
    topology_table <- shiny::reactiveVal(data.frame())

    shiny::observe({
      shiny::updateSelectInput(session, "graph_id", choices = registry_choices(registry, type = "graph"))
    })

    shiny::observeEvent(input$calculate, {
      shiny::req(input$graph_id)
      graph_item <- registry_get(registry, input$graph_id)
      shiny::req(graph_item)

      result <- safe_topology(graph_item$data)
      if (!result$ok) {
        shiny::showNotification(result$message, type = "error")
        return()
      }

      table <- as.data.frame(result$value)
      topology_table(table)
      registry_add(
        registry,
        name = paste0(graph_item$name, "_topology"),
        type = "result",
        data = table,
        source = graph_item$id,
        params = list(metric = "network_topology")
      )
    })

    output$topology <- DT::renderDT(topology_table(), rownames = FALSE)
  })
}
