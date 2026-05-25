mod_graph_explorer_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_columns(
    bslib::card(
      bslib::card_header("Select Graph"),
      shiny::selectInput(ns("graph_id"), "Graph object", choices = character())
    ),
    bslib::card(
      bslib::card_header("Summary"),
      shiny::verbatimTextOutput(ns("summary"))
    ),
    col_widths = c(4, 8)
  )
}

mod_graph_explorer_server <- function(id, registry) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observe({
      shiny::updateSelectInput(session, "graph_id", choices = registry_choices(registry, type = "graph"))
    })

    selected_graph <- shiny::reactive({
      shiny::req(input$graph_id)
      registry_get(registry, input$graph_id)
    })

    output$summary <- shiny::renderPrint({
      item <- selected_graph()
      shiny::req(item)
      print(item$summary)
    })
  })
}
