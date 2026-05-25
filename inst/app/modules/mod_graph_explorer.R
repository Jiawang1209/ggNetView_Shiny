mod_graph_explorer_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::card(
    bslib::card_header("Graph Explorer"),
    shiny::selectInput(ns("graph_id"), "Graph object", choices = character()),
    DT::DTOutput(ns("nodes")),
    DT::DTOutput(ns("edges"))
  )
}

mod_graph_explorer_server <- function(id, registry) {
  shiny::moduleServer(id, function(input, output, session) {})
}
