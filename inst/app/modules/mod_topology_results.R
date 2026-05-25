mod_topology_results_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::card(
    bslib::card_header("Topology Results"),
    shiny::selectInput(ns("graph_id"), "Graph object", choices = character()),
    shiny::actionButton(ns("calculate"), "Calculate"),
    DT::DTOutput(ns("topology"))
  )
}

mod_topology_results_server <- function(id, registry) {
  shiny::moduleServer(id, function(input, output, session) {})
}
