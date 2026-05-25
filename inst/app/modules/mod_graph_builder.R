mod_graph_builder_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::card(
    bslib::card_header("Graph Builder"),
    shiny::selectInput(ns("source_id"), "Source object", choices = character()),
    shiny::actionButton(ns("build"), "Build graph")
  )
}

mod_graph_builder_server <- function(id, registry) {
  shiny::moduleServer(id, function(input, output, session) {})
}
