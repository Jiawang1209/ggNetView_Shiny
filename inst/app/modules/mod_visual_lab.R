mod_visual_lab_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::card(
    bslib::card_header("Visual Lab"),
    shiny::selectInput(ns("graph_id"), "Graph object", choices = character()),
    shiny::actionButton(ns("draw"), "Draw"),
    shiny::plotOutput(ns("plot"), height = 650)
  )
}

mod_visual_lab_server <- function(id, registry) {
  shiny::moduleServer(id, function(input, output, session) {})
}
