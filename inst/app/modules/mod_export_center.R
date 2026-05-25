mod_export_center_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::card(
    bslib::card_header("Export Center"),
    shiny::selectInput(ns("object_id"), "Object", choices = character()),
    shiny::downloadButton(ns("download_rds"), "Download RDS")
  )
}

mod_export_center_server <- function(id, registry) {
  shiny::moduleServer(id, function(input, output, session) {})
}
