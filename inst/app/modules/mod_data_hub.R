mod_data_hub_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_columns(
    bslib::card(
      bslib::card_header("Upload"),
      shiny::fileInput(ns("file"), "Upload CSV, TSV, or TXT"),
      shiny::textInput(ns("object_name"), "Object name", value = "uploaded_matrix"),
      shiny::actionButton(ns("register"), "Register object")
    ),
    bslib::card(
      bslib::card_header("Objects"),
      DT::DTOutput(ns("objects"))
    )
  )
}

mod_data_hub_server <- function(id, registry) {
  shiny::moduleServer(id, function(input, output, session) {
    output$objects <- DT::renderDT(registry_list(registry), rownames = FALSE)
  })
}
