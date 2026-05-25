mod_export_center_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::card(
    bslib::card_header("Export Center"),
    shiny::selectInput(ns("object_id"), "Object", choices = character()),
    shiny::downloadButton(ns("download_rds"), "Download RDS"),
    shiny::downloadButton(ns("download_csv"), "Download CSV"),
    shiny::downloadButton(ns("download_params"), "Download Parameters")
  )
}

mod_export_center_server <- function(id, registry) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observe({
      shiny::updateSelectInput(session, "object_id", choices = registry_choices(registry))
    })

    selected_item <- shiny::reactive({
      shiny::req(input$object_id)
      registry_get(registry, input$object_id)
    })

    output$download_rds <- shiny::downloadHandler(
      filename = function() paste0(selected_item()$name, ".rds"),
      content = function(file) write_registry_object(selected_item()$data, file)
    )

    output$download_csv <- shiny::downloadHandler(
      filename = function() paste0(selected_item()$name, ".csv"),
      content = function(file) {
        data <- selected_item()$data
        if (!is.matrix(data) && !is.data.frame(data)) {
          data <- data.frame(value = utils::capture.output(utils::str(data)))
        }
        write_registry_table(data, file)
      }
    )

    output$download_params <- shiny::downloadHandler(
      filename = function() paste0(selected_item()$name, "_params.json"),
      content = function(file) write_registry_params(selected_item()$params, file)
    )
  })
}
