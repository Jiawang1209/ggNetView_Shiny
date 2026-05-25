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
    shiny::observeEvent(input$register, {
      shiny::req(input$file)

      result <- tryCatch(
        {
          table <- read_user_table(input$file$datapath, filename = input$file$name)
          type <- detect_upload_type(table)
          validation <- if (identical(type, "matrix")) validate_matrix_like(table) else app_success(table)

          if (!validation$ok) {
            shiny::showNotification(validation$message, type = "error")
            return(NULL)
          }

          item <- registry_add(
            registry,
            name = input$object_name,
            type = type,
            data = validation$value,
            source = input$file$name,
            warnings = validation$warnings
          )

          shiny::showNotification(paste("Registered", item$name), type = "message")
          item
        },
        error = function(e) {
          shiny::showNotification(conditionMessage(e), type = "error")
          NULL
        }
      )

      invisible(result)
    })

    output$objects <- DT::renderDT(registry_list(registry), rownames = FALSE)
  })
}
