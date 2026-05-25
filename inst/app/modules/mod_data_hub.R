app_example_matrix_path <- function() {
  candidates <- c(
    file.path(getwd(), "inst", "extdata", "example_matrix.csv"),
    file.path(getwd(), "..", "..", "inst", "extdata", "example_matrix.csv"),
    system.file("extdata", "example_matrix.csv", package = "ggNetView")
  )
  candidates <- candidates[nzchar(candidates)]
  existing <- candidates[file.exists(candidates)]
  if (!length(existing)) {
    stop("Cannot find bundled example_matrix.csv.", call. = FALSE)
  }
  normalizePath(existing[[1]], mustWork = TRUE)
}

preview_table <- function(x, max_rows = 10, max_cols = 8) {
  x <- as.data.frame(x, check.names = FALSE)
  x[
    seq_len(min(nrow(x), max_rows)),
    seq_len(min(ncol(x), max_cols)),
    drop = FALSE
  ]
}

mod_data_hub_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_columns(
    bslib::card(
      bslib::card_header("Upload"),
      shiny::fileInput(ns("file"), "Upload CSV, TSV, or TXT"),
      shiny::textInput(ns("object_name"), "Object name", value = "uploaded_matrix"),
      shiny::actionButton(ns("register"), "Register object"),
      shiny::actionButton(ns("load_example"), "Load example matrix")
    ),
    bslib::card(
      bslib::card_header("Preview"),
      DT::DTOutput(ns("preview"))
    ),
    bslib::card(
      bslib::card_header("Objects"),
      DT::DTOutput(ns("objects"))
    )
  )
}

mod_data_hub_server <- function(id, registry) {
  shiny::moduleServer(id, function(input, output, session) {
    current_table <- shiny::reactiveVal(NULL)

    register_table <- function(table, name, source) {
      type <- detect_upload_type(table)
      validation <- if (identical(type, "matrix")) validate_matrix_like(table) else app_success(table)

      if (!validation$ok) {
        shiny::showNotification(validation$message, type = "error")
        return(NULL)
      }

      registry_add(
        registry,
        name = name,
        type = type,
        data = validation$value,
        source = source,
        warnings = validation$warnings
      )
    }

    shiny::observeEvent(input$register, {
      shiny::req(input$file)

      result <- tryCatch(
        {
          table <- read_user_table(input$file$datapath, filename = input$file$name)
          current_table(table)
          item <- register_table(table, input$object_name, input$file$name)

          if (!is.null(item)) {
            shiny::showNotification(paste("Registered", item$name), type = "message")
          }
          item
        },
        error = function(e) {
          shiny::showNotification(conditionMessage(e), type = "error")
          NULL
        }
      )

      invisible(result)
    })

    shiny::observeEvent(input$load_example, {
      result <- tryCatch(
        {
          path <- app_example_matrix_path()
          table <- read_user_table(path)
          current_table(table)
          item <- register_table(table, "example_matrix", basename(path))

          if (!is.null(item)) {
            shiny::showNotification(paste("Registered", item$name), type = "message")
          }
          item
        },
        error = function(e) {
          shiny::showNotification(conditionMessage(e), type = "error")
          NULL
        }
      )

      invisible(result)
    })

    output$preview <- DT::renderDT({
      table <- current_table()
      shiny::req(table)
      preview_table(table)
    }, rownames = FALSE)

    output$objects <- DT::renderDT(registry_list(registry), rownames = FALSE)
  })
}
