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

normalize_object_name <- function(name, fallback = "object") {
  if (is.null(name)) {
    name <- ""
  }
  name <- trimws(as.character(name))
  if (!nzchar(name)) {
    name <- fallback
  }
  name <- gsub("[^A-Za-z0-9._-]+", "_", name)
  name <- gsub("_+", "_", name)
  name <- gsub("^_+|_+$", "", name)
  if (!nzchar(name)) {
    name <- fallback
  }
  name
}

unique_registry_name <- function(registry, name) {
  listed <- shiny::isolate(registry_list(registry))
  if (!nrow(listed) || !name %in% listed$name) {
    return(name)
  }

  index <- 2L
  candidate <- paste0(name, "_", index)
  while (candidate %in% listed$name) {
    index <- index + 1L
    candidate <- paste0(name, "_", index)
  }
  candidate
}

validated_upload_value <- function(table) {
  type <- detect_upload_type(table)
  validation <- if (identical(type, "matrix")) validate_matrix_like(table) else app_success(table)
  list(type = type, validation = validation)
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
      prepared <- validated_upload_value(table)
      type <- prepared$type
      validation <- prepared$validation

      if (!validation$ok) {
        shiny::showNotification(validation$message, type = "error")
        return(NULL)
      }

      fallback_name <- if (is.null(source) || !nzchar(source)) "object" else tools::file_path_sans_ext(basename(source))
      clean_name <- normalize_object_name(name, fallback = fallback_name)
      clean_name <- unique_registry_name(registry, clean_name)
      registry_add(
        registry,
        name = clean_name,
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
          item <- register_table(table, input$object_name, input$file$name)

          if (!is.null(item)) {
            current_table(item$data)
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
          item <- register_table(table, "example_matrix", basename(path))

          if (!is.null(item)) {
            current_table(item$data)
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
