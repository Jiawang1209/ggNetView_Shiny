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

upload_type_choices <- function() {
  c(
    "Auto detect" = "auto",
    "Matrix" = "matrix",
    "Adjacency matrix" = "adjacency",
    "Edge table" = "edge_table",
    "Module table" = "module_table",
    "Annotation" = "annotation",
    "WGCNA/TOM matrix" = "wgcna_tom",
    "Sample metadata" = "sample_metadata",
    "Environment matrix" = "env_matrix"
  )
}

validated_upload_value <- function(table, requested_type = "auto") {
  detected_type <- detect_upload_type(table)
  type <- if (identical(requested_type, "auto")) detected_type else requested_type
  validation <- if (type %in% c("matrix", "adjacency", "wgcna_tom", "env_matrix")) {
    validate_matrix_like(table)
  } else {
    app_success(table)
  }
  list(type = type, validation = validation)
}

mod_data_hub_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_columns(
    bslib::card(
      bslib::card_header("Upload"),
      shiny::fileInput(ns("file"), "Upload CSV, TSV, or TXT"),
      shiny::selectInput(ns("upload_type"), "Object type", choices = upload_type_choices(), selected = "auto"),
      shiny::textInput(ns("object_name"), "Object name", value = "uploaded_matrix"),
      shiny::actionButton(ns("register"), "Register object"),
      shiny::actionButton(ns("load_example"), "Load example matrix"),
      shiny::actionButton(ns("load_gallery"), "Load manual examples"),
      shiny::selectInput(
        ns("gallery_recipe"),
        "Gallery recipe",
        choices = stats::setNames(gallery_recipe_manifest()$recipe, gallery_recipe_manifest()$label)
      ),
      shiny::actionButton(ns("run_gallery_recipe"), "Run gallery recipe")
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

    register_table <- function(table, name, source, requested_type = "auto") {
      prepared <- validated_upload_value(table, requested_type = requested_type)
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
          item <- register_table(table, input$object_name, input$file$name, requested_type = input$upload_type)

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
          item <- register_table(table, "example_matrix", basename(path), requested_type = "matrix")

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

    shiny::observeEvent(input$load_gallery, {
      result <- tryCatch(
        {
          items <- register_gallery_examples(registry)
          if (length(items)) {
            current_table(items[[1]]$data)
          }
          shiny::showNotification("Registered manual example workflow objects", type = "message")
          items
        },
        error = function(e) {
          shiny::showNotification(conditionMessage(e), type = "error")
          NULL
        }
      )

      invisible(result)
    })

    shiny::observeEvent(input$run_gallery_recipe, {
      shiny::req(input$gallery_recipe)
      result <- run_gallery_recipe(registry, input$gallery_recipe)
      if (!result$ok) {
        detail <- if (!is.null(result$trace)) paste(result$message, result$trace, sep = "\n") else result$message
        shiny::showNotification(detail, type = "error")
        return(invisible(result))
      }

      items <- result$value$items
      if (length(items) && (is.data.frame(items[[1]]$data) || is.matrix(items[[1]]$data))) {
        current_table(items[[1]]$data)
      }
      names <- vapply(items, `[[`, character(1), "name")
      shiny::showNotification(paste("Registered gallery recipe:", paste(names, collapse = ", ")), type = "message")
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
