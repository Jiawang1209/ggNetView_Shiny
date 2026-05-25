mod_export_center_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::card(
    bslib::card_header("Export Center"),
    shiny::selectInput(ns("object_id"), "Object", choices = character()),
    shiny::downloadButton(ns("download_manifest"), "Download Manifest"),
    shiny::downloadButton(ns("download_rds"), "Download RDS"),
    shiny::downloadButton(ns("download_csv"), "Download CSV"),
    shiny::downloadButton(ns("download_params"), "Download Parameters"),
    shiny::downloadButton(ns("download_png"), "Download Plot PNG"),
    shiny::downloadButton(ns("download_pdf"), "Download Plot PDF")
  )
}

safe_download_base <- function(item) {
  name <- item$name
  if (is.null(name) || !nzchar(trimws(name))) {
    name <- item$id
  }
  name <- gsub("[^A-Za-z0-9._-]+", "_", name)
  name <- gsub("_+", "_", name)
  name <- gsub("^_+|_+$", "", name)
  if (!nzchar(name)) {
    name <- item$id
  }
  name
}

registry_manifest <- function(registry) {
  items <- shiny::isolate(registry$items)
  if (!length(items)) {
    return(data.frame(
      id = character(),
      name = character(),
      type = character(),
      source = character(),
      created_at = as.POSIXct(character()),
      stringsAsFactors = FALSE
    ))
  }

  do.call(rbind, lapply(items, function(item) {
    source <- item$source
    if (is.null(source)) {
      source <- ""
    }

    data.frame(
      id = item$id,
      name = item$name,
      type = item$type,
      source = source,
      created_at = item$created_at,
      stringsAsFactors = FALSE
    )
  }))
}

mod_export_center_server <- function(id, registry) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observe({
      shiny::updateSelectInput(session, "object_id", choices = registry_choices(registry))
    })

    selected_item <- shiny::reactive({
      shiny::req(input$object_id)
      item <- registry_get(registry, input$object_id)
      shiny::validate(shiny::need(!is.null(item), "Selected object is no longer available."))
      item
    })

    output$download_manifest <- shiny::downloadHandler(
      filename = function() "ggnetview_manifest.csv",
      content = function(file) write_registry_table(registry_manifest(registry), file)
    )

    output$download_rds <- shiny::downloadHandler(
      filename = function() paste0(safe_download_base(selected_item()), ".rds"),
      content = function(file) write_registry_object(selected_item()$data, file)
    )

    output$download_csv <- shiny::downloadHandler(
      filename = function() paste0(safe_download_base(selected_item()), ".csv"),
      content = function(file) {
        data <- selected_item()$data
        if (!is.matrix(data) && !is.data.frame(data)) {
          data <- data.frame(value = utils::capture.output(utils::str(data)))
        }
        write_registry_table(data, file)
      }
    )

    output$download_params <- shiny::downloadHandler(
      filename = function() paste0(safe_download_base(selected_item()), "_params.json"),
      content = function(file) write_registry_params(selected_item()$params, file)
    )

    output$download_png <- shiny::downloadHandler(
      filename = function() paste0(safe_download_base(selected_item()), ".png"),
      content = function(file) {
        shiny::validate(shiny::need(identical(selected_item()$type, "plot"), "PNG/PDF export requires a plot object."))
        write_plot_png(selected_item()$data, file)
      }
    )

    output$download_pdf <- shiny::downloadHandler(
      filename = function() paste0(safe_download_base(selected_item()), ".pdf"),
      content = function(file) {
        shiny::validate(shiny::need(identical(selected_item()$type, "plot"), "PNG/PDF export requires a plot object."))
        write_plot_pdf(selected_item()$data, file)
      }
    )
  })
}
