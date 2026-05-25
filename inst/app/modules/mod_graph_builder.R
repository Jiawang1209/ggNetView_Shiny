builder_choices_for_type <- function(type) {
  switch(type,
    matrix = c("Matrix" = "matrix"),
    adjacency = c("Adjacency matrix" = "adjacency"),
    edge_table = c("Edge table" = "edge_table"),
    c("Matrix" = "matrix", "Adjacency matrix" = "adjacency", "Edge table" = "edge_table")
  )
}

mod_graph_builder_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_columns(
    bslib::card(
      bslib::card_header("Build Graph"),
      shiny::selectInput(ns("source_id"), "Source object", choices = character()),
      shiny::selectInput(
        ns("builder"),
        "Builder",
        choices = c("Matrix" = "matrix", "Adjacency matrix" = "adjacency", "Edge table" = "edge_table")
      ),
      shiny::textInput(ns("graph_name"), "Graph name", value = "network_graph"),
      shiny::actionButton(ns("build"), "Build graph")
    ),
    bslib::card(
      bslib::card_header("Build status"),
      shiny::verbatimTextOutput(ns("status"))
    )
  )
}

mod_graph_builder_server <- function(id, registry) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observe({
      choices <- registry_choices_by_type(registry, c("matrix", "adjacency", "edge_table"))
      shiny::updateSelectInput(session, "source_id", choices = choices)
    })

    shiny::observe({
      source <- if (!is.null(input$source_id) && length(input$source_id) == 1L && nzchar(input$source_id)) {
        registry_get(registry, input$source_id)
      } else {
        NULL
      }
      source_type <- if (is.null(source)) NULL else source$type
      shiny::updateSelectInput(session, "builder", choices = builder_choices_for_type(source_type))
    })

    status <- shiny::reactiveVal("No graph built yet.")

    shiny::observeEvent(input$build, {
      shiny::req(input$source_id)
      source <- registry_get(registry, input$source_id)
      shiny::req(source)

      result <- safe_build_graph(source$data, input$builder, params = list())
      if (!result$ok) {
        detail <- if (!is.null(result$trace)) paste(result$message, result$trace, sep = "\n") else result$message
        status(detail)
        shiny::showNotification(result$message, type = "error")
        return()
      }

      if (!inherits(result$value, "igraph")) {
        message <- "Graph builder did not return an igraph object."
        status(message)
        shiny::showNotification(message, type = "error")
        return()
      }

      item <- registry_add(
        registry,
        name = input$graph_name,
        type = "graph",
        data = result$value,
        source = source$id,
        params = list(builder = input$builder)
      )
      status(paste("Built graph:", item$name))
      shiny::showNotification(paste("Built graph:", item$name), type = "message")
    })

    output$status <- shiny::renderText(status())
  })
}
