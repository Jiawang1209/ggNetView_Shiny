builder_choices_for_type <- function(type) {
  if (is.null(type) || length(type) != 1L || is.na(type) || !nzchar(type)) {
    return(c("Matrix" = "matrix", "Adjacency matrix" = "adjacency", "Edge table" = "edge_table"))
  }

  switch(type,
    matrix = c("Matrix" = "matrix"),
    adjacency = c("Adjacency matrix" = "adjacency"),
    edge_table = c("Edge table" = "edge_table"),
    c("Matrix" = "matrix", "Adjacency matrix" = "adjacency", "Edge table" = "edge_table")
  )
}

builder_matches_source_type <- function(builder, source_type) {
  if (is.null(builder) || length(builder) != 1L || is.na(builder) || !nzchar(builder)) {
    return(FALSE)
  }
  builder %in% unname(builder_choices_for_type(source_type))
}

graph_builder_params <- function(
  builder,
  method = "cor",
  cor_method = "pearson",
  proc = "none",
  r_threshold = 0.1,
  p_threshold = 1,
  module_method = "Fast_greedy"
) {
  if (!identical(builder, "matrix")) {
    return(list())
  }

  list(
    method = method,
    cor.method = cor_method,
    proc = proc,
    r.threshold = r_threshold,
    p.threshold = p_threshold,
    module.method = module_method
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
      shiny::selectInput(ns("method"), "Association method", choices = c("cor")),
      shiny::selectInput(ns("cor_method"), "Correlation", choices = c("pearson", "spearman")),
      shiny::selectInput(ns("proc"), "P-value adjustment", choices = c("none")),
      shiny::numericInput(ns("r_threshold"), "r threshold", value = 0.1, min = 0, max = 1, step = 0.01),
      shiny::numericInput(ns("p_threshold"), "p threshold", value = 1, min = 0, max = 1, step = 0.01),
      shiny::selectInput(
        ns("module_method"),
        "Module method",
        choices = c("Fast_greedy", "Walktrap", "Edge_betweenness", "Spinglass")
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

      if (!builder_matches_source_type(input$builder, source$type)) {
        message <- sprintf("Builder '%s' is not valid for source type '%s'.", input$builder, source$type)
        status(message)
        shiny::showNotification(message, type = "error")
        return()
      }

      params <- graph_builder_params(
        builder = input$builder,
        method = input$method,
        cor_method = input$cor_method,
        proc = input$proc,
        r_threshold = input$r_threshold,
        p_threshold = input$p_threshold,
        module_method = input$module_method
      )

      result <- safe_build_graph(source$data, input$builder, params = params)
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
        params = params
      )
      status(paste("Built graph:", item$name))
      shiny::showNotification(paste("Built graph:", item$name), type = "message")
    })

    output$status <- shiny::renderText(status())
  })
}
