mod_rmt_builder_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_columns(
    bslib::card(
      bslib::card_header("RMT Threshold Scan"),
      shiny::selectInput(ns("source_id"), "Matrix source", choices = character()),
      shiny::selectInput(ns("method"), "Association method", choices = c("cor", "Hmisc", "WGCNA", "SPARCC", "SpiecEasi")),
      shiny::selectInput(ns("transform_method"), "Transform", choices = c("none", "scale", "center", "log2", "log10", "ln", "rrarefy", "rrarefy_relative")),
      shiny::selectInput(ns("cor_method"), "Correlation", choices = c("pearson", "spearman", "kendall")),
      shiny::actionButton(ns("run_rmt"), "Run RMT", class = "w-100")
    ),
    bslib::card(
      bslib::card_header("RMT-assisted Graph"),
      shiny::numericInput(ns("r_threshold"), "r threshold", value = 0.1, min = 0, max = 1, step = 0.01),
      shiny::numericInput(ns("p_threshold"), "p threshold", value = 1, min = 0, max = 1, step = 0.01),
      shiny::selectInput(ns("proc"), "P-value adjustment", choices = c("none", "BH", "holm", "bonferroni")),
      shiny::selectInput(
        ns("module_method"),
        "Module method",
        choices = c("Fast_greedy", "Walktrap", "Edge_betweenness", "Spinglass")
      ),
      shiny::textInput(ns("graph_name"), "Graph name", value = "rmt_network_graph"),
      shiny::actionButton(ns("build"), "Build graph with RMT", class = "w-100")
    ),
    bslib::card(
      bslib::card_header("RMT status"),
      shiny::verbatimTextOutput(ns("status"))
    ),
    col_widths = c(5, 5, 12)
  )
}

rmt_builder_params <- function(
  method = "cor",
  cor_method = "pearson",
  proc = "none",
  r_threshold = 0.1,
  p_threshold = 1,
  module_method = "Fast_greedy",
  transform_method = "none"
) {
  list(
    transfrom.method = transform_method,
    method = method,
    cor.method = cor_method,
    proc = proc,
    r.threshold = r_threshold,
    p.threshold = p_threshold,
    module.method = module_method
  )
}

mod_rmt_builder_server <- function(id, registry) {
  shiny::moduleServer(id, function(input, output, session) {
    status <- shiny::reactiveVal("No RMT result yet.")

    shiny::observe({
      matrix_choices <- registry_choices(registry, type = "matrix")
      shiny::updateSelectInput(session, "source_id", choices = matrix_choices)
    })

    selected_matrix <- shiny::reactive({
      shiny::req(input$source_id)
      registry_get(registry, input$source_id)
    })

    shiny::observeEvent(input$run_rmt, {
      source <- selected_matrix()
      shiny::req(source)

      status(task_feedback_message("RMT threshold scan", "running"))
      result <- with_task_feedback(
        session,
        "RMT threshold scan",
        session$ns("run_rmt"),
        safe_rmt_threshold(
          source$data,
          params = list(
            transfrom.method = input$transform_method,
            method = input$method,
            cor.method = input$cor_method,
            min.mat.dim = 2,
            verbose = FALSE
          )
        )
      )

      if (!result$ok) {
        detail <- if (!is.null(result$trace)) paste(result$message, result$trace, sep = "\n") else result$message
        status(detail)
        shiny::showNotification(result$message, type = "error")
        return()
      }

      item <- registry_add(
        registry,
        name = paste0(source$name, "_rmt"),
        type = "result",
        data = result$value,
        source = source$id,
        params = list(
          transfrom.method = input$transform_method,
          method = input$method,
          cor.method = input$cor_method
        )
      )
      status(paste("Registered RMT result:", item$name))
      shiny::showNotification(paste("Registered RMT result:", item$name), type = "message")
    })

    shiny::observeEvent(input$build, {
      source <- selected_matrix()
      shiny::req(source)

      params <- rmt_builder_params(
        method = input$method,
        cor_method = input$cor_method,
        proc = input$proc,
        r_threshold = input$r_threshold,
        p_threshold = input$p_threshold,
        module_method = input$module_method,
        transform_method = input$transform_method
      )

      status(task_feedback_message("RMT graph build", "running"))
      result <- with_task_feedback(
        session,
        "RMT graph build",
        session$ns("build"),
        safe_graph_builder(
          "matrix_rmt",
          inputs = list(matrix = source$data),
          params = params
        )
      )
      if (!result$ok) {
        detail <- if (!is.null(result$trace)) paste(result$message, result$trace, sep = "\n") else result$message
        status(detail)
        shiny::showNotification(result$message, type = "error")
        return()
      }

      if (!inherits(result$value, "igraph")) {
        message <- "RMT graph builder did not return an igraph object."
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
        params = graph_builder_registry_params("matrix_rmt", params, source$id)
      )
      status(paste(
        "Built RMT graph:", item$name,
        "\nParameters:", graph_builder_params_json(params)
      ))
      shiny::showNotification(paste("Built RMT graph:", item$name), type = "message")
    })

    output$status <- shiny::renderText(status())
  })
}
