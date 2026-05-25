mod_compare_environment_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_columns(
    bslib::card(
      bslib::card_header("Compare Networks"),
      shiny::selectizeInput(ns("compare_graph_ids"), "Graph objects", choices = character(), multiple = TRUE),
      shiny::selectInput(
        ns("compare_layout"),
        "Group layout",
        choices = c("circle", "row", "column", "square", "diamond", "triangle", "triangle_down", "snake")
      ),
      shiny::selectInput(ns("link_level"), "Link level", choices = c("Module", "Node")),
      shiny::checkboxInput(ns("scale_groups"), "Scale groups", value = TRUE),
      shiny::actionButton(ns("run_compare"), "Compare networks")
    ),
    bslib::card(
      bslib::card_header("Environment Links"),
      shiny::selectInput(ns("spec_id"), "Spec matrix", choices = character()),
      shiny::selectInput(ns("env_id"), "Environment matrix", choices = character()),
      shiny::selectInput(ns("relation_method"), "Relation method", choices = c("correlation", "mantel")),
      shiny::selectInput(ns("cor_method"), "Correlation", choices = c("pearson", "spearman", "kendall")),
      shiny::checkboxInput(ns("drop_nonsig"), "Drop non-significant links", value = FALSE),
      shiny::actionButton(ns("run_environment"), "Run environment link"),
      shiny::actionButton(ns("run_mantel"), "Run Mantel table")
    ),
    bslib::card(
      bslib::card_header("Preview"),
      shiny::plotOutput(ns("plot"), height = 650),
      shiny::verbatimTextOutput(ns("status"))
    ),
    bslib::card(
      bslib::card_header("Statistics"),
      DT::DTOutput(ns("stats"))
    ),
    col_widths = c(4, 4, 8, 12)
  )
}

mod_compare_environment_server <- function(id, registry) {
  shiny::moduleServer(id, function(input, output, session) {
    plot_obj <- shiny::reactiveVal(NULL)
    stats_table <- shiny::reactiveVal(data.frame())
    status <- shiny::reactiveVal("No comparison or environment result yet.")

    unique_output_name <- function(base) {
      suffix <- paste0(format(Sys.time(), "%Y%m%d_%H%M%S"), "_", sprintf("%04d", sample.int(9999, 1)))
      paste0(base, "_", suffix)
    }

    shiny::observe({
      graph_choices <- registry_choices(registry, type = "graph")
      shiny::updateSelectizeInput(session, "compare_graph_ids", choices = graph_choices, server = TRUE)
    })

    shiny::observe({
      matrix_choices <- registry_choices_by_type(registry, c("matrix"))
      env_choices <- registry_choices_by_type(registry, c("env_matrix", "matrix"))
      shiny::updateSelectInput(session, "spec_id", choices = matrix_choices)
      shiny::updateSelectInput(session, "env_id", choices = env_choices)
    })

    register_plot_result <- function(name, plot, source, params) {
      registry_add(
        registry,
        name = name,
        type = "plot",
        data = plot,
        source = source,
        params = params
      )
    }

    register_stats_result <- function(name, stats, source, params) {
      registry_add(
        registry,
        name = name,
        type = "result",
        data = stats,
        source = source,
        params = params
      )
    }

    shiny::observeEvent(input$run_compare, {
      shiny::req(input$compare_graph_ids)
      if (length(input$compare_graph_ids) < 2L) {
        message <- "Select at least two graph objects."
        status(message)
        shiny::showNotification(message, type = "error")
        return()
      }

      items <- lapply(input$compare_graph_ids, function(id) registry_get(registry, id))
      graphs <- lapply(items, `[[`, "data")
      names(graphs) <- vapply(items, function(item) item$name, character(1))
      params <- list(
        group_layout = input$compare_layout,
        link_level = input$link_level,
        scale_groups = input$scale_groups
      )

      result <- safe_multi_network_compare(graphs, params = params)
      if (!result$ok) {
        detail <- if (!is.null(result$trace)) paste(result$message, result$trace, sep = "\n") else result$message
        status(detail)
        shiny::showNotification(result$message, type = "error")
        return()
      }

      plot_obj(result$value$plot)
      stats <- result$value$link_info
      if (is.null(stats)) {
        stats <- data.frame()
      }
      if (is.list(stats) && !is.data.frame(stats)) {
        stats <- data.frame(value = utils::capture.output(utils::str(stats)), stringsAsFactors = FALSE)
      }
      stats_table(stats)

      source_ids <- paste(input$compare_graph_ids, collapse = ",")
      plot_item <- register_plot_result(
        unique_output_name("multi_network_compare_plot"),
        result$value$plot,
        source_ids,
        params
      )
      register_stats_result(
        unique_output_name("multi_network_compare_links"),
        stats,
        source_ids,
        params
      )
      status(paste("Registered comparison plot:", plot_item$name))
      shiny::showNotification(paste("Registered comparison plot:", plot_item$name), type = "message")
    })

    shiny::observeEvent(input$run_environment, {
      shiny::req(input$spec_id, input$env_id)
      spec_item <- registry_get(registry, input$spec_id)
      env_item <- registry_get(registry, input$env_id)
      shiny::req(spec_item, env_item)

      spec <- as.data.frame(t(as.matrix(spec_item$data)), check.names = FALSE)
      env <- as.data.frame(env_item$data, check.names = FALSE)
      if (nrow(env) != nrow(spec)) {
        env <- as.data.frame(t(as.matrix(env_item$data)), check.names = FALSE)
      }

      params <- list(
        relation_method = input$relation_method,
        cor.method = input$cor_method,
        drop_nonsig = input$drop_nonsig
      )
      result <- safe_environment_link(env = env, spec = spec, params = params)
      if (!result$ok) {
        detail <- if (!is.null(result$trace)) paste(result$message, result$trace, sep = "\n") else result$message
        status(detail)
        shiny::showNotification(result$message, type = "error")
        return()
      }

      plot_obj(result$value$plot)
      stats_table(result$value$stats)
      source_ids <- paste(input$spec_id, input$env_id, sep = ",")
      plot_item <- register_plot_result(
        unique_output_name("environment_link_plot"),
        result$value$plot,
        source_ids,
        params
      )
      register_stats_result(
        unique_output_name("environment_link_stats"),
        result$value$stats,
        source_ids,
        params
      )
      status(paste("Registered environment link plot:", plot_item$name))
      shiny::showNotification(paste("Registered environment link plot:", plot_item$name), type = "message")
    })

    shiny::observeEvent(input$run_mantel, {
      shiny::req(input$spec_id, input$env_id)
      spec_item <- registry_get(registry, input$spec_id)
      env_item <- registry_get(registry, input$env_id)
      shiny::req(spec_item, env_item)

      spec <- as.data.frame(t(as.matrix(spec_item$data)), check.names = FALSE)
      env <- as.data.frame(env_item$data, check.names = FALSE)
      if (nrow(env) != nrow(spec)) {
        env <- as.data.frame(t(as.matrix(env_item$data)), check.names = FALSE)
      }

      params <- list(method = input$cor_method, permutations = 99L)
      result <- safe_mantel_pairwise(spec, env, params = params)
      if (!result$ok) {
        detail <- if (!is.null(result$trace)) paste(result$message, result$trace, sep = "\n") else result$message
        status(detail)
        shiny::showNotification(result$message, type = "error")
        return()
      }

      stats_table(result$value)
      item <- register_stats_result(
        unique_output_name("mantel_pairwise_stats"),
        result$value,
        paste(input$spec_id, input$env_id, sep = ","),
        params
      )
      status(paste("Registered Mantel result:", item$name))
      shiny::showNotification(paste("Registered Mantel result:", item$name), type = "message")
    })

    output$plot <- shiny::renderPlot({
      shiny::req(plot_obj())
      plot_obj()
    })

    output$stats <- DT::renderDT(stats_table(), rownames = FALSE)
    output$status <- shiny::renderText(status())
  })
}
