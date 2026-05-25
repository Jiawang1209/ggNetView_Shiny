mod_topology_results_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_columns(
    bslib::card(
      bslib::card_header("Calculate"),
      shiny::selectInput(ns("graph_id"), "Graph object", choices = character()),
      shiny::actionButton(ns("calculate"), "Calculate topology"),
      shiny::checkboxInput(ns("weighted_centrality"), "Weighted centrality", value = FALSE),
      shiny::actionButton(ns("calculate_centrality"), "Calculate centrality"),
      shiny::selectInput(ns("ivi_scale"), "IVI scale", choices = c("range", "z-scale", "none")),
      shiny::actionButton(ns("calculate_ivi"), "Calculate IVI"),
      shiny::numericInput(ns("zi_threshold"), "Zi threshold", value = 2.5, min = 0, step = 0.1),
      shiny::numericInput(ns("pi_threshold"), "Pi threshold", value = 0.62, min = 0, max = 1, step = 0.01),
      shiny::actionButton(ns("calculate_zipi"), "Calculate Zi-Pi")
    ),
    bslib::card(
      bslib::card_header("Topology"),
      DT::DTOutput(ns("topology")),
      shiny::verbatimTextOutput(ns("status"))
    ),
    bslib::card(
      bslib::card_header("Robustness"),
      DT::DTOutput(ns("robustness"))
    ),
    bslib::card(
      bslib::card_header("Node Metrics"),
      DT::DTOutput(ns("node_metrics"))
    ),
    col_widths = c(4, 8, 6, 6)
  )
}

topology_result_table <- function(value) {
  if (is.data.frame(value)) {
    return(value)
  }

  if (is.list(value) && is.data.frame(value$topology)) {
    return(value$topology)
  }

  as.data.frame(value)
}

topology_robustness_table <- function(value) {
  if (is.list(value) && is.data.frame(value$Robustness)) {
    return(value$Robustness)
  }

  data.frame()
}

empty_result_table <- function() {
  data.frame()
}

mod_topology_results_server <- function(id, registry) {
  shiny::moduleServer(id, function(input, output, session) {
    unique_output_name <- function(base) {
      suffix <- paste0(format(Sys.time(), "%Y%m%d_%H%M%S"), "_", sprintf("%04d", sample.int(9999, 1)))
      paste0(base, "_", suffix)
    }

    topology_table <- shiny::reactiveVal(data.frame())
    robustness_table <- shiny::reactiveVal(data.frame())
    node_metrics_table <- shiny::reactiveVal(data.frame())
    status <- shiny::reactiveVal("No topology calculated yet.")

    shiny::observe({
      shiny::updateSelectInput(session, "graph_id", choices = registry_choices(registry, type = "graph"))
    })

    shiny::observeEvent(input$calculate, {
      shiny::req(input$graph_id)
      graph_item <- registry_get(registry, input$graph_id)
      shiny::req(graph_item)

      status(task_feedback_message("network topology", "running"))
      result <- with_task_feedback(
        session,
        "network topology",
        session$ns("calculate"),
        safe_topology(graph_item$data)
      )
      if (!result$ok) {
        topology_table(empty_result_table())
        robustness_table(empty_result_table())
        detail <- if (!is.null(result$trace)) paste(result$message, result$trace, sep = "\n") else result$message
        status(detail)
        shiny::showNotification(result$message, type = "error")
        return()
      }

      table <- topology_result_table(result$value)
      robustness <- topology_robustness_table(result$value)
      topology_name <- unique_output_name(paste0(graph_item$name, "_topology"))
      topology_table(table)
      robustness_table(robustness)
      registry_add(
        registry,
        name = topology_name,
        type = "result",
        data = table,
        source = graph_item$id,
        params = list(metric = "network_topology")
      )
      if (nrow(robustness) > 0L) {
        registry_add(
          registry,
          name = unique_output_name(paste0(graph_item$name, "_robustness")),
          type = "result",
          data = robustness,
          source = graph_item$id,
          params = list(metric = "network_robustness")
        )
      }
      status(paste("Registered topology:", topology_name))
    })

    register_node_metric <- function(metric, result, graph_item, params = list()) {
      if (!result$ok) {
        node_metrics_table(empty_result_table())
        detail <- if (!is.null(result$trace)) paste(result$message, result$trace, sep = "\n") else result$message
        status(detail)
        shiny::showNotification(result$message, type = "error")
        return(NULL)
      }

      table <- as.data.frame(result$value, check.names = FALSE)
      result_name <- unique_output_name(paste0(graph_item$name, "_", metric))
      node_metrics_table(table)
      registry_add(
        registry,
        name = result_name,
        type = "result",
        data = table,
        source = graph_item$id,
        params = c(list(metric = metric), params)
      )
      status(paste("Registered", metric, ":", result_name))
      shiny::showNotification(paste("Registered", metric, ":", result_name), type = "message")
    }

    shiny::observeEvent(input$calculate_centrality, {
      shiny::req(input$graph_id)
      graph_item <- registry_get(registry, input$graph_id)
      shiny::req(graph_item)

      status(task_feedback_message("node centrality", "running"))
      result <- with_task_feedback(
        session,
        "node centrality",
        session$ns("calculate_centrality"),
        safe_node_centrality(graph_item$data, measures = "all", weighted = input$weighted_centrality)
      )
      register_node_metric(
        "node_centrality",
        result,
        graph_item,
        params = list(weighted = input$weighted_centrality)
      )
    })

    shiny::observeEvent(input$calculate_ivi, {
      shiny::req(input$graph_id)
      graph_item <- registry_get(registry, input$graph_id)
      shiny::req(graph_item)

      status(task_feedback_message("node IVI", "running"))
      result <- with_task_feedback(
        session,
        "node IVI",
        session$ns("calculate_ivi"),
        safe_node_ivi(graph_item$data, scale = input$ivi_scale, ncores = 1L)
      )
      register_node_metric(
        "node_ivi",
        result,
        graph_item,
        params = list(scale = input$ivi_scale, ncores = 1L)
      )
    })

    shiny::observeEvent(input$calculate_zipi, {
      shiny::req(input$graph_id)
      graph_item <- registry_get(registry, input$graph_id)
      shiny::req(graph_item)

      status(task_feedback_message("Zi-Pi classification", "running"))
      result <- with_task_feedback(
        session,
        "Zi-Pi classification",
        session$ns("calculate_zipi"),
        safe_zipi(
          graph_item$data,
          zi_threshold = input$zi_threshold,
          pi_threshold = input$pi_threshold
        )
      )
      register_node_metric(
        "zipi",
        result,
        graph_item,
        params = list(zi_threshold = input$zi_threshold, pi_threshold = input$pi_threshold)
      )
    })

    output$topology <- DT::renderDT(topology_table(), rownames = FALSE)
    output$robustness <- DT::renderDT(robustness_table(), rownames = FALSE)
    output$node_metrics <- DT::renderDT(node_metrics_table(), rownames = FALSE)
    output$status <- shiny::renderText(status())
  })
}
