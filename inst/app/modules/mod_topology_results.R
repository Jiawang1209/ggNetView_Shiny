mod_topology_results_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      title = "Calculate",
      width = 420,
      bslib::accordion(
        bslib::accordion_panel(
          "Network topology",
          shiny::selectInput(ns("graph_id"), "Graph object", choices = character()),
          shiny::div(
            class = "topology-control-grid",
            shiny::numericInput(ns("topology_bootstrap"), "Topology bootstrap", value = 0, min = 0, step = 1),
            shiny::numericInput(ns("topology_workers"), "Topology workers", value = 1, min = 1, step = 1)
          ),
          shiny::checkboxInput(ns("topology_parallel_api"), "Use topology parallel API", value = FALSE),
          shiny::checkboxInput(ns("topology_parallel"), "Run topology workers in parallel", value = FALSE),
          shiny::actionButton(ns("calculate"), "Calculate topology", class = "w-100")
        ),
        bslib::accordion_panel(
          "Sample topology",
          shiny::selectInput(ns("matrix_id"), "Matrix for sample topology", choices = character()),
          shiny::div(
            class = "topology-control-grid",
            shiny::numericInput(ns("sample_bootstrap"), "Sample topology bootstrap", value = 0, min = 0, step = 1),
            shiny::numericInput(ns("sample_workers"), "Sample topology workers", value = 1, min = 1, step = 1)
          ),
          shiny::checkboxInput(ns("sample_parallel"), "Use sample topology parallel API", value = FALSE),
          shiny::actionButton(ns("calculate_sample_topology"), "Calculate sample topology", class = "w-100")
        ),
        bslib::accordion_panel(
          "Node centrality",
          shiny::checkboxInput(ns("weighted_centrality"), "Weighted centrality", value = FALSE),
          shiny::actionButton(ns("calculate_centrality"), "Calculate centrality", class = "w-100")
        ),
        bslib::accordion_panel(
          "IVI",
          shiny::selectInput(ns("ivi_scale"), "IVI scale", choices = c("range", "z-scale", "none")),
          shiny::actionButton(ns("calculate_ivi"), "Calculate IVI", class = "w-100")
        ),
        open = "Network topology"
      )
    ),
    bslib::layout_columns(
      bslib::card(
        bslib::card_header("Topology"),
        shiny::uiOutput(ns("placeholder")),
        shiny::uiOutput(ns("topology_metrics")),
        shinycssloaders::withSpinner(DT::DTOutput(ns("topology")), color = "#AE017E", type = 6),
        shiny::downloadButton(ns("download_topology"), "Download Topology CSV"),
        shiny::verbatimTextOutput(ns("status"))
      ),
      bslib::card(
        bslib::card_header("Robustness"),
        DT::DTOutput(ns("robustness")),
        shiny::downloadButton(ns("download_robustness"), "Download Robustness CSV")
      ),
      bslib::card(
        bslib::card_header("Node Metrics"),
        DT::DTOutput(ns("node_metrics")),
        shiny::downloadButton(ns("download_node_metrics"), "Download Node Metrics CSV")
      ),
      bslib::card(
        bslib::card_header("Sample Topology"),
        DT::DTOutput(ns("sample_topology")),
        shiny::downloadButton(ns("download_sample_topology"), "Download Sample Topology CSV")
      ),
      bslib::card(
        bslib::card_header("Sample Stats"),
        DT::DTOutput(ns("sample_stats")),
        shiny::downloadButton(ns("download_sample_stats"), "Download Sample Stats CSV")
      ),
      bslib::card(
        bslib::card_header("Sample Robustness"),
        DT::DTOutput(ns("sample_robustness")),
        shiny::downloadButton(ns("download_sample_robustness"), "Download Sample Robustness CSV")
      ),
      col_widths = c(12, 6, 6, 6, 6, 6)
    )
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

topology_download_handler <- function(table_fn, filename) {
  shiny::downloadHandler(
    filename = function() filename,
    content = function(file) write_registry_table(table_fn(), file)
  )
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
    sample_topology_table <- shiny::reactiveVal(data.frame())
    sample_robustness_table <- shiny::reactiveVal(data.frame())
    sample_stats_table <- shiny::reactiveVal(data.frame())
    status <- shiny::reactiveVal("No topology calculated yet.")

    shiny::observe({
      graph_choices <- registry_choices(registry, type = "graph")
      graph_selected <- shiny::isolate(input$graph_id)
      if (is.null(graph_selected) || !graph_selected %in% unname(graph_choices)) {
        graph_selected <- if (length(graph_choices)) unname(graph_choices[[1]]) else character()
      }
      matrix_choices <- registry_choices(registry, type = "matrix")
      matrix_selected <- shiny::isolate(input$matrix_id)
      if (is.null(matrix_selected) || !matrix_selected %in% unname(matrix_choices)) {
        matrix_selected <- if (length(matrix_choices)) unname(matrix_choices[[1]]) else character()
      }
      shiny::updateSelectInput(session, "graph_id", choices = graph_choices, selected = graph_selected)
      shiny::updateSelectInput(session, "matrix_id", choices = matrix_choices, selected = matrix_selected)
    })

    shiny::observeEvent(input$calculate, {
      shiny::req(input$graph_id)
      graph_item <- registry_get(registry, input$graph_id)
      shiny::req(graph_item)

      params <- list(
        parallel_api = isTRUE(input$topology_parallel_api),
        bootstrap = as.integer(input$topology_bootstrap %||% 0),
        parallel = isTRUE(input$topology_parallel),
        n_workers = as.integer(input$topology_workers %||% 1)
      )
      status(task_feedback_message("network topology", "running"))
      result <- with_task_feedback(
        session,
        "network topology",
        session$ns("calculate"),
        safe_topology(graph_item$data, params = params)
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
        params = c(list(metric = "network_topology"), params)
      )
      if (nrow(robustness) > 0L) {
        registry_add(
          registry,
          name = unique_output_name(paste0(graph_item$name, "_robustness")),
          type = "result",
          data = robustness,
          source = graph_item$id,
          params = c(list(metric = "network_robustness"), params)
        )
      }
      status(paste("Registered topology:", topology_name))
    })

    shiny::observeEvent(input$calculate_sample_topology, {
      shiny::req(input$graph_id, input$matrix_id)
      graph_item <- registry_get(registry, input$graph_id)
      matrix_item <- registry_get(registry, input$matrix_id)
      shiny::req(graph_item, matrix_item)

      params <- list(
        method = "cor",
        cor.method = "pearson",
        proc = "none",
        r.threshold = 0.2,
        p.threshold = 1,
        bootstrap = as.integer(input$sample_bootstrap %||% 0),
        parallel_api = isTRUE(input$sample_parallel),
        parallel = isTRUE(input$sample_parallel),
        n_workers = as.integer(input$sample_workers %||% 1)
      )
      status(task_feedback_message("sample topology", "running"))
      result <- with_task_feedback(
        session,
        "sample topology",
        session$ns("calculate_sample_topology"),
        safe_sample_topology(graph_item$data, matrix_item$data, params = params)
      )
      if (!result$ok) {
        sample_topology_table(empty_result_table())
        sample_robustness_table(empty_result_table())
        sample_stats_table(empty_result_table())
        detail <- if (!is.null(result$trace)) paste(result$message, result$trace, sep = "\n") else result$message
        status(detail)
        shiny::showNotification(result$message, type = "error")
        return()
      }

      topology <- as.data.frame(result$value$topology, check.names = FALSE)
      robustness <- as.data.frame(result$value$Robustness, check.names = FALSE)
      sample_stats <- as.data.frame(result$value$sample_stat, check.names = FALSE)
      sample_topology_table(topology)
      sample_robustness_table(robustness)
      sample_stats_table(sample_stats)

      source_ids <- paste(graph_item$id, matrix_item$id, sep = ",")
      topology_name <- unique_output_name(paste0(graph_item$name, "_sample_topology"))
      registry_add(
        registry,
        name = topology_name,
        type = "result",
        data = topology,
        source = source_ids,
        params = c(list(metric = "sample_topology"), params)
      )
      if (nrow(robustness) > 0L) {
        registry_add(
          registry,
          name = unique_output_name(paste0(graph_item$name, "_sample_robustness")),
          type = "result",
          data = robustness,
          source = source_ids,
          params = c(list(metric = "sample_robustness"), params)
        )
      }
      if (nrow(sample_stats) > 0L) {
        registry_add(
          registry,
          name = unique_output_name(paste0(graph_item$name, "_sample_stats")),
          type = "result",
          data = sample_stats,
          source = source_ids,
          params = c(list(metric = "sample_topology_stats"), params)
        )
      }
      status(paste("Registered sample topology:", topology_name))
      shiny::showNotification(paste("Registered sample topology:", topology_name), type = "message")
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

    output$placeholder <- shiny::renderUI({
      tb <- topology_table()
      if (is.data.frame(tb) && nrow(tb) > 0) return(NULL)
      ui_empty_state(
        icon = "diagram-3",
        title = "No graph selected",
        hint = "Build or pick a graph object to see results here."
      )
    })

    output$topology_metrics <- shiny::renderUI({
      tb <- topology_table()
      if (is.null(tb) || !is.data.frame(tb) || !nrow(tb)) return(NULL)
      pick <- function(nm) {
        v <- tb[[nm]]
        if (is.null(v)) return("—")
        tryCatch(round(v[[1]], 3), error = function(e) "—")
      }
      bslib::layout_columns(
        col_widths = c(3, 3, 3, 3),
        ggnv_value_box("Nodes", pick("Nodes_number"), icon = "diagram-3"),
        ggnv_value_box("Edges", pick("Edges_number"), icon = "share"),
        ggnv_value_box("Avg degree", pick("Average_degree"), icon = "graph-up"),
        ggnv_value_box("Modules", pick("Module_number"), icon = "grid-3x3-gap")
      )
    })

    output$topology <- DT::renderDT({
      shiny::validate(
        shiny::need(isTruthy(input$graph_id), "Select a graph object first.")
      )
      dt_table(topology_table())
    })
    output$robustness <- DT::renderDT(robustness_table(), rownames = FALSE)
    output$node_metrics <- DT::renderDT(node_metrics_table(), rownames = FALSE)
    output$sample_topology <- DT::renderDT(sample_topology_table(), rownames = FALSE)
    output$sample_stats <- DT::renderDT(sample_stats_table(), rownames = FALSE)
    output$sample_robustness <- DT::renderDT(sample_robustness_table(), rownames = FALSE)
    output$status <- shiny::renderText(status())

    output$download_topology <- topology_download_handler(topology_table, "ggnetview_topology.csv")
    output$download_robustness <- topology_download_handler(robustness_table, "ggnetview_robustness.csv")
    output$download_node_metrics <- topology_download_handler(node_metrics_table, "ggnetview_node_metrics.csv")
    output$download_sample_topology <- topology_download_handler(sample_topology_table, "ggnetview_sample_topology.csv")
    output$download_sample_stats <- topology_download_handler(sample_stats_table, "ggnetview_sample_stats.csv")
    output$download_sample_robustness <- topology_download_handler(sample_robustness_table, "ggnetview_sample_robustness.csv")
  })
}
