mod_graph_explorer_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      title = "Graph Controls",
      width = 400,
      shiny::selectInput(ns("graph_id"), "Graph object", choices = character()),
      shiny::actionButton(ns("register_info"), "Register graph info", class = "w-100"),
      shiny::hr(),
      bslib::accordion(
        open = c("Module subgraph", "Sample subgraph"),
        bslib::accordion_panel(
          "Module subgraph",
          shiny::selectInput(ns("module"), "Module", choices = character()),
          shiny::actionButton(ns("register_module_subgraph"), "Register module subgraph", class = "w-100")
        ),
        bslib::accordion_panel(
          "Sample subgraph",
          shiny::selectInput(ns("matrix_id"), "Sample matrix", choices = character()),
          shiny::selectizeInput(ns("sample_ids"), "Samples", choices = character(), multiple = TRUE),
          bslib::layout_columns(
            shiny::numericInput(ns("min_abundance"), "Min abundance", value = 0, min = 0, step = 0.001),
            shiny::selectInput(ns("combine"), "Combine samples", choices = c("union", "intersect")),
            col_widths = c(6, 6)
          ),
          shiny::actionButton(ns("register_sample_subgraph"), "Register sample subgraph", class = "w-100")
        )
      ),
      shiny::verbatimTextOutput(ns("status"))
    ),
    bslib::layout_columns(
      bslib::card(
        bslib::card_header("Summary"),
        shiny::uiOutput(ns("metrics")),
        shiny::verbatimTextOutput(ns("summary"))
      ),
      bslib::card(
        bslib::card_header("Nodes"),
        DT::DTOutput(ns("nodes"))
      ),
      bslib::card(
        bslib::card_header("Edges"),
        DT::DTOutput(ns("edges"))
      ),
      bslib::card(
        bslib::card_header("Module Stats"),
        DT::DTOutput(ns("module_stats"))
      ),
      bslib::card(
        bslib::card_header("Sample Stats"),
        DT::DTOutput(ns("sample_stats"))
      ),
      col_widths = c(12, 6, 6, 6, 6)
    )
  )
}

graph_nodes_table <- function(graph) {
  if (!inherits(graph, "igraph")) {
    return(data.frame())
  }

  nodes <- tryCatch(
    igraph::as_data_frame(graph, what = "vertices"),
    error = function(e) data.frame()
  )

  if (nrow(nodes) == 0L && igraph::vcount(graph) > 0L) {
    node_names <- igraph::vertex_attr(graph, "name")
    if (is.null(node_names)) {
      node_names <- as.character(seq_len(igraph::vcount(graph)))
    }
    nodes <- data.frame(name = node_names, stringsAsFactors = FALSE)
  }

  nodes
}

graph_edges_table <- function(graph) {
  if (!inherits(graph, "igraph")) {
    return(data.frame())
  }

  edges <- tryCatch(
    igraph::as_data_frame(graph, what = "edges"),
    error = function(e) data.frame()
  )

  if (nrow(edges) == 0L) {
    edges <- data.frame(from = character(), to = character(), stringsAsFactors = FALSE)
  }

  edges
}

mod_graph_explorer_server <- function(id, registry) {
  shiny::moduleServer(id, function(input, output, session) {
    status <- shiny::reactiveVal("No subgraph registered yet.")
    module_result <- shiny::reactiveVal(NULL)
    sample_result <- shiny::reactiveVal(NULL)

    shiny::observe({
      shiny::updateSelectInput(session, "graph_id", choices = registry_choices(registry, type = "graph"))
    })

    shiny::observe({
      shiny::updateSelectInput(session, "matrix_id", choices = registry_choices(registry, type = "matrix"))
    })

    selected_graph <- shiny::reactive({
      shiny::req(input$graph_id)
      registry_get(registry, input$graph_id)
    })

    shiny::observe({
      item <- selected_graph()
      shiny::req(item)
      shiny::updateSelectInput(session, "module", choices = graph_module_choices(item$data))
    })

    shiny::observe({
      if (is.null(input$matrix_id) || !nzchar(input$matrix_id)) {
        shiny::updateSelectizeInput(session, "sample_ids", choices = character(), server = TRUE)
        return()
      }
      item <- registry_get(registry, input$matrix_id)
      if (is.null(item) || is.null(colnames(item$data))) {
        shiny::updateSelectizeInput(session, "sample_ids", choices = character(), server = TRUE)
        return()
      }
      samples <- colnames(item$data)
      shiny::updateSelectizeInput(session, "sample_ids", choices = stats::setNames(samples, samples), server = TRUE)
    })

    shiny::observeEvent(input$register_info, {
      item <- selected_graph()
      shiny::req(item)

      result <- safe_graph_info(item$data)
      if (!result$ok) {
        detail <- if (!is.null(result$trace)) paste(result$message, result$trace, sep = "\n") else result$message
        status(detail)
        shiny::showNotification(result$message, type = "error")
        return()
      }

      info_item <- registry_add(
        registry,
        name = paste0(item$name, "_info"),
        type = "result",
        data = result$value,
        source = item$id,
        params = list(action = "get_info_from_graph")
      )
      status(paste("Registered graph info:", info_item$name))
      shiny::showNotification(paste("Registered graph info:", info_item$name), type = "message")
    })

    shiny::observeEvent(input$register_module_subgraph, {
      item <- selected_graph()
      shiny::req(item, input$module)

      result <- safe_module_subgraph(item$data, select_module = input$module)
      if (!result$ok) {
        detail <- if (!is.null(result$trace)) paste(result$message, result$trace, sep = "\n") else result$message
        status(detail)
        shiny::showNotification(result$message, type = "error")
        return()
      }

      module_result(result$value)
      graph <- subgraph_selected_graph(result$value)
      if (is.null(graph)) {
        message <- "No module subgraph was returned for the selected module."
        status(message)
        shiny::showNotification(message, type = "error")
        return()
      }

      sub_item <- registry_add(
        registry,
        name = paste0(item$name, "_module_", input$module),
        type = "graph",
        data = graph,
        source = item$id,
        params = list(action = "get_subgraph", select_module = input$module)
      )
      status(paste("Registered module subgraph:", sub_item$name))
      shiny::showNotification(paste("Registered module subgraph:", sub_item$name), type = "message")
    })

    shiny::observeEvent(input$register_sample_subgraph, {
      item <- selected_graph()
      matrix_item <- registry_get(registry, input$matrix_id)
      shiny::req(item, matrix_item, input$sample_ids)

      result <- safe_sample_subgraph(
        item$data,
        matrix = matrix_item$data,
        select_sample = input$sample_ids,
        min_abundance = input$min_abundance,
        combine = input$combine
      )
      if (!result$ok) {
        detail <- if (!is.null(result$trace)) paste(result$message, result$trace, sep = "\n") else result$message
        status(detail)
        shiny::showNotification(result$message, type = "error")
        return()
      }

      sample_result(result$value)
      graph <- subgraph_selected_graph(result$value)
      if (is.null(graph)) {
        message <- "No sample subgraph was returned for the selected samples."
        status(message)
        shiny::showNotification(message, type = "error")
        return()
      }

      sample_label <- paste(input$sample_ids, collapse = "_")
      sub_item <- registry_add(
        registry,
        name = paste0(item$name, "_samples_", sample_label),
        type = "graph",
        data = graph,
        source = paste(item$id, matrix_item$id, sep = ","),
        params = list(
          action = "get_sample_subgraph",
          select_sample = input$sample_ids,
          min_abundance = input$min_abundance,
          combine = input$combine
        )
      )
      status(paste("Registered sample subgraph:", sub_item$name))
      shiny::showNotification(paste("Registered sample subgraph:", sub_item$name), type = "message")
    })

    output$metrics <- shiny::renderUI({
      item <- selected_graph()
      if (is.null(item)) return(NULL)
      ig <- tryCatch(coerce_tbl_graph(item$data), error = function(e) NULL)
      if (is.null(ig)) return(NULL)
      n_nodes <- tryCatch(igraph::gorder(ig), error = function(e) NA_integer_)
      n_edges <- tryCatch(igraph::gsize(ig), error = function(e) NA_integer_)
      n_mod <- tryCatch(length(unique(igraph::V(ig)$Modularity)), error = function(e) NA_integer_)
      dens <- tryCatch(round(igraph::edge_density(ig), 3), error = function(e) NA_real_)
      bslib::layout_columns(
        col_widths = c(3, 3, 3, 3),
        ggnv_value_box("Nodes", n_nodes, icon = "diagram-3"),
        ggnv_value_box("Edges", n_edges, icon = "share"),
        ggnv_value_box("Modules", n_mod, icon = "grid-3x3-gap"),
        ggnv_value_box("Density", dens, icon = "bounding-box")
      )
    })

    output$summary <- shiny::renderPrint({
      item <- selected_graph()
      shiny::req(item)
      print(item$summary)
    })

    output$nodes <- DT::renderDT({
      item <- selected_graph()
      shiny::req(item)
      graph_nodes_table(item$data)
    }, rownames = FALSE)

    output$edges <- DT::renderDT({
      item <- selected_graph()
      shiny::req(item)
      graph_edges_table(item$data)
    }, rownames = FALSE)

    output$module_stats <- DT::renderDT({
      subgraph_stat_table(module_result(), "stat_module")
    }, rownames = FALSE)

    output$sample_stats <- DT::renderDT({
      subgraph_stat_table(sample_result(), "stat_sample")
    }, rownames = FALSE)

    output$status <- shiny::renderText(status())
  })
}
