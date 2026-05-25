builder_choices_for_type <- function(type) {
  if (is.null(type) || length(type) != 1L || is.na(type) || !nzchar(type)) {
    return(graph_builder_modes())
  }

  switch(type,
    matrix = c("Matrix" = "matrix", "Matrix + RMT" = "matrix_rmt", "Double matrix" = "double_matrix", "Multi matrix" = "multi_matrix"),
    adjacency = c("Adjacency matrix" = "adjacency", "Consensus" = "consensus"),
    edge_table = c("Edge table" = "edge_table", "Node + edge table" = "node_edge"),
    wgcna_tom = c("WGCNA/TOM" = "wgcna_tom"),
    graph = c("Igraph object" = "igraph", "Consensus" = "consensus"),
    graph_builder_modes()
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
  module_method = "Fast_greedy",
  transform_method = "none"
) {
  if (identical(builder, "node_edge")) {
    return(list(
      module.method = module_method
    ))
  }

  if (identical(builder, "igraph")) {
    return(list(
      use_existing_modules = TRUE,
      module.method = module_method
    ))
  }

  if (!builder %in% c("matrix", "matrix_rmt")) {
    return(list())
  }

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

graph_builder_registry_params <- function(builder, params = list(), source_ids = character(), module_id = "", node_id = "") {
  replay_params <- c(
    list(
      builder = builder,
      source_ids = unique(as.character(source_ids))
    ),
    params %||% list()
  )
  if (!is.null(module_id) && length(module_id) == 1L && nzchar(module_id)) {
    replay_params$module_id <- module_id
  }
  if (!is.null(node_id) && length(node_id) == 1L && nzchar(node_id)) {
    replay_params$node_id <- node_id
  }
  replay_params
}

mod_graph_builder_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_columns(
    bslib::card(
      bslib::card_header("Build Graph"),
      shiny::selectInput(ns("source_id"), "Source object", choices = character()),
      shiny::selectInput(ns("source_id_b"), "Second matrix", choices = character()),
      shiny::selectizeInput(ns("multi_source_ids"), "Multiple matrices", choices = character(), multiple = TRUE),
      shiny::selectizeInput(ns("consensus_source_ids"), "Consensus inputs", choices = character(), multiple = TRUE),
      shiny::selectInput(ns("node_id"), "Node table", choices = c("None" = "")),
      shiny::selectInput(ns("module_id"), "Module table", choices = c("None" = "")),
      shiny::selectInput(
        ns("builder"),
        "Builder",
        choices = graph_builder_modes()
      ),
      shiny::selectInput(ns("method"), "Association method", choices = c("cor", "Hmisc", "WGCNA", "SPARCC", "SpiecEasi")),
      shiny::selectInput(ns("transform_method"), "Transform", choices = c("none", "scale", "center", "log2", "log10", "ln", "rrarefy", "rrarefy_relative")),
      shiny::selectInput(ns("cor_method"), "Correlation", choices = c("pearson", "spearman", "kendall")),
      shiny::selectInput(ns("proc"), "P-value adjustment", choices = c("none", "BH", "holm", "bonferroni")),
      shiny::numericInput(ns("r_threshold"), "r threshold", value = 0.1, min = 0, max = 1, step = 0.01),
      shiny::numericInput(ns("p_threshold"), "p threshold", value = 1, min = 0, max = 1, step = 0.01),
      shiny::selectInput(
        ns("module_method"),
        "Module method",
        choices = c("Fast_greedy", "Walktrap", "Edge_betweenness", "Spinglass")
      ),
      shiny::actionButton(ns("run_rmt"), "Run RMT"),
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
      choices <- registry_choices_by_type(registry, c("matrix", "adjacency", "edge_table", "wgcna_tom", "graph"))
      shiny::updateSelectInput(session, "source_id", choices = choices)
    })

    shiny::observe({
      matrix_choices <- registry_choices_by_type(registry, c("matrix"))
      consensus_choices <- registry_choices_by_type(registry, c("graph", "adjacency"))
      node_choices <- c("None" = "", registry_choices(registry, type = "node_table"))
      module_choices <- c("None" = "", registry_choices(registry, type = "module_table"))
      shiny::updateSelectInput(session, "source_id_b", choices = matrix_choices)
      shiny::updateSelectizeInput(session, "multi_source_ids", choices = matrix_choices, server = TRUE)
      shiny::updateSelectizeInput(session, "consensus_source_ids", choices = consensus_choices, server = TRUE)
      shiny::updateSelectInput(session, "node_id", choices = node_choices)
      shiny::updateSelectInput(session, "module_id", choices = module_choices)
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

    shiny::observeEvent(input$run_rmt, {
      shiny::req(input$source_id)
      source <- registry_get(registry, input$source_id)
      shiny::req(source)

      if (!identical(source$type, "matrix")) {
        message <- "RMT requires a matrix source object."
        status(message)
        shiny::showNotification(message, type = "error")
        return()
      }

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
        module_method = input$module_method,
        transform_method = input$transform_method
      )

      source_ids <- input$source_id
      inputs <- list()
      inputs <- switch(input$builder,
        matrix = list(matrix = source$data),
        matrix_rmt = list(matrix = source$data),
        edge_table = list(edge_table = source$data),
        node_edge = {
          shiny::req(input$node_id)
          node_item <- registry_get(registry, input$node_id)
          shiny::req(node_item)
          source_ids <- c(input$source_id, input$node_id)
          list(edge_table = source$data, node_table = node_item$data)
        },
        igraph = list(graph = source$data),
        adjacency = list(adjacency = source$data),
        double_matrix = {
          shiny::req(input$source_id_b)
          second <- registry_get(registry, input$source_id_b)
          shiny::req(second)
          source_ids <- c(input$source_id, input$source_id_b)
          list(matrix_a = source$data, matrix_b = second$data)
        },
        multi_matrix = {
          ids <- unique(c(input$source_id, input$multi_source_ids))
          ids <- ids[nzchar(ids)]
          items <- lapply(ids, function(id) registry_get(registry, id))
          names(items) <- vapply(items, function(x) x$name, character(1))
          source_ids <- ids
          list(matrices = lapply(items, `[[`, "data"))
        },
        wgcna_tom = list(tom = source$data),
        consensus = {
          ids <- unique(c(input$source_id, input$consensus_source_ids))
          ids <- ids[nzchar(ids)]
          items <- lapply(ids, function(id) registry_get(registry, id))
          values <- lapply(items, function(item) {
            if (inherits(item$data, "igraph")) {
              return(as.matrix(igraph::as_adjacency_matrix(item$data, attr = "weight", sparse = FALSE)))
            }
            item$data
          })
          names(values) <- vapply(items, function(x) x$name, character(1))
          source_ids <- ids
          list(graphs_or_adjacency = values)
        },
        list(matrix = source$data)
      )

      if (!is.null(input$module_id) && nzchar(input$module_id) && !identical(input$builder, "node_edge")) {
        module_item <- registry_get(registry, input$module_id)
        if (!is.null(module_item)) {
          inputs$module_table <- module_item$data
        }
      }

      status(task_feedback_message("graph build", "running"))
      result <- with_task_feedback(
        session,
        "graph build",
        session$ns("build"),
        safe_graph_builder(input$builder, inputs = inputs, params = params)
      )
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
        source = paste(source_ids, collapse = ","),
        params = graph_builder_registry_params(
          input$builder,
          params,
          source_ids,
          if (identical(input$builder, "node_edge")) "" else input$module_id,
          if (identical(input$builder, "node_edge")) input$node_id else ""
        )
      )
      status(paste("Built graph:", item$name))
      shiny::showNotification(paste("Built graph:", item$name), type = "message")
    })

    output$status <- shiny::renderText(status())
  })
}
