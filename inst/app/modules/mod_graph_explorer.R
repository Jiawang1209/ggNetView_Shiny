mod_graph_explorer_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_columns(
    bslib::card(
      bslib::card_header("Select Graph"),
      shiny::selectInput(ns("graph_id"), "Graph object", choices = character())
    ),
    bslib::card(
      bslib::card_header("Summary"),
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
    col_widths = c(4, 8, 6, 6)
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
    shiny::observe({
      shiny::updateSelectInput(session, "graph_id", choices = registry_choices(registry, type = "graph"))
    })

    selected_graph <- shiny::reactive({
      shiny::req(input$graph_id)
      registry_get(registry, input$graph_id)
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
  })
}
