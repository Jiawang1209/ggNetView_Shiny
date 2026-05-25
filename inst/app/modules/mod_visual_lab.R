mod_visual_lab_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      shiny::selectInput(ns("graph_id"), "Graph object", choices = character()),
      shiny::selectInput(ns("layout"), "Layout", choices = c("nicely", "fr", "kk", "circle")),
      shiny::selectInput(
        ns("label_layout"),
        "Label layout",
        choices = c("two_column", "two_column_follow", "label_circle")
      ),
      shiny::numericInput(ns("label_wrap_width"), "Label wrap width", value = 18, min = 4, max = 80),
      shiny::numericInput(ns("bandwidth_scale"), "Bandwidth scale", value = 1, min = 0.1, max = 5, step = 0.1),
      shiny::actionButton(ns("draw"), "Draw")
    ),
    bslib::card(
      bslib::card_header("Preview"),
      shiny::plotOutput(ns("plot"), height = 650)
    )
  )
}

mod_visual_lab_server <- function(id, registry) {
  shiny::moduleServer(id, function(input, output, session) {
    plot_obj <- shiny::reactiveVal(NULL)

    shiny::observe({
      shiny::updateSelectInput(session, "graph_id", choices = registry_choices(registry, type = "graph"))
    })

    shiny::observeEvent(input$draw, {
      shiny::req(input$graph_id)
      graph_item <- registry_get(registry, input$graph_id)
      shiny::req(graph_item)

      params <- list(
        layout = input$layout,
        label_layout = input$label_layout,
        label_wrap_width = input$label_wrap_width,
        bandwidth_scale = input$bandwidth_scale
      )

      result <- safe_plot_ggnetview(graph_item$data, params = params)
      if (!result$ok) {
        shiny::showNotification(result$message, type = "error")
        return()
      }

      plot_obj(result$value)
      registry_add(
        registry,
        name = paste0(graph_item$name, "_plot"),
        type = "plot",
        data = result$value,
        source = graph_item$id,
        params = params
      )
    })

    output$plot <- shiny::renderPlot({
      shiny::req(plot_obj())
      plot_obj()
    })
  })
}
