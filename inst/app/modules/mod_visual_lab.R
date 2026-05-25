visual_lab_layout_choices <- function() {
  list(
    "General" = c(
      "nicely", "nicely1", "fr", "fr1", "fr2", "kk", "gephi", "stress", "lgl", "randomly"
    ),
    "Geometric" = c(
      "circle", "circle_outline", "grid", "diamond", "diamond_outline", "rectangle",
      "rectangle_outline", "square", "square2", "square_outline", "star",
      "star_concentric", "petal", "petal2", "heart_centered", "rightiso_layers"
    ),
    "Circular modules" = c(
      "circular_modules_gephi_layout",
      "circular_modules_equal_gephi_layout",
      "circular_modules_petal_layout",
      "circular_modules_equal_petal_layout",
      "circular_modules_petal2_layout",
      "circular_modules_equal_petal2_layout",
      "circular_modules_square_layout",
      "circular_modules_equal_square_layout",
      "circular_modules_square2_layout",
      "circular_modules_equal_square2_layout",
      "circular_modules_star_layout",
      "circular_modules_equal_star_layout",
      "circular_modules_star_concentric_layout",
      "circular_modules_equal_star_concentric_layout",
      "circular_modules_diamond_layout",
      "circular_modules_equal_diamond_layout",
      "circular_modules_heart_centered_layout",
      "circular_modules_equal_heart_centered_layout",
      "consensus_module_gephi",
      "consensus_module_equal_gephi"
    ),
    "Multipartite" = c(
      "bipartite_layout",
      "bipartite_gephi_layout",
      "tripartite_gephi_layout",
      "tripartite_equal_gephi_layout",
      "quadripartite_gephi_layout",
      "quadripartite_equal_gephi_layout",
      "pentapartite_gephi_layout",
      "pentapartite_equal_gephi_layout"
    ),
    "Special" = c("WGCNA", "dendrogram", "multirings")
  )
}

visual_layout_smoke_cases <- function(choices = visual_lab_layout_choices()) {
  layouts <- unlist(choices, use.names = FALSE)
  family <- rep(names(choices), lengths(choices))
  module <- ifelse(
    family %in% c("Circular modules", "Multipartite", "Special"),
    "order",
    "adjacent"
  )
  graph_name <- rep("gallery_matrix_graph", length(layouts))
  graph_name[grepl("tripartite", layouts)] <- "gallery_tripartite_graph"
  graph_name[grepl("quadripartite", layouts)] <- "gallery_quadripartite_graph"
  graph_name[grepl("pentapartite", layouts)] <- "gallery_pentapartite_graph"
  graph_name[layouts == "dendrogram"] <- "gallery_directed_tree_graph"

  data.frame(
    family = family,
    layout = layouts,
    module = module,
    graph_name = graph_name,
    stringsAsFactors = FALSE
  )
}

visual_lab_params <- function(
  layout,
  layout_module = "adjacent",
  show_labels,
  label_layout,
  label_wrap_width,
  label_outer_pad = 0.4,
  bandwidth_scale,
  point_size_min = 1,
  point_size_max = 10,
  add_group_outer = FALSE,
  drop_others = FALSE,
  seed = 1115
) {
  normalize_positive_number <- function(value, default, min = NULL, max = NULL) {
    value <- suppressWarnings(as.numeric(value))
    if (length(value) != 1L || is.na(value) || !is.finite(value) || value <= 0) {
      value <- default
    }
    if (!is.null(min)) {
      value <- max(value, min)
    }
    if (!is.null(max)) {
      value <- min(value, max)
    }
    value
  }

  list(
    layout = if (is.null(layout) || !nzchar(layout)) "nicely" else layout,
    layout.module = if (is.null(layout_module) || !nzchar(layout_module)) "adjacent" else layout_module,
    label = isTRUE(show_labels),
    label_layout = if (is.null(label_layout) || !nzchar(label_layout)) "two_column" else label_layout,
    label_wrap_width = normalize_positive_number(label_wrap_width, default = 18, min = 4, max = 80),
    label_outer_pad = normalize_positive_number(label_outer_pad, default = 0.4, min = 0, max = 5),
    bandwidth_scale = normalize_positive_number(bandwidth_scale, default = 1, min = 0.1, max = 5),
    pointsize = c(
      normalize_positive_number(point_size_min, default = 1, min = 0.1, max = 50),
      normalize_positive_number(point_size_max, default = 10, min = 0.1, max = 50)
    ),
    add_group_outer = isTRUE(add_group_outer),
    dropOthers = isTRUE(drop_others),
    seed = as.integer(normalize_positive_number(seed, default = 1115, min = 1))
  )
}

visual_lab_params_json <- function(params) {
  jsonlite::toJSON(params, auto_unbox = TRUE, pretty = TRUE)
}

mod_visual_lab_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      shiny::selectInput(ns("graph_id"), "Graph object", choices = character()),
      shiny::selectInput(ns("layout"), "Layout", choices = visual_lab_layout_choices()),
      shiny::selectInput(ns("layout_module"), "Module placement", choices = c("adjacent", "random", "order")),
      shiny::selectInput(
        ns("label_layout"),
        "Label layout",
        choices = c("two_column", "two_column_follow", "label_circle")
      ),
      shiny::checkboxInput(ns("show_labels"), "Show labels", value = FALSE),
      shiny::numericInput(ns("label_wrap_width"), "Label wrap width", value = 18, min = 4, max = 80),
      shiny::numericInput(ns("label_outer_pad"), "Label outer padding", value = 0.4, min = 0, max = 5, step = 0.05),
      shiny::numericInput(ns("bandwidth_scale"), "Bandwidth scale", value = 1, min = 0.1, max = 5, step = 0.1),
      shiny::numericInput(ns("point_size_min"), "Point size min", value = 1, min = 0.1, max = 50, step = 0.5),
      shiny::numericInput(ns("point_size_max"), "Point size max", value = 10, min = 0.1, max = 50, step = 0.5),
      shiny::checkboxInput(ns("add_group_outer"), "Add group outer", value = FALSE),
      shiny::checkboxInput(ns("drop_others"), "Drop Others", value = FALSE),
      shiny::numericInput(ns("seed"), "Seed", value = 1115, min = 1, step = 1),
      shiny::actionButton(ns("draw"), "Draw")
    ),
    bslib::card(
      bslib::card_header("Preview"),
      shiny::plotOutput(ns("plot"), height = 650),
      shiny::verbatimTextOutput(ns("status")),
      shiny::verbatimTextOutput(ns("params"))
    )
  )
}

mod_visual_lab_server <- function(id, registry) {
  shiny::moduleServer(id, function(input, output, session) {
    unique_output_name <- function(base) {
      suffix <- paste0(format(Sys.time(), "%Y%m%d_%H%M%S"), "_", sprintf("%04d", sample.int(9999, 1)))
      paste0(base, "_", suffix)
    }

    plot_obj <- shiny::reactiveVal(NULL)
    last_params <- shiny::reactiveVal(visual_lab_params("nicely", "adjacent", FALSE, "two_column", 18, 0.4, 1, 1, 10, FALSE, FALSE, 1115))
    status <- shiny::reactiveVal("No plot drawn yet.")

    shiny::observe({
      shiny::updateSelectInput(session, "graph_id", choices = registry_choices(registry, type = "graph"))
    })

    shiny::observeEvent(input$draw, {
      shiny::req(input$graph_id)
      graph_item <- registry_get(registry, input$graph_id)
      shiny::req(graph_item)

      params <- visual_lab_params(
        layout = input$layout,
        layout_module = input$layout_module,
        show_labels = input$show_labels,
        label_layout = input$label_layout,
        label_wrap_width = input$label_wrap_width,
        label_outer_pad = input$label_outer_pad,
        bandwidth_scale = input$bandwidth_scale,
        point_size_min = input$point_size_min,
        point_size_max = input$point_size_max,
        add_group_outer = input$add_group_outer,
        drop_others = input$drop_others,
        seed = input$seed
      )

      status(task_feedback_message("plot draw", "running"))
      result <- with_task_feedback(
        session,
        "plot draw",
        session$ns("draw"),
        safe_plot_ggnetview(graph_item$data, params = params)
      )
      if (!result$ok) {
        detail <- if (!is.null(result$trace)) paste(result$message, result$trace, sep = "\n") else result$message
        status(detail)
        shiny::showNotification(result$message, type = "error")
        return()
      }

      plot_name <- unique_output_name(paste0(graph_item$name, "_plot"))
      plot_obj(result$value)
      last_params(params)
      registry_add(
        registry,
        name = plot_name,
        type = "plot",
        data = result$value,
        source = graph_item$id,
        params = params
      )
      status(paste("Registered plot:", plot_name))
    })

    output$plot <- shiny::renderPlot({
      shiny::req(plot_obj())
      plot_obj()
    })

    output$status <- shiny::renderText(status())

    output$params <- shiny::renderText({
      visual_lab_params_json(last_params())
    })
  })
}
