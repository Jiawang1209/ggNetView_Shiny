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
  seed = 1115,
  node_add = 7,
  ring_n = NULL,
  r = 1,
  center = TRUE,
  shrink = 1,
  inner_shrink = 1,
  k_nn = 8,
  push_others_delta = 0,
  jitter = FALSE,
  jitter_sd = 0.1,
  plot_line = TRUE,
  curve = FALSE,
  curvature = 0.25,
  linealpha = 0.25,
  linecolor = "grey70",
  pointlabel = "",
  pointlabelsize = 5
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
  normalize_nonnegative_number <- function(value, default, max = NULL) {
    value <- suppressWarnings(as.numeric(value))
    if (length(value) != 1L || is.na(value) || !is.finite(value) || value < 0) {
      value <- default
    }
    if (!is.null(max)) {
      value <- min(value, max)
    }
    value
  }
  normalize_positive_integer <- function(value, default, min = 1L, max = NULL) {
    as.integer(round(normalize_positive_number(value, default = default, min = min, max = max)))
  }
  normalize_optional_positive_integer <- function(value, default = NULL, min = 1L, max = NULL) {
    if (is.null(value) || length(value) == 0L || identical(value, "")) {
      return(default)
    }
    value <- suppressWarnings(as.numeric(value))
    if (length(value) != 1L || is.na(value) || !is.finite(value) || value <= 0) {
      return(default)
    }
    as.integer(round(normalize_positive_number(value, default = value, min = min, max = max)))
  }
  normalize_alpha <- function(value, default) {
    normalize_positive_number(value, default = default, min = 0, max = 1)
  }
  normalize_color <- function(value, default) {
    if (is.null(value) || length(value) != 1L || !nzchar(trimws(as.character(value)))) {
      return(default)
    }
    as.character(value)
  }
  normalize_pointlabel <- function(value) {
    if (is.null(value) || length(value) != 1L || !nzchar(trimws(as.character(value)))) {
      return(NULL)
    }
    as.character(value)
  }

  list(
    layout = if (is.null(layout) || !nzchar(layout)) "nicely" else layout,
    layout.module = if (is.null(layout_module) || !nzchar(layout_module)) "adjacent" else layout_module,
    node_add = normalize_positive_integer(node_add, default = 7L, min = 1L, max = 100L),
    ring_n = normalize_optional_positive_integer(ring_n, default = NULL, min = 1L, max = 100L),
    r = normalize_positive_number(r, default = 1, min = 0.01, max = 50),
    center = isTRUE(center),
    shrink = normalize_positive_number(shrink, default = 1, min = 0.01, max = 10),
    inner_shrink = normalize_positive_number(inner_shrink, default = 1, min = 0.01, max = 10),
    k_nn = normalize_positive_integer(k_nn, default = 8L, min = 1L, max = 100L),
    push_others_delta = normalize_nonnegative_number(push_others_delta, default = 0, max = 50),
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
    jitter = isTRUE(jitter),
    jitter_sd = normalize_positive_number(jitter_sd, default = 0.1, min = 0.001, max = 5),
    plot_line = isTRUE(plot_line),
    curve = isTRUE(curve),
    curvature = normalize_positive_number(curvature, default = 0.25, min = 0, max = 1),
    linealpha = normalize_alpha(linealpha, default = 0.25),
    linecolor = normalize_color(linecolor, default = "grey70"),
    pointlabel = normalize_pointlabel(pointlabel),
    pointlabelsize = normalize_positive_number(pointlabelsize, default = 5, min = 0.1, max = 50),
    seed = as.integer(normalize_positive_number(seed, default = 1115, min = 1))
  )
}

visual_lab_params_json <- function(params) {
  jsonlite::toJSON(params, auto_unbox = TRUE, pretty = TRUE)
}

visual_lab_plot_dimension <- function(value, default, min = 1, max = 30) {
  value <- suppressWarnings(as.numeric(value))
  if (length(value) != 1L || is.na(value) || !is.finite(value) || value <= 0) {
    value <- default
  }
  value <- max(value, min)
  min(value, max)
}

visual_lab_tip <- function(input_tag, text) {
  bslib::tooltip(input_tag, text, placement = "right")
}

mod_visual_lab_ui <- function(id) {
  ns <- shiny::NS(id)

  # JS conditions for layout-aware parameter visibility. Module/ring controls
  # only apply to circular-module, consensus-module, multipartite, and
  # multirings layouts; nearest-neighbour placement only applies to the
  # "adjacent" module-placement method.
  module_layout_js <- sprintf(
    paste0(
      "(function(v){return v && (",
      "v.indexOf('circular_modules')>=0 || v.indexOf('consensus_module')>=0 || ",
      "v.indexOf('partite')>=0 || v.indexOf('multirings')>=0);})(input['%s'])"
    ),
    ns("layout")
  )
  adjacent_module_js <- sprintf("input['%s'] == 'adjacent'", ns("layout_module"))

  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      open = TRUE,
      width = 320,
      bslib::accordion(
        open = "Basics",
        bslib::accordion_panel(
          "Basics",
          shiny::selectInput(ns("graph_id"), "Graph object", choices = character()),
          shiny::selectInput(ns("layout"), "Layout", choices = visual_lab_layout_choices()),
          shiny::selectInput(ns("layout_module"), "Module placement", choices = c("adjacent", "random", "order")),
          shiny::checkboxInput(ns("show_labels"), "Show labels", value = FALSE),
          shiny::numericInput(ns("point_size_min"), "Point size min", value = 1, min = 0.1, max = 50, step = 0.5),
          shiny::numericInput(ns("point_size_max"), "Point size max", value = 10, min = 0.1, max = 50, step = 0.5),
          shiny::numericInput(ns("seed"), "Seed", value = 1115, min = 1, step = 1)
        ),
        bslib::accordion_panel(
          "Appearance",
          shiny::selectInput(
            ns("label_layout"),
            "Label layout",
            choices = c("two_column", "two_column_follow", "label_circle")
          ),
          shiny::numericInput(ns("label_wrap_width"), "Label wrap width", value = 18, min = 4, max = 80),
          visual_lab_tip(
            shiny::numericInput(ns("label_outer_pad"), "Label outer padding", value = 0.4, min = 0, max = 5, step = 0.05),
            "Extra radial gap between the outermost ring of points and their labels."
          ),
          visual_lab_tip(
            shiny::numericInput(ns("bandwidth_scale"), "Bandwidth scale", value = 1, min = 0.1, max = 5, step = 0.1),
            "Scales the smoothing bandwidth used when drawing group hulls/outlines. Higher = looser, smoother outlines."
          ),
          shiny::checkboxInput(ns("add_group_outer"), "Add group outer", value = FALSE),
          shiny::checkboxInput(ns("drop_others"), "Drop Others", value = FALSE),
          shiny::checkboxInput(ns("plot_line"), "Plot edges", value = TRUE),
          shiny::checkboxInput(ns("curve"), "Curve edges", value = FALSE),
          shiny::numericInput(ns("curvature"), "Edge curvature", value = 0.25, min = 0, max = 1, step = 0.05),
          shiny::numericInput(ns("linealpha"), "Edge alpha", value = 0.25, min = 0, max = 1, step = 0.05),
          shiny::textInput(ns("linecolor"), "Edge color", value = "grey70"),
          shiny::textInput(ns("pointlabel"), "Point labels", value = ""),
          shiny::numericInput(ns("pointlabelsize"), "Point label size", value = 5, min = 0.1, max = 50, step = 0.5),
          shiny::numericInput(ns("plot_width"), "Plot width", value = 9, min = 1, max = 30, step = 0.5),
          shiny::numericInput(ns("plot_height"), "Plot height", value = 6, min = 1, max = 30, step = 0.5)
        ),
        bslib::accordion_panel(
          "Advanced layout",
          shiny::checkboxInput(ns("jitter"), "Jitter points", value = FALSE),
          visual_lab_tip(
            shiny::numericInput(ns("jitter_sd"), "Jitter SD", value = 0.1, min = 0.001, max = 5, step = 0.01),
            "Standard deviation of random point jitter; only applied when 'Jitter points' is on."
          ),
          shiny::conditionalPanel(
            condition = adjacent_module_js,
            visual_lab_tip(
              shiny::numericInput(ns("k_nn"), "Nearest neighbors", value = 8, min = 1, max = 100, step = 1),
              "Number of nearest neighbours used to place modules next to each other under the 'adjacent' placement method."
            )
          ),
          shiny::conditionalPanel(
            condition = module_layout_js,
            visual_lab_tip(
              shiny::numericInput(ns("node_add"), "Node add", value = 7, min = 1, max = 100, step = 1),
              "Extra spacing budget added per module when arranging module rings."
            ),
            shiny::numericInput(ns("ring_n"), "Ring count", value = NA, min = 1, max = 100, step = 1),
            shiny::numericInput(ns("layout_r"), "Layout radius", value = 1, min = 0.01, max = 50, step = 0.1),
            shiny::checkboxInput(ns("center"), "Center node", value = TRUE),
            visual_lab_tip(
              shiny::numericInput(ns("shrink"), "Shrink", value = 1, min = 0.01, max = 10, step = 0.05),
              "Scales the overall module ring outward/inward. <1 pulls modules toward the centre."
            ),
            visual_lab_tip(
              shiny::numericInput(ns("inner_shrink"), "Inner shrink", value = 1, min = 0.01, max = 10, step = 0.05),
              "Scales spacing of nodes WITHIN each module independently of the module ring."
            ),
            visual_lab_tip(
              shiny::numericInput(ns("push_others_delta"), "Others offset", value = 0, min = 0, max = 50, step = 0.05),
              "Pushes the 'Others' / unassigned group outward by this offset to separate it from real modules."
            )
          )
        )
      ),
      shiny::actionButton(ns("draw"), "Draw", class = "w-100")
    ),
    bslib::card(
      class = "visual-lab-preview-card",
      bslib::card_header("Preview"),
      shiny::div(
        class = "visual-lab-plot-frame",
        shiny::plotOutput(ns("plot"), height = "620px")
      ),
      shiny::div(
        class = "visual-lab-status",
        shiny::verbatimTextOutput(ns("status"))
      ),
      shiny::div(
        class = "ggnv-export-section",
        shiny::tags$h5("Save Plot"),
        shiny::div(
          class = "ggnv-export-buttons",
          shiny::downloadButton(ns("download_png"), "Download PNG"),
          shiny::downloadButton(ns("download_pdf"), "Download PDF")
        )
      ),
      bslib::accordion(
        open = FALSE,
        bslib::accordion_panel(
          "Plot parameters",
          shiny::verbatimTextOutput(ns("params"))
        )
      )
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
    plot_name <- shiny::reactiveVal("visual_lab_plot")
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
        seed = input$seed,
        node_add = input$node_add,
        ring_n = input$ring_n,
        r = input$layout_r,
        center = input$center,
        shrink = input$shrink,
        inner_shrink = input$inner_shrink,
        k_nn = input$k_nn,
        push_others_delta = input$push_others_delta,
        jitter = input$jitter,
        jitter_sd = input$jitter_sd,
        plot_line = input$plot_line,
        curve = input$curve,
        curvature = input$curvature,
        linealpha = input$linealpha,
        linecolor = input$linecolor,
        pointlabel = input$pointlabel,
        pointlabelsize = input$pointlabelsize
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

      plot_output_name <- unique_output_name(paste0(graph_item$name, "_plot"))
      plot_obj(result$value)
      plot_name(plot_output_name)
      last_params(params)
      registry_add(
        registry,
        name = plot_output_name,
        type = "plot",
        data = result$value,
        source = graph_item$id,
        params = params
      )
      status(paste("Registered plot:", plot_output_name))
    })

    output$plot <- shiny::renderPlot(
      {
        shiny::req(plot_obj())
        plot_obj()
      },
      width = function() {
        width <- visual_lab_plot_dimension(input$plot_width, default = 9)
        as.integer(width * 120)
      },
      height = function() {
        height <- visual_lab_plot_dimension(input$plot_height, default = 6)
        as.integer(height * 120)
      },
      res = 96
    )

    output$status <- shiny::renderText(status())

    output$params <- shiny::renderText({
      visual_lab_params_json(last_params())
    })

    output$download_png <- shiny::downloadHandler(
      filename = function() paste0(plot_name(), ".png"),
      content = function(file) {
        shiny::req(plot_obj())
        write_plot_png(
          plot_obj(),
          file,
          width = visual_lab_plot_dimension(input$plot_width, default = 9),
          height = visual_lab_plot_dimension(input$plot_height, default = 6)
        )
      }
    )

    output$download_pdf <- shiny::downloadHandler(
      filename = function() paste0(plot_name(), ".pdf"),
      content = function(file) {
        shiny::req(plot_obj())
        write_plot_pdf(
          plot_obj(),
          file,
          width = visual_lab_plot_dimension(input$plot_width, default = 9),
          height = visual_lab_plot_dimension(input$plot_height, default = 6)
        )
      }
    )
  })
}
