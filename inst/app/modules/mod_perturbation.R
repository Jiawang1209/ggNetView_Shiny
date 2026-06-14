perturbation_curve_metrics <- function() {
  c(
    "LCC fraction" = "LCC_fraction",
    "Number of components" = "N_components",
    "Natural connectivity" = "Natural_connectivity"
  )
}

mod_perturbation_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      title = "Perturbation",
      width = 430,
      bslib::accordion(
        bslib::accordion_panel(
          "Structural attack",
          shiny::selectInput(ns("attack_graph_id"), "Graph object", choices = character()),
          shiny::selectInput(
            ns("strategy"), "Attack strategy",
            choices = c(
              "Random removal" = "random",
              "Targeted (by centrality)" = "targeted",
              "Module knockout" = "module",
              "Manual node set" = "manual"
            )
          ),
          shiny::conditionalPanel(
            condition = sprintf("input['%s'] == 'targeted'", ns("strategy")),
            shiny::selectInput(
              ns("centrality"), "Centrality",
              choices = c("degree", "strength", "betweenness", "closeness", "eigenvector", "ivi")
            )
          ),
          shiny::conditionalPanel(
            condition = sprintf("input['%s'] == 'module'", ns("strategy")),
            shiny::selectInput(ns("target_modules"), "Module(s) to knock out", choices = character(), multiple = TRUE)
          ),
          shiny::conditionalPanel(
            condition = sprintf("input['%s'] == 'manual'", ns("strategy")),
            shiny::selectInput(ns("target_nodes"), "Node(s) to remove", choices = character(), multiple = TRUE)
          ),
          shiny::div(
            class = "topology-control-grid",
            shiny::numericInput(ns("fraction_step"), "Fraction step", value = 0.05, min = 0.01, max = 0.5, step = 0.01),
            shiny::numericInput(ns("bootstrap"), "Bootstrap (random)", value = 100, min = 1, step = 1),
            shiny::numericInput(ns("seed"), "Seed", value = 123, min = 0, step = 1)
          ),
          shiny::actionButton(ns("run_attack"), "Run attack", class = "w-100")
        ),
        bslib::accordion_panel(
          "Node influence",
          shiny::selectInput(ns("influence_graph_id"), "Graph object", choices = character()),
          shiny::selectInput(ns("influence_source"), "Source node(s)", choices = character(), multiple = TRUE),
          shiny::div(
            class = "topology-control-grid",
            shiny::numericInput(ns("influence_alpha"), "Alpha (0-1)", value = 0.5, min = 0.01, max = 0.99, step = 0.05),
            shiny::numericInput(ns("influence_delta"), "Delta", value = 1, step = 0.5)
          ),
          shiny::checkboxInput(ns("influence_signed"), "Use signed edge weights", value = TRUE),
          shiny::checkboxInput(ns("influence_drop_source"), "Zero out source nodes", value = TRUE),
          shiny::actionButton(ns("run_influence"), "Compute influence", class = "w-100")
        ),
        bslib::accordion_panel(
          "Press perturbation",
          shiny::radioButtons(
            ns("press_input_type"), "Interaction matrix source",
            choices = c("Graph object" = "graph", "Correlation matrix" = "matrix"),
            selected = "graph"
          ),
          shiny::conditionalPanel(
            condition = sprintf("input['%s'] == 'graph'", ns("press_input_type")),
            shiny::selectInput(ns("press_graph_id"), "Graph object", choices = character())
          ),
          shiny::conditionalPanel(
            condition = sprintf("input['%s'] == 'matrix'", ns("press_input_type")),
            shiny::selectInput(ns("press_matrix_id"), "Correlation matrix", choices = character())
          ),
          shiny::numericInput(ns("press_self_regulation"), "Self-regulation (blank = auto)", value = NA, step = 0.1),
          shiny::selectInput(ns("press_source"), "Pressed node(s) (optional)", choices = character(), multiple = TRUE),
          shiny::actionButton(ns("run_press"), "Run press", class = "w-100")
        ),
        open = "Structural attack"
      )
    ),
    bslib::layout_columns(
      bslib::card(
        bslib::card_header("Attack curve"),
        shiny::selectInput(ns("curve_metric"), "Curve metric", choices = perturbation_curve_metrics()),
        shinycssloaders::withSpinner(shiny::plotOutput(ns("attack_plot"), height = "360px"), color = "#AE017E", type = 6),
        shiny::downloadButton(ns("download_attack_plot"), "Download curve PNG"),
        shiny::verbatimTextOutput(ns("status"))
      ),
      bslib::card(
        bslib::card_header("Robustness (Schneider R)"),
        shiny::uiOutput(ns("attack_metrics")),
        DT::DTOutput(ns("robustness_index")),
        shiny::downloadButton(ns("download_curve"), "Download curve CSV")
      ),
      bslib::card(
        bslib::card_header("Node influence"),
        DT::DTOutput(ns("influence")),
        shiny::downloadButton(ns("download_influence"), "Download influence CSV")
      ),
      bslib::card(
        bslib::card_header("Press: net response"),
        shiny::verbatimTextOutput(ns("press_meta")),
        DT::DTOutput(ns("press_response")),
        shiny::downloadButton(ns("download_press_response"), "Download response CSV"),
        shiny::downloadButton(ns("download_press_matrix"), "Download net-effect CSV")
      ),
      col_widths = c(7, 5, 6, 6)
    )
  )
}

mod_perturbation_server <- function(id, registry) {
  shiny::moduleServer(id, function(input, output, session) {
    unique_output_name <- function(base) {
      suffix <- paste0(format(Sys.time(), "%Y%m%d_%H%M%S"), "_", sprintf("%04d", sample.int(9999, 1)))
      paste0(base, "_", suffix)
    }

    curve_table <- shiny::reactiveVal(data.frame())
    robustness_table <- shiny::reactiveVal(data.frame())
    influence_table <- shiny::reactiveVal(data.frame())
    press_response_table <- shiny::reactiveVal(data.frame())
    press_matrix_table <- shiny::reactiveVal(data.frame())
    press_meta_text <- shiny::reactiveVal("No press perturbation run yet.")
    status <- shiny::reactiveVal("No perturbation run yet.")

    # Keep graph/matrix selectors and node/module choices in sync with registry.
    shiny::observe({
      graph_choices <- registry_choices(registry, type = "graph")
      matrix_choices <- registry_choices(registry, type = "matrix")

      keep_selection <- function(inputId, choices) {
        current <- shiny::isolate(input[[inputId]])
        selected <- if (!is.null(current) && current %in% unname(choices)) {
          current
        } else if (length(choices)) {
          unname(choices[[1]])
        } else {
          character()
        }
        shiny::updateSelectInput(session, inputId, choices = choices, selected = selected)
      }

      keep_selection("attack_graph_id", graph_choices)
      keep_selection("influence_graph_id", graph_choices)
      keep_selection("press_graph_id", graph_choices)
      keep_selection("press_matrix_id", matrix_choices)
    })

    # Update module choices for the module-knockout strategy.
    shiny::observe({
      shiny::req(input$attack_graph_id)
      item <- registry_get(registry, input$attack_graph_id)
      shiny::req(item)
      modules <- perturbation_module_values(item$data)
      shiny::updateSelectInput(session, "target_modules", choices = modules)
      shiny::updateSelectInput(session, "target_nodes", choices = perturbation_node_names(item$data))
    })

    shiny::observe({
      shiny::req(input$influence_graph_id)
      item <- registry_get(registry, input$influence_graph_id)
      shiny::req(item)
      shiny::updateSelectInput(session, "influence_source", choices = perturbation_node_names(item$data))
    })

    shiny::observe({
      shiny::req(input$press_graph_id)
      item <- registry_get(registry, input$press_graph_id)
      shiny::req(item)
      shiny::updateSelectInput(session, "press_source", choices = perturbation_node_names(item$data))
    })

    # ---- Structural attack ----
    shiny::observeEvent(input$run_attack, {
      shiny::req(input$attack_graph_id)
      graph_item <- registry_get(registry, input$attack_graph_id)
      shiny::req(graph_item)

      strategy <- input$strategy %||% "random"
      target <- switch(strategy,
        module = input$target_modules,
        manual = input$target_nodes,
        NULL
      )
      params <- list(
        strategy = strategy,
        centrality = input$centrality %||% "degree",
        target = target,
        fraction_step = input$fraction_step %||% 0.05,
        bootstrap = input$bootstrap %||% 100,
        seed = input$seed %||% 123
      )

      status(task_feedback_message("network perturbation", "running"))
      result <- with_task_feedback(
        session,
        "network perturbation",
        session$ns("run_attack"),
        safe_network_perturbation(graph_item$data, params = params)
      )
      if (!result$ok) {
        curve_table(data.frame())
        robustness_table(data.frame())
        detail <- if (!is.null(result$trace)) paste(result$message, result$trace, sep = "\n") else result$message
        status(detail)
        shiny::showNotification(result$message, type = "error")
        return()
      }

      curve_table(result$value$curve)
      robustness_table(result$value$robustness_index)

      base_name <- unique_output_name(paste0(graph_item$name, "_perturbation_", strategy))
      registry_add(
        registry,
        name = base_name,
        type = "result",
        data = result$value$curve,
        source = graph_item$id,
        params = c(list(metric = "network_perturbation_curve"), params)
      )
      registry_add(
        registry,
        name = paste0(base_name, "_robustness"),
        type = "result",
        data = result$value$robustness_index,
        source = graph_item$id,
        params = c(list(metric = "network_perturbation_robustness"), params)
      )
      status(paste("Registered perturbation:", base_name))
    })

    output$attack_plot <- shiny::renderPlot({
      curve <- curve_table()
      shiny::req(is.data.frame(curve), nrow(curve) > 0)
      plot_result <- safe_perturbation_curve_plot(curve, metric = input$curve_metric %||% "LCC_fraction")
      shiny::req(plot_result$ok)
      plot_result$value
    })

    # ---- Node influence ----
    shiny::observeEvent(input$run_influence, {
      shiny::req(input$influence_graph_id)
      graph_item <- registry_get(registry, input$influence_graph_id)
      shiny::req(graph_item)

      params <- list(
        alpha = input$influence_alpha %||% 0.5,
        delta = input$influence_delta %||% 1,
        signed = isTRUE(input$influence_signed),
        drop_source = isTRUE(input$influence_drop_source)
      )
      status(task_feedback_message("node influence", "running"))
      result <- with_task_feedback(
        session,
        "node influence",
        session$ns("run_influence"),
        safe_node_influence(graph_item$data, source = input$influence_source, params = params)
      )
      if (!result$ok) {
        influence_table(data.frame())
        detail <- if (!is.null(result$trace)) paste(result$message, result$trace, sep = "\n") else result$message
        status(detail)
        shiny::showNotification(result$message, type = "error")
        return()
      }

      influence_table(result$value)
      result_name <- unique_output_name(paste0(graph_item$name, "_node_influence"))
      registry_add(
        registry,
        name = result_name,
        type = "result",
        data = result$value,
        source = graph_item$id,
        params = c(list(metric = "node_influence", source = paste(input$influence_source, collapse = ",")), params)
      )
      status(paste("Registered node influence:", result_name))
    })

    # ---- Press perturbation ----
    shiny::observeEvent(input$run_press, {
      use_matrix <- identical(input$press_input_type, "matrix")
      graph <- NULL
      cor_mat <- NULL
      source_label <- NULL
      source_id <- NULL

      if (use_matrix) {
        shiny::req(input$press_matrix_id)
        matrix_item <- registry_get(registry, input$press_matrix_id)
        shiny::req(matrix_item)
        cor_mat <- matrix_item$data
        source_label <- matrix_item$name
        source_id <- matrix_item$id
      } else {
        shiny::req(input$press_graph_id)
        graph_item <- registry_get(registry, input$press_graph_id)
        shiny::req(graph_item)
        graph <- graph_item$data
        source_label <- graph_item$name
        source_id <- graph_item$id
      }

      params <- list(
        self_regulation = input$press_self_regulation,
        source = input$press_source
      )
      status(task_feedback_message("press perturbation", "running"))
      result <- with_task_feedback(
        session,
        "press perturbation",
        session$ns("run_press"),
        safe_press_perturbation(graph = graph, cor_mat = cor_mat, params = params)
      )
      if (!result$ok) {
        press_response_table(data.frame())
        press_matrix_table(data.frame())
        press_meta_text("Press perturbation failed.")
        detail <- if (!is.null(result$trace)) paste(result$message, result$trace, sep = "\n") else result$message
        status(detail)
        shiny::showNotification(result$message, type = "error")
        return()
      }

      press_response_table(result$value$response)
      press_matrix_table(result$value$net_effect)
      meta <- result$value$meta
      press_meta_text(sprintf(
        "Stable: %s | max Re(eigen): %s | self-regulation: %s",
        ifelse(isTRUE(meta$stable), "yes", "no"),
        signif(meta$eigen_real_max, 4),
        signif(meta$self_regulation, 4)
      ))

      result_name <- unique_output_name(paste0(source_label, "_press_net_effect"))
      registry_add(
        registry,
        name = result_name,
        type = "result",
        data = result$value$net_effect,
        source = source_id,
        params = list(metric = "press_net_effect", source = paste(input$press_source, collapse = ","))
      )
      if (nrow(result$value$response) > 0L) {
        registry_add(
          registry,
          name = paste0(result_name, "_response"),
          type = "result",
          data = result$value$response,
          source = source_id,
          params = list(metric = "press_response", source = paste(input$press_source, collapse = ","))
        )
      }
      status(paste("Registered press perturbation:", result_name))
    })

    # ---- Outputs ----
    output$attack_metrics <- shiny::renderUI({
      rb <- robustness_table()
      if (is.null(rb) || !is.data.frame(rb) || !nrow(rb)) return(NULL)
      schneider_col <- intersect(c("Schneider_R", "schneider_R", "R", "robustness"), names(rb))
      schneider_val <- if (length(schneider_col)) round(rb[[schneider_col[[1]]]][[1]], 3) else nrow(rb)
      strategy_col <- intersect(c("strategy", "Strategy", "attack"), names(rb))
      strategy_val <- if (length(strategy_col)) as.character(rb[[strategy_col[[1]]]][[1]]) else "—"
      bslib::layout_columns(
        col_widths = c(6, 6),
        ggnv_value_box("Schneider R", schneider_val, icon = "shield-check"),
        ggnv_value_box("Strategy", strategy_val, icon = "bullseye")
      )
    })

    output$robustness_index <- DT::renderDT(robustness_table(), rownames = FALSE)
    output$influence <- DT::renderDT(influence_table(), rownames = FALSE)
    output$press_response <- DT::renderDT(press_response_table(), rownames = FALSE)
    output$press_meta <- shiny::renderText(press_meta_text())
    output$status <- shiny::renderText(status())

    output$download_curve <- shiny::downloadHandler(
      filename = function() "ggnetview_perturbation_curve.csv",
      content = function(file) write_registry_table(curve_table(), file)
    )
    output$download_influence <- shiny::downloadHandler(
      filename = function() "ggnetview_node_influence.csv",
      content = function(file) write_registry_table(influence_table(), file)
    )
    output$download_press_response <- shiny::downloadHandler(
      filename = function() "ggnetview_press_response.csv",
      content = function(file) write_registry_table(press_response_table(), file)
    )
    output$download_press_matrix <- shiny::downloadHandler(
      filename = function() "ggnetview_press_net_effect.csv",
      content = function(file) write_registry_table(press_matrix_table(), file)
    )
    output$download_attack_plot <- shiny::downloadHandler(
      filename = function() "ggnetview_perturbation_curve.png",
      content = function(file) {
        curve <- curve_table()
        shiny::req(is.data.frame(curve), nrow(curve) > 0)
        plot_result <- safe_perturbation_curve_plot(curve, metric = input$curve_metric %||% "LCC_fraction")
        shiny::req(plot_result$ok)
        write_plot_png(plot_result$value, file, width = 8, height = 6)
      }
    )
  })
}
