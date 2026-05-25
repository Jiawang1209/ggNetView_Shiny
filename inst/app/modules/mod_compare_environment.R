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
      shiny::textAreaInput(
        ns("comparison_pairs"),
        "Comparison pairs",
        value = "",
        placeholder = "Optional: one pair per line, for example Group_A,Group_B",
        rows = 3
      ),
      shiny::actionButton(ns("run_compare"), "Compare networks"),
      shiny::hr(),
      shiny::selectInput(ns("multi_matrix_id"), "Matrix for groups", choices = character()),
      shiny::selectInput(ns("multi_group_id"), "Sample metadata", choices = c("Generated groups" = "")),
      shiny::selectInput(ns("multi_split"), "Group split", choices = c("halves", "alternating")),
      shiny::actionButton(ns("run_multi_group"), "Build group multi-plot")
    ),
    bslib::card(
      bslib::card_header("Environment Links"),
      shiny::selectInput(ns("spec_id"), "Spec matrix", choices = character()),
      shiny::selectInput(ns("env_id"), "Environment matrix", choices = character()),
      shiny::selectInput(ns("relation_method"), "Relation method", choices = c("correlation", "mantel")),
      shiny::selectInput(ns("cor_method"), "Correlation", choices = c("pearson", "spearman", "kendall")),
      shiny::selectInput(ns("mantel_kind"), "Mantel kind", choices = c("block_vs_col", "col_vs_col")),
      shiny::selectInput(ns("mantel_method"), "Mantel correlation", choices = c("pearson", "spearman", "kendall")),
      shiny::selectInput(ns("mantel_alternative"), "Mantel alternative", choices = c("two.sided", "less", "greater")),
      shiny::selectInput(ns("spec_dist_method"), "Spec distance", choices = c("bray", "euclidean", "manhattan", "jaccard")),
      shiny::selectInput(ns("env_dist_method"), "Env distance", choices = c("euclidean", "manhattan", "bray", "jaccard")),
      shiny::numericInput(ns("mantel_permutations"), "Mantel permutations", value = 99, min = 1, step = 1),
      shiny::checkboxInput(ns("spec_collapse"), "Collapse species block", value = FALSE),
      shiny::checkboxInput(ns("drop_nonsig"), "Drop non-significant links", value = FALSE),
      shiny::textAreaInput(
        ns("env_blocks"),
        "Environment blocks",
        value = "",
        placeholder = "Optional: Climate: temperature,pH\nWater: moisture",
        rows = 3
      ),
      shiny::textAreaInput(
        ns("spec_blocks"),
        "Species blocks",
        value = "",
        placeholder = "Optional: Early: OTU1,OTU2,OTU3\nLate: OTU4,OTU5,OTU6",
        rows = 3
      ),
      shiny::textAreaInput(
        ns("env_spec_pairs"),
        "Environment/spec pairs",
        value = "",
        placeholder = "Optional: Climate,Early\nWater,Late",
        rows = 3
      ),
      shiny::textAreaInput(
        ns("env_orientation"),
        "Heatmap orientations",
        value = "top_right",
        placeholder = "top_right,bottom_right,top_left,bottom_left",
        rows = 2
      ),
      shiny::textAreaInput(
        ns("env_spec_layouts"),
        "Spec block layouts",
        value = "circle_outline",
        placeholder = "circle_outline,square_outline,rectangle_outline",
        rows = 2
      ),
      shiny::selectInput(
        ns("env_group_layout"),
        "Core group layout",
        choices = c("circle", "row", "column", "square", "diamond", "triangle", "triangle_down", "snake", "arc")
      ),
      shiny::numericInput(ns("env_group_angle"), "Core group rotation", value = 0, step = 15),
      shiny::numericInput(ns("env_group_arc_angle"), "Core arc angle", value = 90, step = 15),
      shiny::numericInput(ns("env_anchor_dist"), "Core anchor distance", value = 6, min = 0.5, step = 0.5),
      shiny::numericInput(ns("env_distance"), "Heatmap distance", value = 3, step = 0.5),
      shiny::numericInput(ns("env_nrow"), "Core rows", value = 1, min = 1, step = 1),
      shiny::numericInput(ns("env_ncol"), "Core columns", value = NA, min = 1, step = 1),
      shiny::checkboxInput(ns("env_scale_networks"), "Scale core networks", value = TRUE),
      shiny::numericInput(ns("env_core_point_size"), "Core point size", value = 8.5, min = 0.5, step = 0.5),
      shiny::numericInput(ns("env_heatmap_label_size"), "Heatmap label size", value = 5, min = 0.5, step = 0.5),
      shiny::numericInput(ns("env_heatmap_sig_size"), "Heatmap sig size", value = 5, min = 0.5, step = 0.5),
      shiny::numericInput(ns("env_heatmap_point_size"), "Heatmap point size", value = 5, min = 0.5, step = 0.5),
      shiny::numericInput(ns("env_sig_line_width_min"), "Sig line min width", value = 0.5, min = 0.05, step = 0.05),
      shiny::numericInput(ns("env_sig_line_width_max"), "Sig line max width", value = 2, min = 0.05, step = 0.05),
      shiny::textInput(ns("env_sig_line_color_low"), "Sig line low color", value = "#fdbb84"),
      shiny::textInput(ns("env_sig_line_color_high"), "Sig line high color", value = "#d7301f"),
      shiny::numericInput(ns("env_sig_line_alpha"), "Sig line alpha", value = 0.5, min = 0, max = 1, step = 0.05),
      shiny::selectInput(ns("module_graph_id"), "Module heatmap graph", choices = character()),
      shiny::selectInput(ns("module_index"), "Module index", choices = c("eigengene", "abundance")),
      shiny::selectInput(ns("module_abundance_type"), "Module abundance", choices = c("sum", "mean")),
      shiny::selectInput(ns("triple_graph_id"), "Triple heatmap graph", choices = character()),
      shiny::numericInput(ns("triple_feature_count"), "Triple feature count", value = 3, min = 1, step = 1),
      shiny::actionButton(ns("run_environment"), "Run environment link"),
      shiny::actionButton(ns("run_environment_manual"), "Run manual heatmap"),
      shiny::actionButton(ns("run_module_environment"), "Run module heatmap"),
      shiny::actionButton(ns("run_environment_triple"), "Run triple heatmap"),
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
    bslib::card(
      bslib::card_header("Link Summary"),
      DT::DTOutput(ns("compare_links"))
    ),
    bslib::card(
      bslib::card_header("Report Presets"),
      DT::DTOutput(ns("report_presets"))
    ),
    bslib::card(
      bslib::card_header("Topology Comparison"),
      DT::DTOutput(ns("compare_topology"))
    ),
    col_widths = c(4, 4, 8, 6, 6, 6, 6)
  )
}

mod_compare_environment_server <- function(id, registry) {
  shiny::moduleServer(id, function(input, output, session) {
    plot_obj <- shiny::reactiveVal(NULL)
    stats_table <- shiny::reactiveVal(data.frame())
    compare_link_summary_table <- shiny::reactiveVal(data.frame())
    report_preset_table <- shiny::reactiveVal(data.frame())
    compare_topology_table <- shiny::reactiveVal(data.frame())
    status <- shiny::reactiveVal("No comparison or environment result yet.")

    unique_output_name <- function(base) {
      suffix <- paste0(format(Sys.time(), "%Y%m%d_%H%M%S"), "_", sprintf("%04d", sample.int(9999, 1)))
      paste0(base, "_", suffix)
    }

    shiny::observe({
      graph_choices <- registry_choices(registry, type = "graph")
      shiny::updateSelectizeInput(session, "compare_graph_ids", choices = graph_choices, server = TRUE)
      selected_triple <- input$triple_graph_id
      if (is.null(selected_triple) || !selected_triple %in% graph_choices) {
        selected_triple <- if (length(graph_choices)) graph_choices[[1]] else character()
      }
      selected_module <- input$module_graph_id
      if (is.null(selected_module) || !selected_module %in% graph_choices) {
        selected_module <- if (length(graph_choices)) graph_choices[[1]] else character()
      }
      shiny::updateSelectInput(session, "module_graph_id", choices = graph_choices, selected = selected_module)
      shiny::updateSelectInput(session, "triple_graph_id", choices = graph_choices, selected = selected_triple)
    })

    shiny::observe({
      matrix_choices <- registry_choices_by_type(registry, c("matrix"))
      sample_choices <- registry_choices_by_type(registry, c("sample_metadata"))
      env_choices <- registry_choices_by_type(registry, c("env_matrix", "matrix"))
      selected_spec <- input$spec_id
      if (is.null(selected_spec) || !selected_spec %in% matrix_choices) {
        selected_spec <- if (length(matrix_choices)) matrix_choices[[1]] else character()
      }
      selected_env <- input$env_id
      if (is.null(selected_env) || !selected_env %in% env_choices) {
        selected_env <- if (length(env_choices)) env_choices[[1]] else character()
      }
      selected_multi <- input$multi_matrix_id
      if (is.null(selected_multi) || !selected_multi %in% matrix_choices) {
        selected_multi <- if (length(matrix_choices)) matrix_choices[[1]] else character()
      }
      group_choices <- c("Generated groups" = "", sample_choices)
      selected_group <- input$multi_group_id
      if (is.null(selected_group) || !selected_group %in% group_choices) {
        selected_group <- ""
      }
      shiny::updateSelectInput(session, "spec_id", choices = matrix_choices, selected = selected_spec)
      shiny::updateSelectInput(session, "env_id", choices = env_choices, selected = selected_env)
      shiny::updateSelectInput(session, "multi_matrix_id", choices = matrix_choices, selected = selected_multi)
      shiny::updateSelectInput(session, "multi_group_id", choices = group_choices, selected = selected_group)
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

    normalize_stats <- function(stats) {
      if (is.null(stats)) {
        return(data.frame())
      }
      if (is.data.frame(stats)) {
        return(stats)
      }
      if (is.atomic(stats)) {
        return(data.frame(value = stats, stringsAsFactors = FALSE))
      }
      data.frame(value = utils::capture.output(utils::str(stats)), stringsAsFactors = FALSE)
    }

    environment_interpretation <- function(stats) {
      interpret_environment_links(normalize_stats(stats))
    }

    register_environment_report <- function(interpreted, source_ids, kind) {
      report <- interpreted$report %||% data.frame()
      report_preset_table(report)
      if (!nrow(report)) {
        return(NULL)
      }
      register_stats_result(
        unique_output_name(paste0(kind, "_report")),
        report,
        source_ids,
        list(kind = paste0(kind, "_report"))
      )
    }

    environment_block_message <- function(result) {
      env_names <- names(result$value$env_select %||% list())
      spec_names <- names(result$value$spec_select %||% list())
      applied <- if (length(env_names) || length(spec_names)) {
        paste0(
          " Applied blocks: env = ",
          paste(env_names, collapse = ", "),
          "; spec = ",
          paste(spec_names, collapse = ", "),
          "."
        )
      } else {
        ""
      }
      pairs <- result$value$comparison_pairs %||% list()
      pair_message <- if (length(pairs)) {
        paste0(
          " Pair restrictions: ",
          paste(vapply(pairs, paste, character(1), collapse = " vs "), collapse = "; "),
          "."
        )
      } else {
        ""
      }
      warnings <- result$value$block_warnings %||% character()
      if (length(warnings)) {
        paste0(applied, pair_message, "\n", paste(warnings, collapse = "\n"))
      } else {
        paste0(applied, pair_message)
      }
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
        scale_groups = input$scale_groups,
        comparison_pairs = input$comparison_pairs,
        include_topology_summary = TRUE
      )

      status(task_feedback_message("network comparison", "running"))
      result <- with_task_feedback(
        session,
        "network comparison",
        session$ns("run_compare"),
        safe_multi_network_compare(graphs, params = params)
      )
      if (!result$ok) {
        detail <- if (!is.null(result$trace)) paste(result$message, result$trace, sep = "\n") else result$message
        status(detail)
        shiny::showNotification(result$message, type = "error")
        return()
      }

      plot_obj(result$value$plot)
      stats <- result$value$link_table
      stats_table(stats)
      compare_link_summary_table(result$value$link_summary)
      compare_topology_table(result$value$topology_table)

      source_ids <- paste(input$compare_graph_ids, collapse = ",")
      plot_params <- params[names(params) != "include_topology_summary"]
      plot_item <- register_plot_result(
        unique_output_name("multi_network_compare_plot"),
        result$value$plot,
        source_ids,
        plot_params
      )
      register_stats_result(
        unique_output_name("multi_network_compare_links"),
        stats,
        source_ids,
        plot_params
      )
      if (nrow(result$value$link_summary)) {
        register_stats_result(
          unique_output_name("multi_network_compare_link_summary"),
          result$value$link_summary,
          source_ids,
          list(kind = "comparison_link_summary")
        )
      }
      if (nrow(result$value$topology_table)) {
        register_stats_result(
          unique_output_name("multi_network_compare_topology"),
          result$value$topology_table,
          source_ids,
          list(kind = "comparison_topology")
        )
      }
      pair_message <- if (length(result$value$comparison_pairs)) {
        paste0(" Applied pairs: ", paste(vapply(result$value$comparison_pairs, paste, character(1), collapse = " vs "), collapse = "; "), ".")
      } else {
        ""
      }
      warning_message <- if (length(result$value$comparison_warnings)) {
        paste0("\n", paste(result$value$comparison_warnings, collapse = "\n"))
      } else {
        ""
      }
      status(paste0("Registered comparison plot: ", plot_item$name, pair_message, warning_message))
      shiny::showNotification(paste("Registered comparison plot:", plot_item$name), type = "message")
    })

    shiny::observeEvent(input$run_multi_group, {
      shiny::req(input$multi_matrix_id)
      matrix_item <- registry_get(registry, input$multi_matrix_id)
      shiny::req(matrix_item)

      group_id <- input$multi_group_id
      if (is.null(group_id)) {
        group_id <- ""
      }
      group_item <- if (nzchar(group_id)) registry_get(registry, group_id) else NULL
      group_info <- tryCatch({
        if (is.null(group_item)) {
          default_group_info_for_matrix(matrix_item$data, split = input$multi_split)
        } else {
          align_group_info_for_matrix(matrix_item$data, group_item$data)
        }
      }, error = function(e) e)
      if (inherits(group_info, "error")) {
        status(conditionMessage(group_info))
        shiny::showNotification(conditionMessage(group_info), type = "error")
        return()
      }

      params <- list(
        layout = "circle",
        layout.module = "adjacent",
        r.threshold = 0.2,
        p.threshold = 1
      )
      status(task_feedback_message("group multi-network plot", "running"))
      result <- with_task_feedback(
        session,
        "group multi-network plot",
        session$ns("run_multi_group"),
        safe_multi_group_network(matrix_item$data, group_info = group_info, params = params)
      )
      if (!result$ok) {
        detail <- if (!is.null(result$trace)) paste(result$message, result$trace, sep = "\n") else result$message
        status(detail)
        shiny::showNotification(result$message, type = "error")
        return()
      }

      plot_obj(result$value$plot)
      stats_table(result$value$group_info)
      compare_link_summary_table(data.frame())
      compare_topology_table(data.frame())
      plot_item <- register_plot_result(
        unique_output_name("multi_group_network_plot"),
        result$value$plot,
        paste(c(input$multi_matrix_id, group_id), collapse = ","),
        c(params, list(split = input$multi_split, group_metadata = group_id))
      )
      register_stats_result(
        unique_output_name("multi_group_network_groups"),
        result$value$group_info,
        paste(c(input$multi_matrix_id, group_id), collapse = ","),
        list(split = input$multi_split, group_metadata = group_id)
      )
      status(paste("Registered grouped network plot:", plot_item$name))
      shiny::showNotification(paste("Registered grouped network plot:", plot_item$name), type = "message")
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

      geometry_params <- environment_geometry_params(
        orientation_text = input$env_orientation,
        spec_layout_text = input$env_spec_layouts,
        group_layout = input$env_group_layout,
        group_angle = input$env_group_angle,
        group_arc_angle = input$env_group_arc_angle,
        anchor_dist = input$env_anchor_dist,
        distance = input$env_distance,
        nrow = input$env_nrow,
        ncol = input$env_ncol,
        scale_networks = input$env_scale_networks,
        core_point_size = input$env_core_point_size,
        heatmap_label_size = input$env_heatmap_label_size,
        heatmap_sig_size = input$env_heatmap_sig_size,
        heatmap_point_size = input$env_heatmap_point_size,
        sig_line_width_min = input$env_sig_line_width_min,
        sig_line_width_max = input$env_sig_line_width_max,
        sig_line_color_low = input$env_sig_line_color_low,
        sig_line_color_high = input$env_sig_line_color_high,
        sig_line_alpha = input$env_sig_line_alpha
      )
      mantel_params <- environment_mantel_params(
        method = input$mantel_method,
        alternative = input$mantel_alternative,
        spec_dist_method = input$spec_dist_method,
        env_dist_method = input$env_dist_method,
        permutations = input$mantel_permutations
      )
      params <- c(list(
        relation_method = input$relation_method,
        cor.method = input$cor_method,
        drop_nonsig = input$drop_nonsig,
        env_blocks = input$env_blocks,
        spec_blocks = input$spec_blocks,
        env_spec_pairs = input$env_spec_pairs
      ), mantel_params, geometry_params)
      status(task_feedback_message("environment link", "running"))
      result <- with_task_feedback(
        session,
        "environment link",
        session$ns("run_environment"),
        safe_environment_link(env = env, spec = spec, params = params)
      )
      if (!result$ok) {
        detail <- if (!is.null(result$trace)) paste(result$message, result$trace, sep = "\n") else result$message
        status(detail)
        shiny::showNotification(result$message, type = "error")
        return()
      }

      plot_obj(result$value$plot)
      interpreted <- environment_interpretation(result$value$stats)
      stats_table(interpreted$details)
      compare_link_summary_table(interpreted$summary)
      compare_topology_table(data.frame())
      source_ids <- paste(input$spec_id, input$env_id, sep = ",")
      register_environment_report(interpreted, source_ids, "environment_link")
      plot_item <- register_plot_result(
        unique_output_name("environment_link_plot"),
        result$value$plot,
        source_ids,
        params
      )
      register_stats_result(
        unique_output_name("environment_link_stats"),
        interpreted$details,
        source_ids,
        params
      )
      if (nrow(interpreted$summary)) {
        register_stats_result(
          unique_output_name("environment_link_summary"),
          interpreted$summary,
          source_ids,
          list(kind = "environment_link_summary")
        )
      }
      status(paste0("Registered environment link plot: ", plot_item$name, environment_block_message(result)))
      shiny::showNotification(paste("Registered environment link plot:", plot_item$name), type = "message")
    })

    shiny::observeEvent(input$run_environment_manual, {
      shiny::req(input$spec_id, input$env_id)
      spec_item <- registry_get(registry, input$spec_id)
      env_item <- registry_get(registry, input$env_id)
      shiny::req(spec_item, env_item)

      spec <- as.data.frame(t(as.matrix(spec_item$data)), check.names = FALSE)
      env <- as.data.frame(env_item$data, check.names = FALSE)
      if (nrow(env) != nrow(spec)) {
        env <- as.data.frame(t(as.matrix(env_item$data)), check.names = FALSE)
      }

      geometry_params <- environment_geometry_params(
        orientation_text = input$env_orientation,
        spec_layout_text = input$env_spec_layouts,
        group_layout = input$env_group_layout,
        group_angle = input$env_group_angle,
        group_arc_angle = input$env_group_arc_angle,
        anchor_dist = input$env_anchor_dist,
        distance = input$env_distance,
        nrow = input$env_nrow,
        ncol = input$env_ncol,
        scale_networks = input$env_scale_networks,
        core_point_size = input$env_core_point_size,
        heatmap_label_size = input$env_heatmap_label_size,
        heatmap_sig_size = input$env_heatmap_sig_size,
        heatmap_point_size = input$env_heatmap_point_size,
        sig_line_width_min = input$env_sig_line_width_min,
        sig_line_width_max = input$env_sig_line_width_max,
        sig_line_color_low = input$env_sig_line_color_low,
        sig_line_color_high = input$env_sig_line_color_high,
        sig_line_alpha = input$env_sig_line_alpha
      )
      mantel_params <- environment_mantel_params(
        method = input$mantel_method,
        alternative = input$mantel_alternative,
        spec_dist_method = input$spec_dist_method,
        env_dist_method = input$env_dist_method,
        permutations = input$mantel_permutations
      )
      params <- c(list(
        relation_method = input$relation_method,
        cor.method = input$cor_method,
        mantel_kind = input$mantel_kind,
        spec_collapse = input$spec_collapse,
        drop_nonsig = input$drop_nonsig,
        env_blocks = input$env_blocks,
        spec_blocks = input$spec_blocks,
        env_spec_pairs = input$env_spec_pairs
      ), mantel_params, geometry_params)
      status(task_feedback_message("manual environment heatmap", "running"))
      result <- with_task_feedback(
        session,
        "manual environment heatmap",
        session$ns("run_environment_manual"),
        safe_environment_heatmap(env = env, spec = spec, params = params)
      )
      if (!result$ok) {
        detail <- if (!is.null(result$trace)) paste(result$message, result$trace, sep = "\n") else result$message
        status(detail)
        shiny::showNotification(result$message, type = "error")
        return()
      }

      plot_obj(result$value$plot)
      interpreted <- environment_interpretation(result$value$stats)
      stats <- interpreted$details
      stats_table(stats)
      compare_link_summary_table(interpreted$summary)
      compare_topology_table(data.frame())
      source_ids <- paste(input$spec_id, input$env_id, sep = ",")
      register_environment_report(interpreted, source_ids, "manual_environment_heatmap")
      plot_item <- register_plot_result(
        unique_output_name("manual_environment_heatmap_plot"),
        result$value$plot,
        source_ids,
        params
      )
      register_stats_result(
        unique_output_name("manual_environment_heatmap_stats"),
        stats,
        source_ids,
        params
      )
      if (nrow(interpreted$summary)) {
        register_stats_result(
          unique_output_name("manual_environment_heatmap_summary"),
          interpreted$summary,
          source_ids,
          list(kind = "manual_environment_heatmap_summary")
        )
      }
      status(paste0("Registered manual environment heatmap: ", plot_item$name, environment_block_message(result)))
      shiny::showNotification(paste("Registered manual environment heatmap:", plot_item$name), type = "message")
    })

    shiny::observeEvent(input$run_module_environment, {
      shiny::req(input$spec_id, input$env_id, input$module_graph_id)
      spec_item <- registry_get(registry, input$spec_id)
      env_item <- registry_get(registry, input$env_id)
      graph_item <- registry_get(registry, input$module_graph_id)
      shiny::req(spec_item, env_item, graph_item)

      otu_mat <- as.matrix(spec_item$data)
      env <- as.data.frame(env_item$data, check.names = FALSE)
      if (nrow(env) != ncol(otu_mat)) {
        env <- as.data.frame(t(as.matrix(env_item$data)), check.names = FALSE)
      }

      geometry_params <- environment_geometry_params(
        orientation_text = input$env_orientation,
        spec_layout_text = input$env_spec_layouts,
        group_layout = input$env_group_layout,
        group_angle = input$env_group_angle,
        group_arc_angle = input$env_group_arc_angle,
        anchor_dist = input$env_anchor_dist,
        distance = input$env_distance,
        nrow = input$env_nrow,
        ncol = input$env_ncol,
        scale_networks = input$env_scale_networks,
        core_point_size = input$env_core_point_size,
        heatmap_label_size = input$env_heatmap_label_size,
        heatmap_sig_size = input$env_heatmap_sig_size,
        heatmap_point_size = input$env_heatmap_point_size,
        sig_line_width_min = input$env_sig_line_width_min,
        sig_line_width_max = input$env_sig_line_width_max,
        sig_line_color_low = input$env_sig_line_color_low,
        sig_line_color_high = input$env_sig_line_color_high,
        sig_line_alpha = input$env_sig_line_alpha
      )
      mantel_params <- environment_mantel_params(
        method = input$mantel_method,
        alternative = input$mantel_alternative,
        spec_dist_method = input$spec_dist_method,
        env_dist_method = input$env_dist_method,
        permutations = input$mantel_permutations
      )
      geometry_params$spec_layout <- NULL
      geometry_params$group_layout <- NULL
      geometry_params$group_angle <- NULL
      geometry_params$group_arc_angle <- NULL
      geometry_params$anchor_dist <- NULL
      geometry_params$nrow <- NULL
      geometry_params$scale_networks <- NULL
      geometry_params$core_point_size <- NULL
      params <- c(list(
        relation_method = input$relation_method,
        cor.method = input$cor_method,
        mantel_kind = input$mantel_kind,
        drop_nonsig = input$drop_nonsig,
        env_blocks = input$env_blocks,
        module_index = input$module_index,
        abundance_type = input$module_abundance_type,
        layout = "circle",
        layout.module = "adjacent"
      ), mantel_params, geometry_params)
      status(task_feedback_message("module environment heatmap", "running"))
      result <- with_task_feedback(
        session,
        "module environment heatmap",
        session$ns("run_module_environment"),
        safe_module_environment_heatmap(
          graph = graph_item$data,
          env = env,
          otu_mat = otu_mat,
          params = params
        )
      )
      if (!result$ok) {
        detail <- if (!is.null(result$trace)) paste(result$message, result$trace, sep = "\n") else result$message
        status(detail)
        shiny::showNotification(result$message, type = "error")
        return()
      }

      plot_obj(result$value$plot)
      interpreted <- environment_interpretation(result$value$stats)
      stats <- interpreted$details
      stats_table(stats)
      compare_link_summary_table(interpreted$summary)
      compare_topology_table(data.frame())
      source_ids <- paste(input$spec_id, input$env_id, input$module_graph_id, sep = ",")
      register_environment_report(interpreted, source_ids, "module_environment_heatmap")
      plot_item <- register_plot_result(
        unique_output_name("module_environment_heatmap_plot"),
        result$value$plot,
        source_ids,
        params
      )
      register_stats_result(
        unique_output_name("module_environment_heatmap_stats"),
        stats,
        source_ids,
        params
      )
      if (nrow(interpreted$summary)) {
        register_stats_result(
          unique_output_name("module_environment_heatmap_summary"),
          interpreted$summary,
          source_ids,
          list(kind = "module_environment_heatmap_summary")
        )
      }
      env_names <- names(result$value$env_select %||% list())
      block_message <- if (length(env_names)) {
        paste0(" Applied module env blocks: ", paste(env_names, collapse = ", "), ".")
      } else {
        ""
      }
      warning_message <- if (length(result$value$block_warnings)) {
        paste0("\n", paste(result$value$block_warnings, collapse = "\n"))
      } else {
        ""
      }
      status(paste0("Registered module environment heatmap: ", plot_item$name, block_message, warning_message))
      shiny::showNotification(paste("Registered module environment heatmap:", plot_item$name), type = "message")
    })

    shiny::observeEvent(input$run_environment_triple, {
      shiny::req(input$spec_id, input$env_id, input$triple_graph_id)
      spec_item <- registry_get(registry, input$spec_id)
      env_item <- registry_get(registry, input$env_id)
      graph_item <- registry_get(registry, input$triple_graph_id)
      shiny::req(spec_item, env_item, graph_item)

      experiment <- as.data.frame(t(as.matrix(spec_item$data)), check.names = FALSE)
      env <- as.data.frame(env_item$data, check.names = FALSE)
      if (nrow(env) != nrow(experiment)) {
        env <- as.data.frame(t(as.matrix(env_item$data)), check.names = FALSE)
      }

      params <- list(
        feature_count = input$triple_feature_count,
        r = 6
      )
      status(task_feedback_message("triple environment heatmap", "running"))
      result <- with_task_feedback(
        session,
        "triple environment heatmap",
        session$ns("run_environment_triple"),
        safe_environment_triple_heatmap(
          env = env,
          experiment = experiment,
          graph = graph_item$data,
          params = params
        )
      )
      if (!result$ok) {
        detail <- if (!is.null(result$trace)) paste(result$message, result$trace, sep = "\n") else result$message
        status(detail)
        shiny::showNotification(result$message, type = "error")
        return()
      }

      plot_obj(result$value$plot)
      stats <- data.frame(
        table = c("nodes", "edges", "experiment_features"),
        rows = c(nrow(result$value$nodes), nrow(result$value$edges), ncol(result$value$experiment)),
        stringsAsFactors = FALSE
      )
      stats_table(stats)
      compare_link_summary_table(data.frame())
      report_preset_table(data.frame())
      compare_topology_table(data.frame())
      source_ids <- paste(input$spec_id, input$env_id, input$triple_graph_id, sep = ",")
      plot_item <- register_plot_result(
        unique_output_name("triple_environment_heatmap_plot"),
        result$value$plot,
        source_ids,
        params
      )
      register_stats_result(
        unique_output_name("triple_environment_heatmap_summary"),
        stats,
        source_ids,
        params
      )
      status(paste("Registered triple environment heatmap:", plot_item$name))
      shiny::showNotification(paste("Registered triple environment heatmap:", plot_item$name), type = "message")
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

      mantel_params <- environment_mantel_params(
        method = input$mantel_method,
        alternative = input$mantel_alternative,
        spec_dist_method = input$spec_dist_method,
        env_dist_method = input$env_dist_method,
        permutations = input$mantel_permutations
      )
      params <- c(list(mantel_kind = input$mantel_kind), mantel_params)
      status(task_feedback_message("Mantel table", "running"))
      result <- with_task_feedback(
        session,
        "Mantel table",
        session$ns("run_mantel"),
        safe_mantel_table(spec, env, params = params)
      )
      if (!result$ok) {
        detail <- if (!is.null(result$trace)) paste(result$message, result$trace, sep = "\n") else result$message
        status(detail)
        shiny::showNotification(result$message, type = "error")
        return()
      }

      interpreted <- environment_interpretation(result$value)
      stats_table(interpreted$details)
      compare_link_summary_table(interpreted$summary)
      compare_topology_table(data.frame())
      register_environment_report(
        interpreted,
        paste(input$spec_id, input$env_id, sep = ","),
        "mantel_pairwise"
      )
      item <- register_stats_result(
        unique_output_name("mantel_pairwise_stats"),
        interpreted$details,
        paste(input$spec_id, input$env_id, sep = ","),
        params
      )
      if (nrow(interpreted$summary)) {
        register_stats_result(
          unique_output_name("mantel_pairwise_summary"),
          interpreted$summary,
          paste(input$spec_id, input$env_id, sep = ","),
          list(kind = "mantel_summary")
        )
      }
      status(paste("Registered Mantel result:", item$name))
      shiny::showNotification(paste("Registered Mantel result:", item$name), type = "message")
    })

    output$plot <- shiny::renderPlot({
      shiny::req(plot_obj())
      plot_obj()
    })

    output$stats <- DT::renderDT(stats_table(), rownames = FALSE)
    output$compare_links <- DT::renderDT(compare_link_summary_table(), rownames = FALSE)
    output$report_presets <- DT::renderDT(report_preset_table(), rownames = FALSE)
    output$compare_topology <- DT::renderDT(compare_topology_table(), rownames = FALSE)
    output$status <- shiny::renderText(status())
  })
}
