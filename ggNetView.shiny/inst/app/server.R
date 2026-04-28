# server.R — ggNetView Shiny app

server <- function(input, output, session) {

  # ---------------------------------------------------------------------------
  # Shared reactive state
  # ---------------------------------------------------------------------------
  state <- reactiveValues(
    raw       = NULL,   # input matrix / edge data.frame / adjacency
    raw_kind  = NULL,   # "matrix", "df", "adj", "graph"
    anno      = NULL,   # node annotation
    graph     = NULL,   # tbl_graph / igraph
    last_plot = NULL,   # ggplot of network
    topo      = NULL,   # topology output
    zipi_df   = NULL,   # zi-pi data.frame
    zipi_plot = NULL,   # zi-pi ggplot
    link_plot = NULL    # gglink_heatmaps output
  )

  # ===========================================================================
  # 1. DATA TAB
  # ===========================================================================
  observeEvent(input$load_data, {
    res <- tryCatch({
      if (input$data_source == "builtin") {
        get_ggNetView_data(input$builtin_dataset)
      } else {
        req(input$upload_file)
        rn_col <- input$upload_rownames_col
        if (is.na(rn_col) || rn_col < 1) rn_col <- NULL
        read_user_table(input$upload_file$datapath,
                        has_header     = input$upload_header,
                        row_names_col  = rn_col)
      }
    }, error = function(e) e)

    if (inherits(res, "error")) {
      showNotification(paste("Load failed:", conditionMessage(res)),
                       type = "error", duration = 8)
      return()
    }

    state$raw <- res
    state$raw_kind <- if (is_graph_obj(res)) {
      "graph"
    } else if (is.data.frame(res) && all(c("from", "to") %in% names(res))) {
      "df"
    } else if (is.matrix(res) && nrow(res) == ncol(res) &&
               !is.null(rownames(res)) &&
               identical(rownames(res), colnames(res))) {
      "adj"
    } else {
      "matrix"
    }

    # If it's already a tbl_graph, also fill state$graph
    if (state$raw_kind == "graph") state$graph <- res

    # optional node annotation
    if (!is.null(input$upload_anno)) {
      anno <- tryCatch(
        read_user_table(input$upload_anno$datapath,
                        has_header = input$anno_header),
        error = function(e) NULL)
      state$anno <- anno
    }

    showNotification("Data loaded.", type = "message", duration = 4)
  })

  output$data_summary <- renderText({
    if (is.null(state$raw)) return("No data loaded yet.")
    paste0(
      "Detected kind:  ", state$raw_kind, "\n",
      "Object:         ", describe_object(state$raw), "\n",
      if (!is.null(state$anno))
        paste0("Annotation:     ", describe_object(state$anno), "\n")
      else "Annotation:     (none)\n"
    )
  })

  output$data_preview <- DT::renderDT({
    req(state$raw)
    obj <- state$raw
    if (is.matrix(obj) || is.data.frame(obj)) {
      DT::datatable(
        as.data.frame(obj)[seq_len(min(8, nrow(obj))),
                           seq_len(min(8, ncol(obj))), drop = FALSE],
        options = list(scrollX = TRUE, dom = "t"))
    } else {
      DT::datatable(data.frame(class = class(obj)[1]))
    }
  })

  # ===========================================================================
  # 2. BUILD NETWORK TAB
  # ===========================================================================
  observeEvent(input$build_go, {
    if (is.null(state$raw)) {
      showNotification("Please load data first (Data tab).",
                       type = "error", duration = 6); return()
    }

    showNotification("Building network — this may take a while …",
                     type = "message", duration = 4)

    g <- tryCatch({
      switch(input$build_kind,
        "mat" = {
          mat <- as.matrix(state$raw)
          ggNetView::build_graph_from_mat(
            mat              = mat,
            transfrom.method = input$bg_trans,
            method           = input$bg_method,
            cor.method       = input$bg_cor_method,
            proc             = input$bg_proc,
            r.threshold      = input$bg_r,
            p.threshold      = input$bg_p,
            module.method    = input$bg_module,
            SpiecEasi.method = if (!is.null(input$bg_se_method))
              input$bg_se_method else "mb",
            sparcc_R         = if (!is.null(input$bg_sparcc_R))
              input$bg_sparcc_R else 20,
            top_modules      = input$bg_top_modules,
            node_annotation  = state$anno,
            seed             = input$bg_seed
          )
        },
        "adj" = {
          ggNetView::build_graph_from_adj_mat(
            adjacency_matrix = as.matrix(state$raw),
            module.method    = input$bg_adj_module,
            top_modules      = input$bg_adj_top,
            node_annotation  = state$anno,
            seed             = input$bg_adj_seed
          )
        },
        "df"  = {
          df <- as.data.frame(state$raw)
          ggNetView::build_graph_from_df(
            df               = df,
            directed         = input$bg_df_directed,
            module.method    = input$bg_df_module,
            top_modules      = input$bg_df_top,
            node_annotation  = state$anno,
            seed             = input$bg_df_seed
          )
        }
      )
    }, error = function(e) e)

    if (inherits(g, "error")) {
      showNotification(paste("Network build failed:",
                             conditionMessage(g)),
                       type = "error", duration = 10)
      return()
    }

    state$graph <- g
    showNotification("Network built successfully.",
                     type = "message", duration = 4)
  })

  output$graph_summary <- renderText({
    if (is.null(state$graph)) return("No graph yet — click 'Build network'.")
    g <- state$graph
    paste0(
      "Graph object: ", describe_object(g), "\n",
      "Class:        ", paste(class(g), collapse = ", "), "\n",
      "Nodes:        ", igraph::vcount(g), "\n",
      "Edges:        ", igraph::ecount(g), "\n",
      "Components:   ", igraph::components(g)$no, "\n"
    )
  })

  output$graph_modules <- DT::renderDT({
    req(state$graph)
    nodes <- tryCatch(ggNetView::get_graph_nodes(state$graph),
                      error = function(e) NULL)
    if (is.null(nodes) || !"Modularity" %in% names(nodes)) {
      return(DT::datatable(
        data.frame(message = "No 'Modularity' column on graph nodes."),
        options = list(dom = "t")))
    }
    tab <- as.data.frame(table(Module = nodes$Modularity),
                         stringsAsFactors = FALSE)
    names(tab) <- c("Module", "n_nodes")
    tab <- tab[order(-tab$n_nodes), , drop = FALSE]
    DT::datatable(tab, options = list(pageLength = 10, scrollX = TRUE))
  })

  output$download_graph <- downloadHandler(
    filename = function() "ggNetView_graph.rds",
    content  = function(file) {
      req(state$graph); saveRDS(state$graph, file)
    }
  )

  # ===========================================================================
  # 3. VISUALIZE TAB
  # ===========================================================================
  do_render <- function() {
    req(state$graph)
    pl_arg <- if (nzchar(input$vis_pointlabel) &&
                  input$vis_pointlabel != "NULL")
      input$vis_pointlabel else NULL
    color_arg <- if (nzchar(input$vis_color_by)) input$vis_color_by else NULL

    p <- ggNetView::ggNetView(
      graph_obj      = state$graph,
      layout         = input$vis_layout,
      layout.module  = input$vis_layout_module,
      group.by       = input$vis_group_by,
      fill.by        = input$vis_fill_by,
      color.by       = color_arg,
      pointsize      = c(input$vis_pointsize[1], input$vis_pointsize[2]),
      center         = input$vis_center,
      jitter         = input$vis_jitter,
      jitter_sd      = input$vis_jitter_sd,
      mapping_line   = input$vis_mapping_line,
      curve          = input$vis_curve,
      curvature      = input$vis_curvature,
      shrink         = input$vis_shrink,
      linealpha      = input$vis_linealpha,
      linecolor      = input$vis_linecolor,
      add_outer      = input$vis_add_outer,
      outerwidth     = input$vis_outerwidth,
      label          = input$vis_label,
      labelsize      = input$vis_labelsize,
      pointlabel     = pl_arg,
      seed           = input$vis_seed
    )
    state$last_plot <- p
    p
  }

  output$vis_plot <- renderPlot({
    req(input$vis_render)        # only re-render on button press
    isolate(do_render())
  })

  output$vis_download_pdf <- downloadHandler(
    filename = function() "ggNetView_plot.pdf",
    content  = function(file) {
      req(state$last_plot)
      ggplot2::ggsave(
        file, plot = state$last_plot,
        width = input$vis_export_w, height = input$vis_export_h,
        device = "pdf", limitsize = FALSE)
    }
  )

  output$vis_download_png <- downloadHandler(
    filename = function() "ggNetView_plot.png",
    content  = function(file) {
      req(state$last_plot)
      ggplot2::ggsave(
        file, plot = state$last_plot,
        width = input$vis_export_w, height = input$vis_export_h,
        dpi = 300, device = "png", limitsize = FALSE)
    }
  )

  # ===========================================================================
  # 4. TOPOLOGY / zi-pi TAB
  # ===========================================================================
  observeEvent(input$topo_run, {
    req(state$graph)
    res <- tryCatch(
      ggNetView::get_network_topology(graph_obj = state$graph),
      error = function(e) e)
    if (inherits(res, "error")) {
      showNotification(paste("Topology failed:", conditionMessage(res)),
                       type = "error", duration = 8); return()
    }
    state$topo <- res
    showNotification("Topology computed.", type = "message", duration = 3)
  })

  output$topo_table <- DT::renderDT({
    req(state$topo)
    df <- if (is.list(state$topo) && !is.data.frame(state$topo)) {
      tryCatch(as.data.frame(state$topo[[1]]),
               error = function(e) data.frame(out = "see RDS"))
    } else as.data.frame(state$topo)
    DT::datatable(df, options = list(scrollX = TRUE, pageLength = 15))
  })

  output$topo_download <- downloadHandler(
    filename = function() "ggNetView_topology.csv",
    content  = function(file) {
      req(state$topo)
      df <- if (is.list(state$topo) && !is.data.frame(state$topo))
        as.data.frame(state$topo[[1]]) else as.data.frame(state$topo)
      utils::write.csv(df, file, row.names = FALSE)
    }
  )

  # ----- zi-pi ----------------------------------------------------------------
  observeEvent(input$zipi_run, {
    req(state$graph)
    nodes <- tryCatch(ggNetView::get_graph_nodes(state$graph),
                      error = function(e) NULL)
    if (is.null(nodes)) {
      showNotification("Could not extract node table.",
                       type = "error", duration = 6); return()
    }
    z_mat <- tryCatch(igraph::as_adjacency_matrix(state$graph,
                                                  attr   = "weight",
                                                  sparse = FALSE),
                      error = function(e)
                        igraph::as_adjacency_matrix(state$graph,
                                                    sparse = FALSE))

    res <- tryCatch(
      ggNetView::ggnetview_zipi(
        nodes_bulk      = nodes,
        z_bulk_mat      = z_mat,
        modularity_col  = input$zipi_modularity_col,
        degree_col      = input$zipi_degree_col,
        zi_threshold    = input$zipi_zi_thr,
        pi_threshold    = input$zipi_pi_thr,
        na.rm           = input$zipi_na_rm
      ),
      error = function(e) e)

    if (inherits(res, "error")) {
      showNotification(paste("zi-pi failed:", conditionMessage(res)),
                       type = "error", duration = 10); return()
    }

    # ggnetview_zipi may return a list or a ggplot or both
    if (is.list(res) && !inherits(res, "ggplot")) {
      df <- res[sapply(res, is.data.frame)]
      pl <- res[sapply(res, function(x) inherits(x, "ggplot"))]
      state$zipi_df   <- if (length(df)) df[[1]] else NULL
      state$zipi_plot <- if (length(pl)) pl[[1]] else NULL
    } else if (inherits(res, "ggplot")) {
      state$zipi_plot <- res
    } else if (is.data.frame(res)) {
      state$zipi_df <- res
    }
    showNotification("zi-pi computed.", type = "message", duration = 3)
  })

  output$zipi_plot <- renderPlot({
    if (is.null(state$zipi_plot))
      return(ggplot2::ggplot() +
               ggplot2::annotate("text", x = 0, y = 0,
                                 label = "Run 'Compute zi-pi' first.") +
               ggplot2::theme_void())
    state$zipi_plot
  })

  output$zipi_table <- DT::renderDT({
    req(state$zipi_df)
    DT::datatable(state$zipi_df, options = list(scrollX = TRUE,
                                                pageLength = 15))
  })

  output$zipi_download <- downloadHandler(
    filename = function() "ggNetView_zipi.csv",
    content  = function(file) {
      req(state$zipi_df); utils::write.csv(state$zipi_df, file,
                                           row.names = FALSE)
    }
  )

  # ===========================================================================
  # 5. ENV-SPEC LINKAGE TAB
  # ===========================================================================
  resolve_block <- function(text) {
    text <- trimws(text)
    if (!nzchar(text)) return(NULL)
    tryCatch(eval(parse(text = text)), error = function(e) {
      showNotification(paste("Block parse error:",
                             conditionMessage(e)),
                       type = "error", duration = 8)
      NULL
    })
  }

  get_envspec_data <- function() {
    env_obj <- if (input$env_src == "builtin") {
      get_ggNetView_data(input$env_builtin)
    } else {
      req(input$env_upload)
      read_user_table(input$env_upload$datapath)
    }
    spec_obj <- if (input$spec_src == "builtin") {
      get_ggNetView_data(input$spec_builtin)
    } else {
      req(input$spec_upload)
      read_user_table(input$spec_upload$datapath)
    }
    list(env = env_obj, spec = spec_obj)
  }

  observeEvent(input$link_run, {
    dat <- tryCatch(get_envspec_data(), error = function(e) e)
    if (inherits(dat, "error")) {
      showNotification(paste("Data load failed:", conditionMessage(dat)),
                       type = "error", duration = 8); return()
    }

    env_select  <- resolve_block(input$env_select)
    spec_select <- resolve_block(input$spec_select)
    orient_vec  <- trimws(strsplit(input$link_orient, ",")[[1]])
    orient_vec  <- orient_vec[nzchar(orient_vec)]

    res <- tryCatch(
      ggNetView::gglink_heatmaps(
        env             = dat$env,
        spec            = dat$spec,
        env_select      = env_select,
        spec_select     = spec_select,
        relation_method = input$link_relation,
        cor.method      = input$link_corm,
        spec_layout     = input$link_layout,
        drop_nonsig     = input$link_drop_nonsig,
        r               = input$link_r,
        distance        = input$link_distance,
        orientation     = orient_vec
      ),
      error = function(e) e)

    if (inherits(res, "error")) {
      showNotification(paste("Linkage failed:", conditionMessage(res)),
                       type = "error", duration = 10); return()
    }

    # gglink_heatmaps returns a list of plots
    pl <- if (is.list(res) && !inherits(res, "ggplot")) res[[1]] else res
    state$link_plot <- pl
    showNotification("Linkage rendered.", type = "message", duration = 3)
  })

  output$link_plot <- renderPlot({
    if (is.null(state$link_plot))
      return(ggplot2::ggplot() +
               ggplot2::annotate("text", x = 0, y = 0,
                                 label = "Click 'Render heatmap-link'.") +
               ggplot2::theme_void())
    state$link_plot
  })

  output$link_download_pdf <- downloadHandler(
    filename = function() "ggNetView_envspec.pdf",
    content  = function(file) {
      req(state$link_plot)
      ggplot2::ggsave(file, plot = state$link_plot,
                      width = 12, height = 12, device = "pdf",
                      limitsize = FALSE)
    }
  )
}
