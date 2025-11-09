server <- function(input, output, session){
  
  # ---- Upload modules (reactive data.frames) ----
  mna_dat   <- uploadBoxServer("up_mna")
  ppi_dat   <- uploadBoxServer("up_ppi")
  wgcna_dat <- uploadBoxServer("up_wgcna")
  env_dat   <- uploadBoxServer("up_env")
  
  # ---- Helper: df -> numeric matrix (samples in rows, variables in columns) ----
  as_numeric_matrix <- function(df, transpose = FALSE) {
    req(df)
    # If the first column is non-numeric and unique, use it as rownames
    if (ncol(df) >= 2 && !is.numeric(df[[1]])) {
      if (anyDuplicated(df[[1]]) == 0) {
        rn <- as.character(df[[1]])
        df <- df[,-1, drop = FALSE]
        rownames(df) <- rn
      }
    }
    num <- suppressWarnings(as.data.frame(lapply(df, function(x) as.numeric(as.character(x)))))
    mat <- as.matrix(num)
    # Drop columns that are all NA or all zeros
    keep <- colSums(!is.na(mat)) > 0 & colSums(abs(mat), na.rm = TRUE) > 0
    mat  <- mat[, keep, drop = FALSE]
    # Replace non-finite with 0
    mat[!is.finite(mat)] <- 0
    if (transpose) mat <- t(mat)
    validate(need(nrow(mat) > 1 && ncol(mat) > 1, "Insufficient valid data dimensions."))
    mat
  }
  
  # ---------------------------------------------------------------------------
  # Microbial Network (MNA): Build network (button-triggered)
  # ---------------------------------------------------------------------------
  
  # Initial UI states for plotting section
  shinyjs::disable("mna_do_plot")
  output$mna_plot_status <- renderText("Waiting for graph. Build the network first.")
  
  mna_graph <- eventReactive(input$mna_build, {
    df <- mna_dat()
    validate(need(!is.null(df), "Please complete the file 'Upload' step above before building the network."))
    
    withProgress(message = "Building network...", value = 0, {
      incProgress(0.25, detail = "Preprocessing data")
      mat <- as_numeric_matrix(df, transpose = isTRUE(input$mna_transpose))
      
      incProgress(0.6, detail = "Correlation / network inference")
      # Direct call to your ggNetView API. Make sure build_graph_from_mat supports 'SPARCC' internally.
      gobj <- try({
        ggNetView::build_graph_from_mat(
          mat                = mat,
          transfrom.method   = input$mna_transfrom,   # note: 'transfrom' per your function signature
          r.threshold        = input$mna_r,
          p.threshold        = input$mna_p,
          method             = input$mna_method,
          cor.method         = input$mna_cor_method,
          proc               = input$mna_proc,
          module.method      = input$mna_module,
          SpiecEasi.method   = input$mna_spieceasi,
          node_annotation    = NULL,                  # hook a second uploader here if needed
          top_modules        = input$mna_topk,
          seed               = 1115
        )
      }, silent = TRUE)
      
      validate(need(!inherits(gobj, "try-error"), paste0("Network construction failed: ", as.character(gobj))))
      incProgress(1)
      gobj
    })
  }, ignoreInit = TRUE)
  
  # Build status
  output$mna_status <- renderText({
    req(input$mna_build)
    g <- mna_graph()
    if (is.null(g)) "Building or failed."
    else paste0("Graph ready: ", igraph::gorder(g), " nodes; ", igraph::gsize(g), " edges.")
  })
  
  # After graph is ready, enable Plot button and prompt user to click it
  observeEvent(mna_graph(), {
    req(mna_graph())
    shinyjs::enable("mna_do_plot")
    output$mna_plot_status <- renderText("Graph ready. Click 'Plot' to render.")
    # Optionally clear old plot:
    # output$mna_plot <- renderPlot(NULL)
  })
  
  # Any visualization parameter change: do NOT auto-redraw; just prompt to click Plot
  observeEvent(
    list(input$mna_layout, input$mna_layout_module, input$mna_orientation,
         input$mna_angle, input$mna_label, input$mna_add_outer,
         input$mna_remove_others, input$mna_mapping_line, input$mna_pointsize),
    {
      if (!is.null(mna_graph())) {
        output$mna_plot_status <- renderText("Parameters changed. Click 'Plot' to update.")
      }
    },
    ignoreInit = TRUE
  )
  
  # Plot (button-triggered)
  mna_plot_obj <- eventReactive(input$mna_do_plot, {
    g <- mna_graph()
    validate(need(!is.null(g), "Build the network first."))
    
    withProgress(message = "Rendering plot...", value = 0, {
      incProgress(0.4, detail = "Laying out modules")
      p <- ggNetView::ggNetView(
        graph_obj     = g,
        layout        = input$mna_layout,
        layout.module = input$mna_layout_module,
        orientation   = input$mna_orientation,
        angle         = input$mna_angle,
        label         = isTRUE(input$mna_label),
        add_outer     = isTRUE(input$mna_add_outer),
        remove        = isTRUE(input$mna_remove_others),
        mapping_line  = isTRUE(input$mna_mapping_line),
        pointsize     = input$mna_pointsize,
        seed          = 1115
      )
      incProgress(1)
      p
    })
  }, ignoreInit = TRUE)
  
  output$mna_plot <- renderPlot({
    p <- mna_plot_obj(); req(p)
    output$mna_plot_status <- renderText("Plot rendered.")
    p
  })
  
  # ---------------------------------------------------------------------------
  # Downloads (MNA)
  # ---------------------------------------------------------------------------
  
  output$mna_dl_nodes <- downloadHandler(
    filename = function() sprintf("MNA_nodes_%s.csv", Sys.Date()),
    content  = function(file){
      g <- mna_graph(); req(g)
      v <- igraph::as_data_frame(g, what = "vertices")
      readr::write_csv(v, file)
    }
  )
  
  output$mna_dl_edges <- downloadHandler(
    filename = function() sprintf("MNA_edges_%s.csv", Sys.Date()),
    content  = function(file){
      g <- mna_graph(); req(g)
      e <- igraph::as_data_frame(g, what = "edges")
      readr::write_csv(e, file)
    }
  )
  
  output$mna_dl_png <- downloadHandler(
    filename = function() sprintf("MNA_plot_%s.png", Sys.Date()),
    content  = function(file){
      p <- mna_plot_obj()
      validate(need(!is.null(p), "Please click 'Plot' before downloading."))
      ggplot2::ggsave(filename = file, plot = p, width = 9, height = 7, dpi = 300, bg = "white")
    }
  )
  
  output$mna_dl_pdf <- downloadHandler(
    filename = function() sprintf("MNA_plot_%s.pdf", Sys.Date()),
    content  = function(file){
      p <- mna_plot_obj()
      validate(need(!is.null(p), "Please click 'Plot' before downloading."))
      use_cairo <- isTRUE(capabilities("cairo"))
      ggplot2::ggsave(
        filename = file, plot = p,
        device   = if (use_cairo) grDevices::cairo_pdf else "pdf",
        width    = 9, height = 7, bg = "white"
      )
    }
  )
  
  # ---------------------------------------------------------------------------
  # Debug echoes for other tabs (optional)
  # ---------------------------------------------------------------------------
  observeEvent(mna_dat(),   { d <- mna_dat();   if(!is.null(d)) cat("[MNA]   ", nrow(d), "x", ncol(d), "\n") })
  observeEvent(ppi_dat(),   { d <- ppi_dat();   if(!is.null(d)) cat("[PPI]   ", nrow(d), "x", ncol(d), "\n") })
  observeEvent(wgcna_dat(), { d <- wgcna_dat(); if(!is.null(d)) cat("[WGCNA] ", nrow(d), "x", ncol(d), "\n") })
  observeEvent(env_dat(),   { d <- env_dat();   if(!is.null(d)) cat("[Env]   ", nrow(d), "x", ncol(d), "\n") })
}
