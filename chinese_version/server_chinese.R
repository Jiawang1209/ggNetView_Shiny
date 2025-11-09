server <- function(input, output, session){
  # Microbial Network
  mna_dat   <- uploadBoxServer("up_mna")
  
  # —— 小工具：df -> 数值矩阵（首列若为字符且唯一则当行名）——
  as_numeric_matrix <- function(df, transpose = FALSE) {
    req(df)
    if (ncol(df) >= 2 && !is.numeric(df[[1]])) {
      if (anyDuplicated(df[[1]]) == 0) {
        rn <- as.character(df[[1]])
        df <- df[,-1, drop = FALSE]
        rownames(df) <- rn
      }
    }
    num <- suppressWarnings(as.data.frame(lapply(df, function(x) as.numeric(as.character(x)))))
    mat <- as.matrix(num)
    keep <- colSums(!is.na(mat)) > 0 & colSums(abs(mat), na.rm = TRUE) > 0
    mat  <- mat[, keep, drop = FALSE]
    mat[!is.finite(mat)] <- 0
    if (transpose) mat <- t(mat)
    validate(need(nrow(mat) > 1 && ncol(mat) > 1, "有效数据维度不足。"))
    mat
  }
  
  # —— 构建网络：点击【构建网络】触发 —— 
  mna_graph <- eventReactive(input$mna_build, {
    df <- mna_dat()
    validate(need(!is.null(df), "请先完成上方文件的『读取』，再点『构建网络』。"))
    
    withProgress(message = "构建网络…", value = 0, {
      incProgress(0.25, detail = "预处理数据")
      mat <- as_numeric_matrix(df, transpose = isTRUE(input$mna_transpose))
      
      incProgress(0.6, detail = "计算相关/网络推断")
      # ★ 直接调用你包的 build_graph_from_mat（参数名与取值保持一致）
      gobj <- try({
        ggNetView::build_graph_from_mat(
          mat                = mat,
          transfrom.method   = input$mna_transfrom,   # 注意：就是 transfrom（你的函数签名）
          r.threshold        = input$mna_r,
          p.threshold        = input$mna_p,
          method             = input$mna_method,      # 确保内部分支支持 "SPARCC"
          cor.method         = input$mna_cor_method,
          proc               = input$mna_proc,
          module.method      = input$mna_module,
          SpiecEasi.method   = input$mna_spieceasi,
          node_annotation    = NULL,                  # 以后可接第二个上传入口
          top_modules        = input$mna_topk,
          seed               = 1115
        )
      }, silent = TRUE)
      
      validate(need(!inherits(gobj, "try-error"), paste0("构建失败：", as.character(gobj))))
      incProgress(1)
      gobj  # 这是 tbl_graph（或你的 graph_obj）
    })
  }, ignoreInit = TRUE)
  
  # —— 状态提示 —— 
  output$mna_status <- renderText({
    req(input$mna_build)
    g <- mna_graph()
    if (is.null(g)) "构建中或失败"
    else paste0("图已就绪：", igraph::gorder(g), " 个节点；", igraph::gsize(g), " 条边。")
  })
  
  # —— 使用 ggNetView 绘图：随着右侧控件变化而重绘 —— 
  mna_plot_obj <- reactive({
    g <- mna_graph(); req(g)
    
    # 组装绘图参数（只放你 UI 暴露的那些，其他用默认）
    ggNetView::ggNetView(
      graph_obj       = g,
      layout          = input$mna_layout,
      layout.module   = input$mna_layout_module,
      orientation     = input$mna_orientation,
      angle           = input$mna_angle,
      label           = isTRUE(input$mna_label),
      add_outer       = isTRUE(input$mna_add_outer),
      remove          = isTRUE(input$mna_remove_others),
      mapping_line    = isTRUE(input$mna_mapping_line),
      pointsize       = input$mna_pointsize,
      seed            = 1115
    )
  })
  
  output$mna_plot <- renderPlot({
    p <- mna_plot_obj(); req(p)
    p
  })
  
  # —— 导出：节点、边、PNG —— 
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
      p <- mna_plot_obj(); req(p)
      ggplot2::ggsave(filename = file, plot = p, width = 9, height = 7, dpi = 300, bg = "white")
    }
  )
  output$mna_dl_pdf <- downloadHandler(
    filename = function() sprintf("MNA_plot_%s.pdf", Sys.Date()),
    content  = function(file){
      p <- mna_plot_obj(); req(p)
      # 优先用 cairo_pdf，中文/透明度友好；若 cairo 不可用则退回默认 pdf
      use_cairo <- isTRUE(capabilities("cairo"))
      if (use_cairo) {
        ggplot2::ggsave(
          filename = file, plot = p,
          device   = grDevices::cairo_pdf,  # 矢量、支持透明、中文更稳
          width    = 9, height = 7, bg = "white"
        )
      } else {
        ggplot2::ggsave(
          filename = file, plot = p,
          device   = "pdf",
          width    = 9, height = 7, bg = "white"
        )
      }
    }
  )
  
  
  
  # Protein Network
  ppi_dat   <- uploadBoxServer("up_ppi")
  
  # WGCNA
  wgcna_dat <- uploadBoxServer("up_wgcna")
  
  
  # Env-link
  env_dat   <- uploadBoxServer("up_env")
  
  # 示例：只有在“上传并预览”被点击且成功后，才会触发
  observeEvent(mna_dat(),  { d <- mna_dat();  if(!is.null(d)) cat("[MNA]",  nrow(d), "x", ncol(d), "\n") })
  observeEvent(ppi_dat(),  { d <- ppi_dat();  if(!is.null(d)) cat("[PPI]",  nrow(d), "x", ncol(d), "\n") })
  observeEvent(wgcna_dat(),{ d <- wgcna_dat();if(!is.null(d)) cat("[WGCNA]",nrow(d), "x", ncol(d), "\n") })
  observeEvent(env_dat(),  { d <- env_dat();  if(!is.null(d)) cat("[Env ]", nrow(d), "x", ncol(d), "\n") })
}
