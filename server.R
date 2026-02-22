load_rdata_file <- function(path) {
  e <- new.env(parent = emptyenv())
  obj_names <- load(path, envir = e)
  if (length(obj_names) == 0) {
    stop("RData file is empty.", call. = FALSE)
  }
  objs <- mget(obj_names, envir = e)
  if (length(objs) == 1) {
    return(objs[[1]])
  }
  objs
}

normalize_loaded_object <- function(x) {
  # If .rda contains multiple objects, prefer common matrix-like names.
  if (is.list(x) && !is.data.frame(x) && !is.matrix(x)) {
    nms <- names(x)
    preferred <- intersect(c("otu_tab", "otu_rare", "otu_rare_relative", "tax_tab", "Envdf_4st", "Spedf"), nms)
    if (length(preferred) > 0) {
      return(x[[preferred[1]]])
    }
    return(x[[1]])
  }
  x
}

set_first_col_as_rownames <- function(x) {
  if (!is.data.frame(x) || ncol(x) < 2) {
    return(x)
  }
  id_col <- as.character(x[[1]])
  if (anyNA(id_col) || any(!nzchar(id_col)) || anyDuplicated(id_col)) {
    return(x)
  }
  out <- x[, -1, drop = FALSE]
  rownames(out) <- id_col
  out
}

read_uploaded_data <- function(file_input, first_col_as_rownames = FALSE) {
  if (is.null(file_input)) {
    stop("Please upload a file.", call. = FALSE)
  }
  ext <- tolower(tools::file_ext(file_input$name))
  if (ext %in% c("csv")) {
    out <- readr::read_csv(file_input$datapath, show_col_types = FALSE)
    if (isTRUE(first_col_as_rownames)) {
      out <- set_first_col_as_rownames(as.data.frame(out))
    }
    return(out)
  }
  if (ext %in% c("tsv", "txt")) {
    out <- readr::read_tsv(file_input$datapath, show_col_types = FALSE)
    if (isTRUE(first_col_as_rownames)) {
      out <- set_first_col_as_rownames(as.data.frame(out))
    }
    return(out)
  }
  if (ext == "rds") {
    return(readRDS(file_input$datapath))
  }
  if (ext %in% c("rda", "rdata")) {
    return(load_rdata_file(file_input$datapath))
  }
  stop("Unsupported file format. Please upload csv/tsv/txt/rds/rda.", call. = FALSE)
}

load_demo_dataset <- function(name, fallback = NULL) {
  candidate_paths <- c(
    file.path("data", paste0(name, ".rda")),
    file.path("inst", "extdata", paste0(name, ".rda"))
  )
  for (p in candidate_paths) {
    if (file.exists(p)) {
      return(load_rdata_file(p))
    }
  }

  pkg_data <- tryCatch(
    utils::data(list = name, package = "ggNetView", envir = environment()),
    error = function(e) NULL
  )
  if (!is.null(pkg_data) && exists(name, inherits = FALSE)) {
    return(get(name, inherits = FALSE))
  }

  if (!is.null(fallback)) {
    return(fallback)
  }
  stop(sprintf("Demo dataset `%s` is not available.", name), call. = FALSE)
}

coerce_matrix <- function(x) {
  if (is.matrix(x)) {
    return(x)
  }
  if (is.data.frame(x)) {
    x_df <- as.data.frame(x)
    # If row names already exist and are unique, keep them.
    rn <- rownames(x_df)
    if (!is.null(rn) && length(rn) == nrow(x_df) && !anyDuplicated(rn)) {
      num_df <- x_df[, vapply(x_df, is.numeric, logical(1)), drop = FALSE]
      if (ncol(num_df) > 0) {
        rownames(num_df) <- rn
        return(as.matrix(num_df))
      }
    }

    id_candidates <- c("id", "otu", "otuid", "asv", "feature", "name", "taxa")
    nms_lower <- tolower(colnames(x_df))
    id_idx <- which(nms_lower %in% id_candidates)
    if (length(id_idx) > 0) {
      id_col <- x_df[[id_idx[1]]]
      num_df <- x_df[, setdiff(seq_len(ncol(x_df)), id_idx[1]), drop = FALSE]
      if (all(vapply(num_df, is.numeric, logical(1)))) {
        id_chr <- as.character(id_col)
        if (!anyDuplicated(id_chr)) {
          rownames(num_df) <- id_chr
          return(as.matrix(num_df))
        }
      }
    }

    # Fallback: if first column looks like ID and is unique, use it.
    first_col <- x_df[[1]]
    other_cols <- x_df[-1]
    if ((is.character(first_col) || is.factor(first_col)) &&
      all(vapply(other_cols, is.numeric, logical(1))) &&
      !anyDuplicated(as.character(first_col))) {
      rownames(other_cols) <- as.character(first_col)
      return(as.matrix(other_cols))
    }

    # Pure numeric table -> direct matrix conversion.
    if (all(vapply(x_df, is.numeric, logical(1)))) {
      return(as.matrix(x_df))
    }
  }
  stop(
    "Cannot convert input to numeric matrix. Please provide a numeric matrix or a table with one unique ID column and numeric value columns.",
    call. = FALSE
  )
}

coerce_edge_df <- function(df) {
  d <- as.data.frame(df)
  nms <- tolower(colnames(d))
  if (all(c("from", "to") %in% nms)) {
    colnames(d)[match("from", nms)] <- "from"
    colnames(d)[match("to", nms)] <- "to"
  } else if (all(c("source", "target") %in% nms)) {
    colnames(d)[match("source", nms)] <- "from"
    colnames(d)[match("target", nms)] <- "to"
  } else {
    if (ncol(d) < 2) {
      stop("Edge table needs at least two columns.", call. = FALSE)
    }
    colnames(d)[1:2] <- c("from", "to")
  }
  if (!"weight" %in% colnames(d)) {
    d$weight <- 1
  }
  d
}

coerce_node_annotation <- function(df) {
  if (is.null(df)) {
    return(NULL)
  }
  out <- as.data.frame(df)
  if (ncol(out) < 1) {
    stop("Node annotation needs at least one column.", call. = FALSE)
  }
  out
}

build_graph_summary <- function(graph_obj) {
  g <- tidygraph::as.igraph(graph_obj)
  data.frame(
    Metric = c("Nodes", "Edges", "Components", "Average degree"),
    Value = c(
      igraph::gorder(g),
      igraph::gsize(g),
      igraph::components(g)$no,
      round(mean(igraph::degree(g)), 3)
    ),
    stringsAsFactors = FALSE
  )
}

plot_graph_with_controls <- function(graph_obj, prefix, input) {
  args <- list(
    graph_obj = graph_obj,
    layout = input[[paste0(prefix, "_layout")]],
    layout.module = input[[paste0(prefix, "_layout_module")]],
    group.by = input[[paste0(prefix, "_group_by")]],
    fill.by = input[[paste0(prefix, "_fill_by")]],
    pointsize = c(input[[paste0(prefix, "_point_min")]], input[[paste0(prefix, "_point_max")]]),
    plot_line = isTRUE(input[[paste0(prefix, "_plot_line")]]),
    linealpha = input[[paste0(prefix, "_line_alpha")]],
    linecolor = input[[paste0(prefix, "_line_color")]],
    label = isTRUE(input[[paste0(prefix, "_label")]]),
    add_outer = isTRUE(input[[paste0(prefix, "_add_outer")]]),
    seed = as.integer(input[[paste0(prefix, "_seed")]])
  )
  do.call(ggNetView, args)
}

plotly_from_gg <- function(p) {
  tryCatch(
    plotly::ggplotly(p),
    error = function(e) {
      plotly::plotly_empty(type = "scatter", mode = "markers") %>%
        plotly::layout(title = paste("ggplotly conversion warning:", e$message))
    }
  )
}

demo_matrix <- function(features = 60, samples = 18, seed = 1115) {
  set.seed(seed)
  mat <- matrix(rpois(features * samples, lambda = 80), nrow = features, ncol = samples)
  rownames(mat) <- paste0("ASV_", seq_len(features))
  colnames(mat) <- paste0("Sample_", seq_len(samples))
  mat
}

demo_annotation_from_matrix <- function(mat) {
  data.frame(
    ID = rownames(mat),
    Group = sample(c("A", "B", "C", "D"), size = nrow(mat), replace = TRUE),
    stringsAsFactors = FALSE
  )
}

demo_edge_df <- function() {
  p <- "inst/extdata/PPI_links.csv"
  if (!file.exists(p)) {
    set.seed(1115)
    return(data.frame(
      from = sample(paste0("P", 1:50), 240, replace = TRUE),
      to = sample(paste0("P", 1:50), 240, replace = TRUE),
      weight = runif(240, -1, 1)
    ))
  }
  d <- readr::read_csv(p, show_col_types = FALSE)
  d <- coerce_edge_df(d)
  if (!"weight" %in% colnames(d) || all(d$weight == 1)) {
    set.seed(1115)
    d$weight <- runif(nrow(d), -1, 1)
  }
  d
}

demo_node_df <- function() {
  p <- "inst/extdata/PPI_nodes.csv"
  if (!file.exists(p)) {
    return(NULL)
  }
  readr::read_csv(p, show_col_types = FALSE)
}

demo_wgcna <- function() {
  d <- demo_edge_df()
  nodes <- sort(unique(c(as.character(d$from), as.character(d$to))))
  module <- data.frame(
    ID = nodes,
    Module = sample(paste0("M", 1:6), length(nodes), replace = TRUE),
    stringsAsFactors = FALSE
  )
  list(edge = d, module = module)
}

demo_fallback_by_name <- function(name) {
  if (identical(name, "otu_tab")) return(demo_matrix())
  if (identical(name, "otu_rare")) return(demo_matrix(features = 50, samples = 15, seed = 222))
  if (identical(name, "otu_rare_relative")) {
    mat <- demo_matrix(features = 50, samples = 15, seed = 333)
    return(sweep(mat, 2, colSums(mat), FUN = "/"))
  }
  if (identical(name, "tax_tab")) {
    mat <- demo_matrix(features = 40, samples = 10, seed = 444)
    return(demo_annotation_from_matrix(mat))
  }
  if (identical(name, "Envdf_4st")) {
    out <- as.data.frame(matrix(rnorm(20 * 24), nrow = 20, ncol = 24))
    colnames(out) <- paste0("Env_", seq_len(ncol(out)))
    return(out)
  }
  if (identical(name, "Spedf")) {
    out <- as.data.frame(matrix(abs(rnorm(20 * 18)), nrow = 20, ncol = 18))
    colnames(out) <- paste0("Spec_", seq_len(ncol(out)))
    return(out)
  }
  NULL
}

preview_to_table <- function(x, max_rows = 200) {
  if (is.null(x)) {
    return(data.frame(Message = "No data loaded yet."))
  }
  if (is.matrix(x)) {
    x <- as.data.frame(x)
  }
  if (is.data.frame(x)) {
    return(utils::head(x, max_rows))
  }
  if (is.list(x)) {
    if (length(x) == 1) {
      obj <- x[[1]]
      if (is.matrix(obj)) obj <- as.data.frame(obj)
      if (is.data.frame(obj)) {
        return(utils::head(obj, max_rows))
      }
      return(data.frame(Message = paste("Unsupported:", paste(class(obj), collapse = ", "))))
    }
    parts <- lapply(names(x), function(nm) {
      obj <- x[[nm]]
      if (is.matrix(obj)) obj <- as.data.frame(obj)
      if (!is.data.frame(obj)) {
        return(data.frame(.Dataset = nm, Message = paste("Unsupported:", paste(class(obj), collapse = ", "))))
      }
      d <- utils::head(obj, max_rows)
      d$.Dataset <- nm
      d
    })
    out <- dplyr::bind_rows(parts)
    if (".Dataset" %in% colnames(out)) {
      out <- out[, c(".Dataset", setdiff(colnames(out), ".Dataset")), drop = FALSE]
    }
    return(out)
  }
  data.frame(Message = paste("Unsupported object:", paste(class(x), collapse = ", ")))
}

preview_info_text <- function(x) {
  if (is.null(x)) {
    return("No data loaded yet.")
  }
  if (is.matrix(x)) {
    return(sprintf("matrix: %d x %d", nrow(x), ncol(x)))
  }
  if (is.data.frame(x)) {
    return(sprintf("data.frame: %d x %d", nrow(x), ncol(x)))
  }
  if (is.list(x)) {
    lines <- unlist(lapply(names(x), function(nm) {
      obj <- x[[nm]]
      if (is.matrix(obj)) {
        sprintf("%s -> matrix: %d x %d", nm, nrow(obj), ncol(obj))
      } else if (is.data.frame(obj)) {
        sprintf("%s -> data.frame: %d x %d", nm, nrow(obj), ncol(obj))
      } else if (is.null(obj)) {
        sprintf("%s -> NULL", nm)
      } else {
        sprintf("%s -> %s", nm, paste(class(obj), collapse = ", "))
      }
    }))
    return(paste(lines, collapse = "\n"))
  }
  paste("Object class:", paste(class(x), collapse = ", "))
}

split_preview_data <- function(x) {
  if (is.null(x)) {
    return(list(main = data.frame(Message = "No data loaded yet."), aux = data.frame(Message = "No annotation/additional data loaded yet.")))
  }
  if (is.list(x) && !is.data.frame(x) && !is.matrix(x)) {
    nms <- names(x)
    if (length(nms) == 0) {
      nms <- paste0("dataset_", seq_along(x))
    }
    main_idx <- 1
    aux_idx <- setdiff(seq_along(x), main_idx)
    main_obj <- setNames(list(x[[main_idx]]), nms[main_idx])
    aux_obj <- if (length(aux_idx) > 0) setNames(x[aux_idx], nms[aux_idx]) else NULL
    main_df <- preview_to_table(main_obj)
    aux_df <- if (is.null(aux_obj)) data.frame(Message = "No annotation/additional data loaded yet.") else preview_to_table(aux_obj)
    return(list(main = main_df, aux = aux_df))
  }
  list(main = preview_to_table(x), aux = data.frame(Message = "No annotation/additional data loaded yet."))
}

assign_network_outputs <- function(input, output, session, prefix, graph_reactive, data_loader) {
  plotly_obj <- reactiveVal(NULL)
  plot_obj <- reactiveVal(NULL)
  data_preview_obj <- reactiveVal(NULL)
  stat_obj <- reactiveVal(NULL)
  build_note_id <- paste0(prefix, "_build_note")

  observeEvent(input[[paste0(prefix, "_preview_data")]], {
    out <- tryCatch(data_loader(), error = function(e) {
      showNotification(e$message, type = "error", duration = 8)
      NULL
    })
    data_preview_obj(out)
  })

  observeEvent(graph_reactive(), {
    res <- graph_reactive()
    if (!is.null(res) && !is.null(res$data_preview)) {
      data_preview_obj(res$data_preview)
    }
    plot_obj(NULL)
    plotly_obj(NULL)
    stat_obj(NULL)
    shiny::removeNotification(id = build_note_id)
    if (!is.null(res)) {
      showNotification("graph_obj built successfully. Click `Compute graph_obj Statistics` when needed.", type = "message", duration = 4)
    }
  })

  observeEvent(input[[paste0(prefix, "_build_graph")]], {
    showNotification("Building graph_obj, please wait...", type = "warning", duration = NULL, id = build_note_id)
  })

  output[[paste0(prefix, "_data_preview_main")]] <- DT::renderDT({
    x <- split_preview_data(data_preview_obj())$main
    DT::datatable(x, options = list(pageLength = 8, scrollX = TRUE))
  })
  output[[paste0(prefix, "_data_preview_aux")]] <- DT::renderDT({
    x <- split_preview_data(data_preview_obj())$aux
    DT::datatable(x, options = list(pageLength = 8, scrollX = TRUE))
  })
  output[[paste0(prefix, "_data_info")]] <- renderText({
    preview_info_text(data_preview_obj())
  })

  output[[paste0(prefix, "_plot")]] <- renderPlot({
    p <- plot_obj()
    validate(need(!is.null(p), "Click `Visualize Graph` after graph_obj is built."))
    p
  },
  width = function() {
    w <- input[[paste0(prefix, "_plot_width")]]
    if (is.null(w) || !is.numeric(w) || is.na(w)) 900 else w
  },
  height = function() {
    h <- input[[paste0(prefix, "_plot_height")]]
    if (is.null(h) || !is.numeric(h) || is.na(h)) 650 else h
  })

  observeEvent(input[[paste0(prefix, "_plot_graph")]], {
    res <- graph_reactive()
    req(res)
    p <- plot_graph_with_controls(res$graph_obj, prefix, input)
    plot_obj(p)
    plotly_obj(NULL)
  })

  observeEvent(input[[paste0(prefix, "_build_plotly")]], {
    p <- plot_obj()
    req(p)
    plotly_obj(plotly_from_gg(p))
  })

  output[[paste0(prefix, "_plotly")]] <- plotly::renderPlotly({
    p <- plotly_obj()
    validate(need(!is.null(p), "Click `Generate Plotly` to create interactive plot."))
    p
  })

  observeEvent(input[[paste0(prefix, "_compute_stat")]], {
    res <- graph_reactive()
    req(res)
    if (!isTRUE(input[[paste0(prefix, "_enable_stat")]])) {
      showNotification("Statistics are disabled by parameter. Enable and try again.", type = "warning", duration = 4)
      return(invisible(NULL))
    }
    withProgress(message = "Computing graph_obj statistics...", value = 0.1, {
      summary_df <- build_graph_summary(res$graph_obj)
      incProgress(0.5)
      module_df <- tryCatch({
        get_subgraph(res$graph_obj)$stat_module
      }, error = function(e) {
        data.frame(Message = e$message)
      })
      incProgress(0.4)
      stat_obj(list(summary = summary_df, module = module_df))
    })
  })

  output[[paste0(prefix, "_summary")]] <- DT::renderDT({
    s <- stat_obj()
    if (is.null(s)) {
      return(DT::datatable(data.frame(Message = "Click `Compute graph_obj Statistics` after graph_obj is built."), options = list(dom = "tip")))
    }
    DT::datatable(s$summary, options = list(pageLength = 5, dom = "tip"))
  })

  output[[paste0(prefix, "_module_stat")]] <- DT::renderDT({
    s <- stat_obj()
    if (is.null(s)) {
      return(DT::datatable(data.frame(Message = "Click `Compute graph_obj Statistics` after graph_obj is built."), options = list(dom = "tip")))
    }
    DT::datatable(s$module, options = list(pageLength = 8, dom = "tip"))
  })

  output[[paste0(prefix, "_save_pdf")]] <- downloadHandler(
    filename = function() {
      paste0(prefix, "_plot_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".pdf")
    },
    content = function(file) {
      p <- plot_obj()
      req(p)
      w <- input[[paste0(prefix, "_plot_width")]]
      h <- input[[paste0(prefix, "_plot_height")]]
      ggplot2::ggsave(
        filename = file,
        plot = p,
        width = as.numeric(w) / 96,
        height = as.numeric(h) / 96,
        units = "in",
        device = "pdf"
      )
    }
  )

  output[[paste0(prefix, "_save_png")]] <- downloadHandler(
    filename = function() {
      paste0(prefix, "_plot_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")
    },
    content = function(file) {
      p <- plot_obj()
      req(p)
      w <- input[[paste0(prefix, "_plot_width")]]
      h <- input[[paste0(prefix, "_plot_height")]]
      ggplot2::ggsave(
        filename = file,
        plot = p,
        width = as.numeric(w) / 96,
        height = as.numeric(h) / 96,
        units = "in",
        dpi = 300,
        device = "png"
      )
    }
  )
}

server <- function(input, output, session) {
  app_env <- environment()

  sample_dataset_obj <- eventReactive(input$sample_dataset_refresh, {
    normalize_loaded_object(load_demo_dataset(
      input$sample_dataset_name,
      fallback = demo_fallback_by_name(input$sample_dataset_name)
    ))
  }, ignoreNULL = FALSE)

  output$sample_dataset_info <- renderText({
    x <- sample_dataset_obj()
    req(x)
    dims <- if (is.matrix(x) || is.data.frame(x)) {
      paste0(nrow(as.data.frame(x)), " x ", ncol(as.data.frame(x)))
    } else {
      paste(class(x), collapse = ", ")
    }
    paste0(
      "Dataset: ", input$sample_dataset_name,
      "\nClass: ", paste(class(x), collapse = ", "),
      "\nShape: ", dims
    )
  })

  output$sample_dataset_table <- DT::renderDT({
    x <- sample_dataset_obj()
    req(x)
    if (is.matrix(x)) {
      x <- as.data.frame(x)
    }
    if (!is.data.frame(x)) {
      return(DT::datatable(data.frame(Message = "当前对象不是表格/矩阵，无法直接预览"), options = list(dom = "tip")))
    }
    show_df <- utils::head(x, 200)
    DT::datatable(show_df, options = list(pageLength = 10, scrollX = TRUE))
  })

  output$sample_dataset_download <- downloadHandler(
    filename = function() {
      fmt <- input$sample_download_format
      if (is.null(fmt) || !nzchar(fmt)) fmt <- "csv"
      paste0(input$sample_dataset_name, ".", fmt)
    },
    content = function(file) {
      x <- sample_dataset_obj()
      req(x)
      fmt <- input$sample_download_format
      if (is.null(fmt) || !nzchar(fmt)) fmt <- "csv"
      keep_row_names <- !identical(input$sample_dataset_name, "tax_tab")
      export_x <- x
      if (!keep_row_names && (is.data.frame(export_x) || is.matrix(export_x))) {
        rownames(export_x) <- NULL
      }
      if (identical(fmt, "rds")) {
        saveRDS(export_x, file = file)
        return(invisible(NULL))
      }
      if (is.matrix(export_x)) {
        export_x <- as.data.frame(export_x)
      }
      if (!is.data.frame(export_x)) {
        stop("Selected dataset is not a table/matrix and cannot be exported as csv/txt/xlsx/rds.", call. = FALSE)
      }
      if (identical(fmt, "txt")) {
        utils::write.table(export_x, file = file, sep = "\t", quote = FALSE, row.names = keep_row_names, col.names = TRUE)
      } else if (identical(fmt, "xlsx")) {
        if (!requireNamespace("openxlsx", quietly = TRUE)) {
          stop("To export XLSX, please install package `openxlsx`.", call. = FALSE)
        }
        wb <- openxlsx::createWorkbook()
        openxlsx::addWorksheet(wb, "data")
        openxlsx::writeData(wb, sheet = "data", x = export_x, rowNames = keep_row_names)
        openxlsx::saveWorkbook(wb, file = file, overwrite = TRUE)
      } else {
        utils::write.csv(export_x, file = file, row.names = keep_row_names)
      }
    }
  )

  micro_mat_data_loader <- function() {
    mat <- if (identical(input$micro_mat_data_mode, "Demo")) {
      normalize_loaded_object(load_demo_dataset("otu_tab", fallback = demo_matrix()))
    } else {
      normalize_loaded_object(read_uploaded_data(input$micro_mat_file, first_col_as_rownames = TRUE))
    }
    mat <- coerce_matrix(mat)
    anno <- if (is.null(input$micro_mat_anno)) {
      tryCatch(
        coerce_node_annotation(normalize_loaded_object(load_demo_dataset("tax_tab", fallback = demo_annotation_from_matrix(mat)))),
        error = function(e) demo_annotation_from_matrix(mat)
      )
    } else {
      coerce_node_annotation(normalize_loaded_object(read_uploaded_data(input$micro_mat_anno, first_col_as_rownames = TRUE)))
    }
    list(mat = mat, anno = anno)
  }

  micro_mat_res <- eventReactive(input$micro_mat_build_graph, {
    tryCatch({
      dat <- micro_mat_data_loader()
      graph_obj <- build_graph_from_mat(
        mat = dat$mat,
        transfrom.method = input$micro_mat_transform,
        r.threshold = input$micro_mat_r_threshold,
        p.threshold = input$micro_mat_p_threshold,
        method = input$micro_mat_method,
        cor.method = input$micro_mat_cor,
        module.method = input$micro_mat_module_method,
        node_annotation = dat$anno,
        top_modules = input$micro_mat_top_modules,
        seed = input$micro_mat_seed
      )
      list(graph_obj = graph_obj, data_preview = list(matrix = dat$mat, annotation = dat$anno))
    }, error = function(e) {
      showNotification(e$message, type = "error", duration = 8)
      NULL
    })
  })
  assign_network_outputs(input, output, session, "micro_mat", micro_mat_res, micro_mat_data_loader)

  micro_df_data_loader <- function() {
    df <- if (identical(input$micro_df_data_mode, "Demo")) demo_edge_df() else coerce_edge_df(normalize_loaded_object(read_uploaded_data(input$micro_df_file)))
    anno <- if (is.null(input$micro_df_anno)) demo_node_df() else coerce_node_annotation(normalize_loaded_object(read_uploaded_data(input$micro_df_anno)))
    list(df = df, anno = anno)
  }

  micro_df_res <- eventReactive(input$micro_df_build_graph, {
    tryCatch({
      dat <- micro_df_data_loader()
      graph_obj <- build_graph_from_df(
        df = dat$df,
        node_annotation = dat$anno,
        directed = isTRUE(input$micro_df_directed),
        module.method = input$micro_df_module_method,
        top_modules = input$micro_df_top_modules,
        seed = input$micro_df_seed
      )
      list(graph_obj = graph_obj, data_preview = list(edge_df = dat$df, annotation = dat$anno))
    }, error = function(e) {
      showNotification(e$message, type = "error", duration = 8)
      NULL
    })
  })
  assign_network_outputs(input, output, session, "micro_df", micro_df_res, micro_df_data_loader)

  micro_adj_data_loader <- function() {
    adj <- if (identical(input$micro_adj_data_mode, "Demo")) {
      mat <- coerce_matrix(normalize_loaded_object(load_demo_dataset("otu_tab", fallback = demo_matrix(features = 45, samples = 20))))
      stats::cor(t(mat))
    } else {
      coerce_matrix(normalize_loaded_object(read_uploaded_data(input$micro_adj_file)))
    }
    anno <- if (is.null(input$micro_adj_anno)) NULL else coerce_node_annotation(normalize_loaded_object(read_uploaded_data(input$micro_adj_anno)))
    list(adj = adj, anno = anno)
  }

  micro_adj_res <- eventReactive(input$micro_adj_build_graph, {
    tryCatch({
      dat <- micro_adj_data_loader()
      graph_obj <- build_graph_from_adj_mat(
        adjacency_matrix = dat$adj,
        module.method = input$micro_adj_module_method,
        node_annotation = dat$anno,
        top_modules = input$micro_adj_top_modules,
        seed = input$micro_adj_seed
      )
      list(graph_obj = graph_obj, data_preview = list(adjacency_matrix = dat$adj, annotation = dat$anno))
    }, error = function(e) {
      showNotification(e$message, type = "error", duration = 8)
      NULL
    })
  })
  assign_network_outputs(input, output, session, "micro_adj", micro_adj_res, micro_adj_data_loader)

  micro_double_data_loader <- function() {
    if (identical(input$micro_double_data_mode, "Demo")) {
      mat1 <- coerce_matrix(normalize_loaded_object(load_demo_dataset("otu_tab", fallback = demo_matrix(features = 40, samples = 20, seed = 1115))))
      mat2 <- demo_matrix(features = 32, samples = 20, seed = 1234)
    } else {
      mat1 <- coerce_matrix(normalize_loaded_object(read_uploaded_data(input$micro_double_mat1)))
      mat2 <- coerce_matrix(normalize_loaded_object(read_uploaded_data(input$micro_double_mat2)))
    }
    anno <- if (is.null(input$micro_double_anno)) NULL else coerce_node_annotation(normalize_loaded_object(read_uploaded_data(input$micro_double_anno)))
    list(mat1 = mat1, mat2 = mat2, anno = anno)
  }

  micro_double_res <- eventReactive(input$micro_double_build_graph, {
    tryCatch({
      dat <- micro_double_data_loader()
      graph_obj <- build_graph_from_double_mat(
        mat1 = dat$mat1,
        mat2 = dat$mat2,
        node_annotation = dat$anno,
        directed = isTRUE(input$micro_double_directed),
        module.method = input$micro_double_module_method,
        top_modules = input$micro_double_top_modules,
        seed = input$micro_double_seed
      )
      list(graph_obj = graph_obj, data_preview = list(mat1 = dat$mat1, mat2 = dat$mat2, annotation = dat$anno))
    }, error = function(e) {
      showNotification(e$message, type = "error", duration = 8)
      NULL
    })
  })
  assign_network_outputs(input, output, session, "micro_double", micro_double_res, micro_double_data_loader)

  micro_wgcna_data_loader <- function() {
    demo_obj <- demo_wgcna()
    edge_df <- if (identical(input$micro_wgcna_data_mode, "Demo")) demo_obj$edge else coerce_edge_df(normalize_loaded_object(read_uploaded_data(input$micro_wgcna_edge)))
    module_df <- if (identical(input$micro_wgcna_data_mode, "Demo")) demo_obj$module else as.data.frame(normalize_loaded_object(read_uploaded_data(input$micro_wgcna_module)))
    anno <- if (is.null(input$micro_wgcna_anno)) NULL else coerce_node_annotation(normalize_loaded_object(read_uploaded_data(input$micro_wgcna_anno)))
    list(edge_df = edge_df, module_df = module_df, anno = anno)
  }

  micro_wgcna_res <- eventReactive(input$micro_wgcna_build_graph, {
    tryCatch({
      dat <- micro_wgcna_data_loader()
      graph_obj <- build_graph_from_wgcna(
        wgcna_tom = dat$edge_df,
        module = dat$module_df,
        node_annotation = dat$anno,
        directed = isTRUE(input$micro_wgcna_directed),
        seed = input$micro_wgcna_seed
      )
      list(graph_obj = graph_obj, data_preview = list(wgcna_edge = dat$edge_df, module = dat$module_df, annotation = dat$anno))
    }, error = function(e) {
      showNotification(e$message, type = "error", duration = 8)
      NULL
    })
  })
  assign_network_outputs(input, output, session, "micro_wgcna", micro_wgcna_res, micro_wgcna_data_loader)

  micro_module_data_loader <- function() {
    demo_obj <- demo_wgcna()
    edge_df <- if (identical(input$micro_module_data_mode, "Demo")) demo_obj$edge else coerce_edge_df(normalize_loaded_object(read_uploaded_data(input$micro_module_edge)))
    anno <- if (identical(input$micro_module_data_mode, "Demo")) demo_obj$module else coerce_node_annotation(normalize_loaded_object(read_uploaded_data(input$micro_module_anno)))
    list(edge_df = edge_df, anno = anno)
  }

  micro_module_res <- eventReactive(input$micro_module_build_graph, {
    tryCatch({
      dat <- micro_module_data_loader()
      graph_obj <- build_graph_from_module(
        df = dat$edge_df,
        node_annotation = dat$anno,
        directed = isTRUE(input$micro_module_directed),
        top_modules = input$micro_module_top_modules,
        seed = input$micro_module_seed
      )
      list(graph_obj = graph_obj, data_preview = list(edge_df = dat$edge_df, annotation = dat$anno))
    }, error = function(e) {
      showNotification(e$message, type = "error", duration = 8)
      NULL
    })
  })
  assign_network_outputs(input, output, session, "micro_module", micro_module_res, micro_module_data_loader)

  protein_data_loader <- function() {
    if (identical(input$protein_data_mode, "Demo")) {
      data_obj <- demo_edge_df()
      anno <- demo_node_df()
    } else {
      data_obj <- read_uploaded_data(input$protein_file)
      anno <- if (is.null(input$protein_anno)) NULL else coerce_node_annotation(normalize_loaded_object(read_uploaded_data(input$protein_anno)))
    }
    data_obj <- normalize_loaded_object(data_obj)
    list(data_obj = data_obj, anno = anno)
  }

  protein_res <- eventReactive(input$protein_build_graph, {
    tryCatch({
      dat <- protein_data_loader()

      graph_obj <- if (identical(input$protein_input_type, "Adjacency matrix")) {
        build_graph_from_adj_mat(
          adjacency_matrix = coerce_matrix(dat$data_obj),
          module.method = input$protein_module_method,
          node_annotation = dat$anno,
          top_modules = input$protein_top_modules,
          seed = input$protein_seed
        )
      } else {
        build_graph_from_df(
          df = coerce_edge_df(dat$data_obj),
          node_annotation = dat$anno,
          directed = isTRUE(input$protein_directed),
          module.method = input$protein_module_method,
          top_modules = input$protein_top_modules,
          seed = input$protein_seed
        )
      }
      list(graph_obj = graph_obj, data_preview = list(primary = dat$data_obj, annotation = dat$anno))
    }, error = function(e) {
      showNotification(e$message, type = "error", duration = 8)
      NULL
    })
  })
  assign_network_outputs(input, output, session, "protein", protein_res, protein_data_loader)

  wgcna_data_loader <- function() {
    demo_obj <- demo_wgcna()
    edge_df <- if (identical(input$wgcna_data_mode, "Demo")) demo_obj$edge else coerce_edge_df(normalize_loaded_object(read_uploaded_data(input$wgcna_edge)))
    module_df <- if (identical(input$wgcna_data_mode, "Demo")) demo_obj$module else as.data.frame(normalize_loaded_object(read_uploaded_data(input$wgcna_module)))
    anno <- if (is.null(input$wgcna_anno)) NULL else coerce_node_annotation(normalize_loaded_object(read_uploaded_data(input$wgcna_anno)))
    list(edge_df = edge_df, module_df = module_df, anno = anno)
  }

  wgcna_res <- eventReactive(input$wgcna_build_graph, {
    tryCatch({
      dat <- wgcna_data_loader()
      graph_obj <- build_graph_from_wgcna(
        wgcna_tom = dat$edge_df,
        module = dat$module_df,
        node_annotation = dat$anno,
        directed = isTRUE(input$wgcna_directed),
        seed = input$wgcna_seed
      )
      list(graph_obj = graph_obj, data_preview = list(wgcna_edge = dat$edge_df, module = dat$module_df, annotation = dat$anno))
    }, error = function(e) {
      showNotification(e$message, type = "error", duration = 8)
      NULL
    })
  })
  assign_network_outputs(input, output, session, "wgcna", wgcna_res, wgcna_data_loader)

  envlink_data_loader <- function() {
    if (identical(input$envlink_data_mode, "Demo")) {
      env <- as.data.frame(normalize_loaded_object(load_demo_dataset(
        "Envdf_4st",
        fallback = as.data.frame(matrix(rnorm(20 * 24), nrow = 20, ncol = 24))
      )))
      spec <- as.data.frame(normalize_loaded_object(load_demo_dataset(
        "Spedf",
        fallback = as.data.frame(matrix(abs(rnorm(20 * 18)), nrow = 20, ncol = 18))
      )))
      if (is.null(colnames(env))) colnames(env) <- paste0("Env_", seq_len(ncol(env)))
      if (is.null(colnames(spec))) colnames(spec) <- paste0("Spec_", seq_len(ncol(spec)))
    } else {
      env <- as.data.frame(normalize_loaded_object(read_uploaded_data(input$envlink_env)))
      spec <- as.data.frame(normalize_loaded_object(read_uploaded_data(input$envlink_spec)))
    }
    list(env = env, spec = spec)
  }

  envlink_data_preview <- reactiveVal(NULL)
  envlink_obj <- reactiveVal(NULL)
  envlink_plot_obj <- reactiveVal(NULL)

  observeEvent(input$envlink_preview_data, {
    out <- tryCatch(envlink_data_loader(), error = function(e) {
      showNotification(e$message, type = "error", duration = 8)
      NULL
    })
    envlink_data_preview(out)
  })

  observeEvent(input$envlink_build_obj, {
    out <- tryCatch({
      dat <- envlink_data_loader()
      env_select <- list(Env01 = seq_len(ncol(dat$env)))
      spec_select <- list(Spec01 = seq_len(ncol(dat$spec)))
      res <- gglink_heatmaps(
        env = dat$env,
        spec = dat$spec,
        env_select = env_select,
        spec_select = spec_select,
        relation_method = input$envlink_relation,
        cor.method = input$envlink_cor,
        orientation = input$envlink_orientation,
        r = input$envlink_radius,
        distance = input$envlink_distance,
        drop_nonsig = isTRUE(input$envlink_drop_nonsig)
      )
      list(data = dat, result = res)
    }, error = function(e) {
      showNotification(e$message, type = "error", duration = 8)
      NULL
    })
    envlink_obj(out)
    if (!is.null(out)) envlink_data_preview(out$data)
    envlink_plot_obj(NULL)
  })

  observeEvent(input$envlink_plot_graph, {
    obj <- envlink_obj()
    req(obj)
    envlink_plot_obj(obj$result)
  })

  output$envlink_data_preview <- DT::renderDT({
    dat <- envlink_data_preview()
    if (is.null(dat)) {
      return(DT::datatable(data.frame(Message = "Click `Preview Input Data` to load env/spec preview."), options = list(dom = "tip")))
    }
    env_df <- utils::head(dat$env, 100)
    spec_df <- utils::head(dat$spec, 100)
    env_df$.Dataset <- "env"
    spec_df$.Dataset <- "spec"
    all_df <- dplyr::bind_rows(env_df, spec_df)
    DT::datatable(all_df, options = list(pageLength = 8, scrollX = TRUE))
  })

  output$envlink_relation_summary <- DT::renderDT({
    obj <- envlink_obj()
    if (is.null(obj)) {
      return(DT::datatable(data.frame(Message = "Click `Build relation_obj` first."), options = list(dom = "tip")))
    }
    tbl <- obj$result[[3]]
    summary_df <- data.frame(
      Metric = c("Rows in relation table", "Unique species IDs", "Unique env variables"),
      Value = c(
        nrow(tbl),
        length(unique(tbl$ID)),
        length(unique(tbl$Type))
      )
    )
    DT::datatable(summary_df, options = list(dom = "tip"))
  })

  output$envlink_plot_1 <- renderPlot({
    x <- envlink_plot_obj()
    validate(need(!is.null(x), "Click `Visualize` after relation_obj is built."))
    x[[1]]
  })
  output$envlink_plot_2 <- renderPlot({
    x <- envlink_plot_obj()
    validate(need(!is.null(x), "Click `Visualize` after relation_obj is built."))
    x[[2]]
  })
  output$envlink_table <- DT::renderDT({
    x <- envlink_plot_obj()
    validate(need(!is.null(x), "Click `Visualize` after relation_obj is built."))
    DT::datatable(head(x[[3]], 300), options = list(pageLength = 10, scrollX = TRUE))
  })

  output$dev_pkg <- renderText({
    desc <- read.dcf("DESCRIPTION")
    paste(
      "Package:", desc[, "Package"],
      "\nVersion:", desc[, "Version"],
      "\nTitle:", desc[, "Title"]
    )
  })

  output$dev_functions <- DT::renderDT({
    funs <- ls(pattern = "^build_graph_from_", envir = app_env)
    if ("ggNetView" %in% ls(envir = app_env)) {
      funs <- c(funs, "ggNetView")
    }
    DT::datatable(data.frame(Function = sort(unique(funs))), options = list(dom = "tip", pageLength = 15))
  })

  output$dev_session <- renderText({
    paste(capture.output(sessionInfo()), collapse = "\n")
  })

  output$citation_text <- renderText({
    if (!file.exists("README.md")) {
      return("Citation text not found.")
    }
    lines <- readLines("README.md", warn = FALSE, encoding = "UTF-8")
    idx <- grep("^#### Citation", lines)
    if (length(idx) == 0) {
      return("Yue Liu, Chao Wang (2026). ggNetView: An R Package for Reproducible and Deterministic Network Analysis and Visualization.")
    }
    from <- idx[1]
    to <- min(length(lines), from + 8)
    paste(lines[from:to], collapse = "\n")
  })
}
