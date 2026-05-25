mod_export_center_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::card(
    bslib::card_header("Export Center"),
    shiny::selectInput(ns("object_id"), "Object", choices = character()),
    shiny::tableOutput(ns("object_summary")),
    object_download_controls(ns),
    shiny::uiOutput(ns("type_downloads")),
    workflow_download_controls(ns),
    bslib::card_header("Replay Plan"),
    shiny::fileInput(ns("workflow_manifest"), "Workflow JSON", accept = c(".json", "application/json")),
    shiny::actionButton(ns("run_replay_recipes"), "Run Replay Recipes"),
    shiny::verbatimTextOutput(ns("replay_status")),
    shiny::tableOutput(ns("replay_plan"))
  )
}

safe_download_base <- function(item) {
  name <- item$name
  if (is.null(name) || !nzchar(trimws(name))) {
    name <- item$id
  }
  name <- gsub("[^A-Za-z0-9._-]+", "_", name)
  name <- gsub("_+", "_", name)
  name <- gsub("^_+|_+$", "", name)
  if (!nzchar(name)) {
    name <- item$id
  }
  name
}

export_control_section <- function(title, ...) {
  controls <- list(...)
  controls <- controls[!vapply(controls, is.null, logical(1))]
  if (!length(controls)) {
    return(NULL)
  }

  shiny::tagList(
    shiny::tags$div(
      class = "ggnv-export-section",
      shiny::tags$h5(title),
      shiny::tags$div(
        class = "ggnv-export-buttons",
        controls
      )
    )
  )
}

object_download_controls <- function(ns = identity) {
  export_control_section(
    "Selected Object Downloads",
    shiny::downloadButton(ns("download_rds"), "Download Object RDS"),
    shiny::downloadButton(ns("download_csv"), "Download Object CSV"),
    shiny::downloadButton(ns("download_params"), "Download Parameters JSON")
  )
}

workflow_download_controls <- function(ns = identity) {
  export_control_section(
    "Session & Workflow Downloads",
    shiny::downloadButton(ns("download_manifest"), "Download Session Manifest CSV"),
    shiny::downloadButton(ns("download_workflow_manifest"), "Download Workflow Manifest JSON")
  )
}

registry_manifest <- function(registry) {
  items <- shiny::isolate(registry$items)
  if (!length(items)) {
    return(data.frame(
      id = character(),
      name = character(),
      type = character(),
      source = character(),
      created_at = as.POSIXct(character()),
      stringsAsFactors = FALSE
    ))
  }

  do.call(rbind, lapply(items, function(item) {
    source <- item$source
    if (is.null(source)) {
      source <- ""
    }

    data.frame(
      id = item$id,
      name = item$name,
      type = item$type,
      source = source,
      created_at = item$created_at,
      stringsAsFactors = FALSE
    )
  }))
}

is_plot_item <- function(item) {
  !is.null(item) && identical(item$type, "plot")
}

plot_download_controls <- function(item, ns = identity) {
  if (!is_plot_item(item)) {
    return(NULL)
  }

  shiny::tagList(
    shiny::downloadButton(ns("download_png"), "Download Plot PNG"),
    shiny::downloadButton(ns("download_pdf"), "Download Plot PDF")
  )
}

type_download_controls <- function(item, ns = identity) {
  if (is.null(item)) {
    return(NULL)
  }

  formats <- export_formats_for_type(item$type)
  controls <- list()
  if ("nodes_csv" %in% formats) {
    controls <- c(controls, list(shiny::downloadButton(ns("download_nodes_csv"), "Download Nodes CSV")))
  }
  if ("edges_csv" %in% formats) {
    controls <- c(controls, list(shiny::downloadButton(ns("download_edges_csv"), "Download Edges CSV")))
  }
  if ("adjacency_csv" %in% formats) {
    controls <- c(controls, list(shiny::downloadButton(ns("download_adjacency_csv"), "Download Adjacency CSV")))
  }
  if ("png" %in% formats || "pdf" %in% formats) {
    controls <- c(controls, list(plot_download_controls(item, ns)))
  }
  if (!length(controls)) {
    return(NULL)
  }

  title <- switch(item$type,
    graph = "Graph Downloads",
    plot = "Plot Downloads",
    paste(tools::toTitleCase(gsub("_", " ", item$type %||% "Object")), "Downloads")
  )
  do.call(export_control_section, c(list(title = title), controls))
}

export_format_labels <- function(type) {
  labels <- c(
    rds = "RDS",
    csv = "CSV",
    nodes_csv = "Nodes CSV",
    edges_csv = "Edges CSV",
    adjacency_csv = "Adjacency CSV",
    params_json = "Parameters JSON",
    png = "PNG",
    pdf = "PDF"
  )
  formats <- export_formats_for_type(type)
  unname(labels[formats] %||% formats)
}

export_object_summary <- function(item) {
  if (is.null(item)) {
    return(data.frame(field = character(), value = character(), stringsAsFactors = FALSE))
  }

  source <- item$source
  if (is.null(source) || !nzchar(as.character(source))) {
    source <- "manual/session"
  }
  params <- names(item$params %||% list())
  params <- if (length(params)) paste(params, collapse = ", ") else "none"
  summary <- item$summary %||% list()
  summary_text <- if (length(summary)) {
    paste(sprintf("%s=%s", names(summary), vapply(summary, function(x) paste(head(x, 3), collapse = "/"), character(1))), collapse = "; ")
  } else {
    "none"
  }

  data.frame(
    field = c("ID", "Name", "Type", "Source", "Formats", "Summary", "Parameters"),
    value = c(
      item$id %||% "",
      item$name %||% "",
      item$type %||% "",
      as.character(source),
      paste(export_format_labels(item$type), collapse = ", "),
      summary_text,
      params
    ),
    stringsAsFactors = FALSE
  )
}

mod_export_center_server <- function(id, registry) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observe({
      shiny::updateSelectInput(session, "object_id", choices = registry_choices(registry))
    })

    selected_item <- shiny::reactive({
      shiny::req(input$object_id)
      item <- registry_get(registry, input$object_id)
      shiny::validate(shiny::need(!is.null(item), "Selected object is no longer available."))
      item
    })

    output$type_downloads <- shiny::renderUI({
      item <- selected_item()
      type_download_controls(item, session$ns)
    })

    output$object_summary <- shiny::renderTable({
      export_object_summary(selected_item())
    }, striped = TRUE, bordered = TRUE, spacing = "s", rownames = FALSE)

    replay_plan <- shiny::reactive({
      file <- input$workflow_manifest
      shiny::req(file)
      manifest <- read_workflow_manifest(file$datapath)
      workflow_replay_plan(manifest)
    })

    output$replay_plan <- shiny::renderTable({
      replay_plan()
    }, striped = TRUE, bordered = TRUE, spacing = "s", rownames = FALSE)

    replay_status <- shiny::reactiveVal("Import a workflow JSON to preview replay steps.")

    shiny::observeEvent(input$run_replay_recipes, {
      plan <- replay_plan()
      recipes <- workflow_replay_recipes(plan, gallery_recipe_manifest()$recipe)
      if (!length(recipes)) {
        replay_status("No supported gallery recipes found in the imported manifest.")
        return(invisible(NULL))
      }

      results <- lapply(recipes, function(recipe) run_gallery_recipe(registry, recipe))
      failed <- vapply(results, function(result) !isTRUE(result$ok), logical(1))
      if (any(failed)) {
        messages <- vapply(results[failed], function(result) result$message %||% "unknown replay failure", character(1))
        replay_status(paste("Replay failed:", paste(messages, collapse = " | ")))
        return(invisible(results))
      }

      replay_status(paste("Replayed gallery recipes:", paste(recipes, collapse = ", ")))
      invisible(results)
    })

    output$replay_status <- shiny::renderText(replay_status())

    output$download_manifest <- shiny::downloadHandler(
      filename = function() "ggnetview_manifest.csv",
      content = function(file) write_registry_table(registry_manifest(registry), file)
    )

    output$download_workflow_manifest <- shiny::downloadHandler(
      filename = function() "ggnetview_workflow_manifest.json",
      content = function(file) write_workflow_manifest(registry, file)
    )

    output$download_rds <- shiny::downloadHandler(
      filename = function() paste0(safe_download_base(selected_item()), ".rds"),
      content = function(file) write_registry_object(selected_item()$data, file)
    )

    output$download_csv <- shiny::downloadHandler(
      filename = function() paste0(safe_download_base(selected_item()), ".csv"),
      content = function(file) {
        data <- selected_item()$data
        if (!is.matrix(data) && !is.data.frame(data)) {
          data <- data.frame(value = utils::capture.output(utils::str(data)))
        }
        write_registry_table(data, file)
      }
    )

    output$download_params <- shiny::downloadHandler(
      filename = function() paste0(safe_download_base(selected_item()), "_params.json"),
      content = function(file) write_registry_params(selected_item()$params, file)
    )

    output$download_nodes_csv <- shiny::downloadHandler(
      filename = function() paste0(safe_download_base(selected_item()), "_nodes.csv"),
      content = function(file) {
        shiny::validate(shiny::need(identical(selected_item()$type, "graph"), "Node export requires a graph object."))
        write_graph_nodes_csv(selected_item()$data, file)
      }
    )

    output$download_edges_csv <- shiny::downloadHandler(
      filename = function() paste0(safe_download_base(selected_item()), "_edges.csv"),
      content = function(file) {
        shiny::validate(shiny::need(identical(selected_item()$type, "graph"), "Edge export requires a graph object."))
        write_graph_edges_csv(selected_item()$data, file)
      }
    )

    output$download_adjacency_csv <- shiny::downloadHandler(
      filename = function() paste0(safe_download_base(selected_item()), "_adjacency.csv"),
      content = function(file) {
        shiny::validate(shiny::need(identical(selected_item()$type, "graph"), "Adjacency export requires a graph object."))
        write_graph_adjacency_csv(selected_item()$data, file)
      }
    )

    output$download_png <- shiny::downloadHandler(
      filename = function() paste0(safe_download_base(selected_item()), ".png"),
      content = function(file) {
        shiny::validate(shiny::need(is_plot_item(selected_item()), "PNG/PDF export requires a plot object."))
        write_plot_png(selected_item()$data, file)
      }
    )

    output$download_pdf <- shiny::downloadHandler(
      filename = function() paste0(safe_download_base(selected_item()), ".pdf"),
      content = function(file) {
        shiny::validate(shiny::need(is_plot_item(selected_item()), "PNG/PDF export requires a plot object."))
        write_plot_pdf(selected_item()$data, file)
      }
    )
  })
}
