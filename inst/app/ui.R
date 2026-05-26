ui <- bslib::page_navbar(
  title = "ggNetView",
  theme = bslib::bs_theme(version = 5, bootswatch = "flatly"),
  header = shiny::tagList(
    shiny::tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    app_task_feedback_script()
  ),
  bslib::nav_panel(
    "Introduction",
    bslib::card(
      class = "ggnv-introduction",
      shiny::includeMarkdown(file.path(app_root, "README.md"))
    )
  ),
  bslib::nav_panel("Data Hub", mod_data_hub_ui("data_hub")),
  bslib::nav_panel("Graph Builder", mod_graph_builder_ui("graph_builder")),
  bslib::nav_panel("Graph Explorer", mod_graph_explorer_ui("graph_explorer")),
  bslib::nav_panel("Visual Lab", mod_visual_lab_ui("visual_lab")),
  bslib::nav_panel("Topology", mod_topology_results_ui("topology_results")),
  bslib::nav_panel("Compare & Environment", mod_compare_environment_ui("compare_environment")),
  bslib::nav_panel("Export", mod_export_center_ui("export_center")),
  bslib::nav_panel(
    "Manual",
    shiny::div(
      class = "ggnv-manual-toolbar",
      shiny::tags$a(
        href = "manual/index.html",
        target = "_blank",
        rel = "noopener",
        "Open manual in new tab"
      )
    ),
    shiny::tags$iframe(
      class = "ggnv-manual-frame",
      src = "manual/index.html",
      title = "ggNetView manual"
    )
  )
)
