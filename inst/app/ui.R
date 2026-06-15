ui <- bslib::page_navbar(
  title = shiny::tags$span(
    class = "ggnv-brand",
    shiny::tags$img(src = "logo.png", class = "ggnv-brand-logo", alt = "ggNetView"),
    "ggNetView"
  ),
  id = "main_nav",
  theme = app_bs_theme(),
  header = shiny::tagList(
    shiny::tags$link(rel = "stylesheet", type = "text/css", href = "styles.css?v=3"),
    shiny::tags$link(rel = "icon", type = "image/png", href = "favicon.png"),
    app_task_feedback_script()
  ),
  bslib::nav_panel("Introduction", mod_landing_ui("landing")),
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
  ),
  bslib::nav_panel("Data Hub", mod_data_hub_ui("data_hub")),
  bslib::nav_panel("Graph Builder", mod_graph_builder_ui("graph_builder")),
  bslib::nav_panel("RMT Builder", mod_rmt_builder_ui("rmt_builder")),
  bslib::nav_panel("Graph Explorer", mod_graph_explorer_ui("graph_explorer")),
  bslib::nav_panel("Visual Lab", mod_visual_lab_ui("visual_lab")),
  bslib::nav_menu(
    "Analysis",
    bslib::nav_panel("Topology", mod_topology_results_ui("topology_results")),
    bslib::nav_panel("Zi-Pi", mod_zipi_results_ui("zipi_results")),
    bslib::nav_panel("Perturbation", mod_perturbation_ui("perturbation")),
    bslib::nav_panel("Network Compare", mod_network_compare_ui("network_compare")),
    bslib::nav_panel("Environment Links", mod_environment_links_ui("environment_links"))
  ),
  bslib::nav_panel("Export", mod_export_center_ui("export_center")),
  bslib::nav_spacer(),
  bslib::nav_item(bslib::input_dark_mode(id = "color_mode", mode = "light"))
)
