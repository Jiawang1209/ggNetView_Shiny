ui <- bslib::page_navbar(
  title = "ggNetView",
  theme = bslib::bs_theme(version = 5, bootswatch = "flatly"),
  header = shiny::tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
  bslib::nav_panel("Data Hub", mod_data_hub_ui("data_hub")),
  bslib::nav_panel("Graph Builder", mod_graph_builder_ui("graph_builder")),
  bslib::nav_panel("Graph Explorer", mod_graph_explorer_ui("graph_explorer")),
  bslib::nav_panel("Visual Lab", mod_visual_lab_ui("visual_lab")),
  bslib::nav_panel("Topology", mod_topology_results_ui("topology_results")),
  bslib::nav_panel("Export", mod_export_center_ui("export_center"))
)
