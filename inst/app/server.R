server <- function(input, output, session) {
  registry <- registry_new()

  mod_data_hub_server("data_hub", registry)
  mod_graph_builder_server("graph_builder", registry)
  mod_graph_explorer_server("graph_explorer", registry)
  mod_visual_lab_server("visual_lab", registry)
  mod_topology_results_server("topology_results", registry)
  mod_compare_environment_server("compare_environment", registry)
  mod_export_center_server("export_center", registry)
}
