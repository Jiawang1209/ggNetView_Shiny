server <- function(input, output, session) {
  registry <- registry_new()

  mod_data_hub_server("data_hub", registry)
  mod_graph_builder_server("graph_builder", registry)
  mod_rmt_builder_server("rmt_builder", registry)
  mod_graph_explorer_server("graph_explorer", registry)
  mod_visual_lab_server("visual_lab", registry)
  mod_topology_results_server("topology_results", registry)
  mod_zipi_results_server("zipi_results", registry)
  mod_network_compare_server("network_compare", registry)
  mod_environment_links_server("environment_links", registry)
  mod_export_center_server("export_center", registry)
}
