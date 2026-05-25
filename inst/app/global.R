library(shiny)
library(bslib)
library(DT)
library(ggplot2)
library(igraph)

app_root <- normalizePath(file.path("..", ".."), mustWork = FALSE)
options(ggnetview.app_root = app_root)

app_helper_env <- new.env(parent = .GlobalEnv)
app_helper_files <- file.path(app_root, "R", c(
  "app_validation.R",
  "app_registry.R",
  "app_adapters.R",
  "app_graph_builders.R",
  "app_graph_inspect.R",
  "app_topology_adapters.R",
  "app_compare_environment.R",
  "app_gallery_presets.R",
  "app_exports.R"
))

load_app_helper <- function(name) {
  if (exists(name, envir = .GlobalEnv, inherits = FALSE)) {
    return(invisible(TRUE))
  }

  ns <- tryCatch(asNamespace("ggNetView"), error = function(e) NULL)
  if (!is.null(ns) && exists(name, envir = ns, inherits = FALSE)) {
    assign(name, get(name, envir = ns), envir = .GlobalEnv)
    return(invisible(TRUE))
  }

  for (helper_file in app_helper_files) {
    if (file.exists(helper_file)) {
      sys.source(helper_file, envir = app_helper_env)
    }
  }

  if (exists(name, envir = app_helper_env, inherits = FALSE)) {
    assign(name, get(name, envir = app_helper_env), envir = .GlobalEnv)
    return(invisible(TRUE))
  }

  invisible(FALSE)
}

invisible(lapply(c(
  "app_result", "app_success", "app_failure",
  "read_user_table", "detect_upload_type", "validate_matrix_like",
  "safe_call", "safe_build_graph", "safe_graph_builder", "safe_rmt_threshold", "safe_plot_ggnetview", "safe_topology",
  "graph_builder_modes", "normalize_graph_builder_params", "required_builder_inputs",
  "validate_graph_builder_inputs", "graph_builder_function_name", "graph_builder_call_args",
  "safe_graph_info", "graph_module_choices", "safe_module_subgraph", "safe_sample_subgraph",
  "subgraph_selected_graph", "subgraph_stat_table",
  "safe_node_centrality", "safe_node_ivi", "safe_zipi", "node_table_from_graph", "adjacency_from_graph",
  "safe_multi_network_compare", "default_group_info_for_matrix", "align_group_info_for_matrix", "safe_multi_group_network",
  "safe_environment_link", "safe_environment_heatmap", "safe_environment_triple_heatmap", "safe_mantel_pairwise",
  "gallery_example_paths", "load_gallery_example_tables", "gallery_workflow_manifest",
  "gallery_recipe_manifest", "run_gallery_recipe", "register_gallery_examples",
  "registry_new", "registry_next_id", "registry_summarize", "registry_add",
  "registry_get", "registry_delete", "registry_count", "registry_list",
  "registry_choices", "registry_choices_by_type", "registry_log_error",
  "write_registry_table", "write_registry_object", "write_registry_params", "write_workflow_manifest",
  "read_workflow_manifest", "workflow_replay_plan", "workflow_replay_recipes",
  "export_formats_for_type", "write_graph_nodes_csv", "write_graph_edges_csv",
  "write_graph_adjacency_csv", "write_plot_png", "write_plot_pdf"
), load_app_helper))

module_base <- "modules"
if (!dir.exists(module_base) && dir.exists(file.path("inst", "app", "modules"))) {
  module_base <- file.path("inst", "app", "modules")
}

module_files <- file.path(module_base, c(
  "mod_data_hub.R",
  "mod_graph_builder.R",
  "mod_graph_explorer.R",
  "mod_visual_lab.R",
  "mod_topology_results.R",
  "mod_compare_environment.R",
  "mod_export_center.R"
))
invisible(lapply(module_files, source, local = FALSE))
