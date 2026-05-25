gallery_example_paths <- function(root = getOption("ggnetview.app_root", getwd())) {
  extdata <- file.path(root, "inst", "extdata")
  list(
    matrix = file.path(extdata, "phase2_example_matrix.csv"),
    matrix_b = file.path(extdata, "phase2_example_matrix_b.csv"),
    rmt_matrix = file.path(extdata, "phase2_example_rmt_matrix.csv"),
    edges = file.path(extdata, "phase2_example_edges.csv"),
    modules = file.path(extdata, "phase2_example_modules.csv"),
    sample_metadata = file.path(extdata, "phase2_example_sample_metadata.csv"),
    adjacency = file.path(extdata, "phase2_example_adjacency.csv"),
    tom = file.path(extdata, "phase2_example_tom.csv")
  )
}

load_gallery_example_tables <- function(root = getOption("ggnetview.app_root", getwd())) {
  paths <- gallery_example_paths(root)
  list(
    matrix = utils::read.csv(paths$matrix, row.names = 1, check.names = FALSE),
    matrix_b = utils::read.csv(paths$matrix_b, row.names = 1, check.names = FALSE),
    rmt_matrix = utils::read.csv(paths$rmt_matrix, row.names = 1, check.names = FALSE),
    edges = utils::read.csv(paths$edges, check.names = FALSE),
    modules = utils::read.csv(paths$modules, check.names = FALSE),
    sample_metadata = utils::read.csv(paths$sample_metadata, check.names = FALSE),
    adjacency = utils::read.csv(paths$adjacency, row.names = 1, check.names = FALSE),
    tom = utils::read.csv(paths$tom, row.names = 1, check.names = FALSE)
  )
}

gallery_workflow_manifest <- function() {
  data.frame(
    workflow = c(
      "matrix_graph",
      "edge_module_graph",
      "adjacency_graph",
      "grouped_matrix_graph",
      "double_matrix_graph",
      "multi_matrix_graph",
      "wgcna_tom_graph",
      "consensus_graph"
    ),
    manual_area = c(
      "Build graph from matrix",
      "Build graph from data frame with module",
      "Build graph from adjacency matrix",
      "Build grouped networks from matrix and sample metadata",
      "Build graph from double matrix",
      "Build graph from multi matrix",
      "Build graph from WGCNA/TOM",
      "Build graph from consensus"
    ),
    stringsAsFactors = FALSE
  )
}

gallery_recipe_manifest <- function() {
  data.frame(
    recipe = c(
      "network_plot_circle",
      "grouped_network_plot",
      "graph_info_topology",
      "environment_heatmap",
      "mantel_pairwise",
      "multi_network_compare",
      "triple_environment_heatmap",
      "multi_omics_network",
      "multi_omics_double_matrix",
      "multi_omics_environment_blocks",
      "environment_collapsed_core",
      "environment_arc_collapsed_core"
    ),
    label = c(
      "Circle network plot",
      "Grouped matrix network plot",
      "Graph info and topology",
      "Environment heatmap",
      "Mantel pairwise table",
      "Multi-network comparison",
      "Triple environment heatmap",
      "Multi-omics network",
      "Multi-omics double-matrix network",
      "Multi-omics environment blocks",
      "Collapsed-core environment heatmap",
      "Arc collapsed-core environment heatmap"
    ),
    output_type = c(
      "plot",
      "plot,result",
      "result",
      "plot,result",
      "result",
      "plot,result",
      "result",
      "graph,plot",
      "graph,plot",
      "plot,result",
      "plot,result",
      "plot,result"
    ),
    manual_area = c(
      "Gallery network layout",
      "Network comparison from sample metadata",
      "Get network information and topology",
      "Network-environment heatmap",
      "Environment Mantel helper",
      "Multi-network comparison",
      "Network-environment triple heatmap",
      "Multi-omics network analysis",
      "Multi-omics double matrix network analysis",
      "Multi-omics network-environment analysis",
      "Network-environment collapsed core",
      "Network-environment rotated arc collapsed core"
    ),
    stringsAsFactors = FALSE
  )
}

gallery_environment_fixture <- function(matrix) {
  spec <- t(as.matrix(matrix))
  env <- data.frame(
    temperature = c(12, 13, 14, 16, 18),
    pH = c(6.8, 6.9, 7.1, 7.2, 7.4),
    moisture = c(30, 31, 35, 36, 40),
    row.names = rownames(spec),
    check.names = FALSE
  )
  list(spec = as.data.frame(spec, check.names = FALSE), env = env)
}

gallery_multi_omics_environment_fixture <- function(matrix_a, matrix_b) {
  spec_a <- as.data.frame(t(as.matrix(matrix_a)), check.names = FALSE)
  spec_b <- as.data.frame(t(as.matrix(matrix_b)), check.names = FALSE)
  spec <- cbind(spec_a, spec_b)
  env <- gallery_environment_fixture(matrix_a)$env
  env$conductivity <- c(100, 105, 112, 119, 126)
  list(
    spec = spec,
    env = env,
    env_select = list(Soil = c("temperature", "pH"), Water = c("moisture", "conductivity")),
    spec_select = list(
      Microbiome = colnames(spec_a),
      Transcriptome = colnames(spec_b)
    ),
    env_spec_pairs = "Soil,Microbiome\nWater,Transcriptome"
  )
}

gallery_registry_item_by_name <- function(registry, name) {
  listed <- shiny::isolate(registry_list(registry))
  hit <- listed[listed$name == name, , drop = FALSE]
  if (!nrow(hit)) {
    return(NULL)
  }
  shiny::isolate(registry_get(registry, hit$id[[1]]))
}

register_gallery_examples <- function(registry, root = getOption("ggnetview.app_root", getwd())) {
  data <- load_gallery_example_tables(root)
  items <- list()
  add_item <- function(name, type, value, source) {
    item <- registry_add(registry, name = name, type = type, data = value, source = source)
    items[[name]] <<- item
    item
  }

  add_item("gallery_matrix", "matrix", data$matrix, "phase2_example_matrix.csv")
  add_item("gallery_matrix_b", "matrix", data$matrix_b, "phase2_example_matrix_b.csv")
  add_item("gallery_edges", "edge_table", data$edges, "phase2_example_edges.csv")
  add_item("gallery_modules", "module_table", data$modules, "phase2_example_modules.csv")
  add_item("gallery_sample_metadata", "sample_metadata", data$sample_metadata, "phase2_example_sample_metadata.csv")
  add_item("gallery_adjacency", "adjacency", data$adjacency, "phase2_example_adjacency.csv")
  add_item("gallery_tom", "wgcna_tom", data$tom, "phase2_example_tom.csv")
  add_item("gallery_workflows", "result", gallery_workflow_manifest(), "manual-gallery-presets")

  matrix_graph <- safe_graph_builder(
    "matrix",
    inputs = list(matrix = data$matrix),
    params = list(method = "cor", r.threshold = 0.2, p.threshold = 1)
  )
  if (matrix_graph$ok) {
    add_item("gallery_matrix_graph", "graph", matrix_graph$value, "gallery_matrix")
  }

  edge_graph <- safe_graph_builder(
    "edge_table",
    inputs = list(edge_table = data$edges, module_table = data$modules),
    params = list()
  )
  if (edge_graph$ok) {
    add_item("gallery_edge_module_graph", "graph", edge_graph$value, "gallery_edges,gallery_modules")
  }

  add_item("gallery_rmt_matrix", "matrix", data$rmt_matrix, "phase2_example_rmt_matrix.csv")

  invisible(items)
}

run_gallery_recipe <- function(registry, recipe) {
  recipes <- gallery_recipe_manifest()
  if (!recipe %in% recipes$recipe) {
    return(app_failure(sprintf("Unknown gallery recipe: %s", recipe)))
  }

  add_recipe_item <- function(name, type, data, source, params = list()) {
    registry_add(
      registry,
      name = name,
      type = type,
      data = data,
      source = source,
      params = c(list(recipe = recipe), params)
    )
  }

  if (identical(recipe, "network_plot_circle")) {
    graph_item <- gallery_registry_item_by_name(registry, "gallery_matrix_graph")
    if (is.null(graph_item)) {
      return(app_failure("Load gallery examples before running this recipe."))
    }
    result <- safe_plot_ggnetview(graph_item$data, params = list(layout = "circle"))
    if (!result$ok) {
      return(result)
    }
    item <- add_recipe_item(
      "gallery_recipe_circle_plot",
      "plot",
      result$value,
      graph_item$id,
      list(layout = "circle")
    )
    return(app_success(list(items = list(item))))
  }

  if (identical(recipe, "grouped_network_plot")) {
    matrix_item <- gallery_registry_item_by_name(registry, "gallery_matrix")
    metadata_item <- gallery_registry_item_by_name(registry, "gallery_sample_metadata")
    if (is.null(matrix_item) || is.null(metadata_item)) {
      return(app_failure("Load gallery examples before running this recipe."))
    }
    result <- safe_multi_group_network(
      matrix_item$data,
      group_info = metadata_item$data,
      params = list(r.threshold = 0.2, p.threshold = 1)
    )
    if (!result$ok) {
      return(result)
    }
    plot_item <- add_recipe_item(
      "gallery_recipe_grouped_network_plot",
      "plot",
      result$value$plot,
      paste(matrix_item$id, metadata_item$id, sep = ","),
      list(r.threshold = 0.2, p.threshold = 1)
    )
    group_item <- add_recipe_item(
      "gallery_recipe_grouped_network_groups",
      "result",
      result$value$group_info,
      paste(matrix_item$id, metadata_item$id, sep = ","),
      list(kind = "sample_groups")
    )
    return(app_success(list(items = list(plot_item, group_item))))
  }

  if (identical(recipe, "graph_info_topology")) {
    graph_item <- gallery_registry_item_by_name(registry, "gallery_matrix_graph")
    if (is.null(graph_item)) {
      return(app_failure("Load gallery examples before running this recipe."))
    }
    info <- safe_graph_info(graph_item$data)
    if (!info$ok) {
      return(info)
    }
    topology <- safe_topology(graph_item$data)
    if (!topology$ok) {
      return(topology)
    }
    info_item <- add_recipe_item(
      "gallery_recipe_graph_info",
      "result",
      info$value,
      graph_item$id,
      list(kind = "graph_info")
    )
    topology_item <- add_recipe_item(
      "gallery_recipe_network_topology",
      "result",
      topology$value,
      graph_item$id,
      list(kind = "network_topology")
    )
    return(app_success(list(items = list(info_item, topology_item))))
  }

  if (identical(recipe, "environment_heatmap")) {
    matrix_item <- gallery_registry_item_by_name(registry, "gallery_matrix")
    if (is.null(matrix_item)) {
      return(app_failure("Load gallery examples before running this recipe."))
    }
    fixture <- gallery_environment_fixture(matrix_item$data)
    result <- safe_environment_heatmap(
      env = fixture$env,
      spec = fixture$spec,
      env_select = list(Environment = seq_len(ncol(fixture$env))),
      spec_select = list(Species = seq_len(ncol(fixture$spec)))
    )
    if (!result$ok) {
      return(result)
    }
    plot_item <- add_recipe_item(
      "gallery_recipe_environment_heatmap",
      "plot",
      result$value$plot,
      matrix_item$id,
      list(relation_method = "correlation")
    )
    stats_item <- add_recipe_item(
      "gallery_recipe_environment_stats",
      "result",
      result$value$stats,
      matrix_item$id,
      list(kind = "environment_stats", relation_method = "correlation")
    )
    return(app_success(list(items = list(plot_item, stats_item))))
  }

  if (identical(recipe, "mantel_pairwise")) {
    matrix_item <- gallery_registry_item_by_name(registry, "gallery_matrix")
    if (is.null(matrix_item)) {
      return(app_failure("Load gallery examples before running this recipe."))
    }
    fixture <- gallery_environment_fixture(matrix_item$data)
    result <- safe_mantel_pairwise(
      spec = fixture$spec[, 1:2, drop = FALSE],
      env = fixture$env[, 1:2, drop = FALSE],
      params = list(permutations = 9L)
    )
    if (!result$ok) {
      return(result)
    }
    item <- add_recipe_item(
      "gallery_recipe_mantel_pairwise",
      "result",
      result$value,
      matrix_item$id,
      list(permutations = 9L)
    )
    return(app_success(list(items = list(item))))
  }

  if (identical(recipe, "multi_network_compare")) {
    graph_item <- gallery_registry_item_by_name(registry, "gallery_matrix_graph")
    edge_graph_item <- gallery_registry_item_by_name(registry, "gallery_edge_module_graph")
    if (is.null(graph_item) || is.null(edge_graph_item)) {
      return(app_failure("Load gallery examples before running this recipe."))
    }
    result <- safe_multi_network_compare(list(Matrix = graph_item$data, EdgeModule = edge_graph_item$data))
    if (!result$ok) {
      return(result)
    }
    plot_item <- add_recipe_item(
      "gallery_recipe_multi_network_compare",
      "plot",
      result$value$plot,
      paste(graph_item$id, edge_graph_item$id, sep = ","),
      list(layout = "fr", layout.module = "adjacent")
    )
    link_item <- add_recipe_item(
      "gallery_recipe_multi_network_links",
      "result",
      result$value$link_info %||% data.frame(),
      paste(graph_item$id, edge_graph_item$id, sep = ","),
      list(kind = "multi_network_link_info")
    )
    return(app_success(list(items = list(plot_item, link_item))))
  }

  if (identical(recipe, "triple_environment_heatmap")) {
    matrix_item <- gallery_registry_item_by_name(registry, "gallery_matrix")
    graph_item <- gallery_registry_item_by_name(registry, "gallery_matrix_graph")
    if (is.null(matrix_item) || is.null(graph_item)) {
      return(app_failure("Load gallery examples before running this recipe."))
    }
    fixture <- gallery_environment_fixture(matrix_item$data)
    result <- safe_environment_triple_heatmap(
      env = fixture$env,
      experiment = fixture$spec,
      graph = graph_item$data,
      params = list(feature_count = 3L, r = 6)
    )
    if (!result$ok) {
      return(result)
    }
    plot_item <- add_recipe_item(
      "gallery_recipe_triple_environment_heatmap",
      "plot",
      result$value$plot,
      paste(matrix_item$id, graph_item$id, sep = ","),
      list(feature_count = 3L)
    )
    nodes_item <- add_recipe_item(
      "gallery_recipe_triple_environment_nodes",
      "result",
      result$value$nodes,
      graph_item$id,
      list(kind = "triple_heatmap_nodes")
    )
    return(app_success(list(items = list(plot_item, nodes_item))))
  }

  if (identical(recipe, "multi_omics_network")) {
    matrix_item <- gallery_registry_item_by_name(registry, "gallery_matrix")
    matrix_b_item <- gallery_registry_item_by_name(registry, "gallery_matrix_b")
    if (is.null(matrix_item) || is.null(matrix_b_item)) {
      return(app_failure("Load gallery examples before running this recipe."))
    }
    graph_result <- safe_graph_builder(
      "multi_matrix",
      inputs = list(matrices = list(matrix_item$data, matrix_b_item$data)),
      params = list()
    )
    if (!graph_result$ok) {
      return(graph_result)
    }
    graph_item <- add_recipe_item(
      "gallery_recipe_multi_omics_graph",
      "graph",
      graph_result$value,
      paste(matrix_item$id, matrix_b_item$id, sep = ","),
      list(kind = "multi_matrix_graph", blocks = c("matrix", "matrix_b"))
    )

    plot_result <- safe_plot_ggnetview(graph_result$value, params = list(layout = "circle"))
    if (!plot_result$ok) {
      return(plot_result)
    }
    plot_item <- add_recipe_item(
      "gallery_recipe_multi_omics_plot",
      "plot",
      plot_result$value,
      graph_item$id,
      list(layout = "circle", graph = graph_item$name)
    )
    return(app_success(list(items = list(graph_item, plot_item))))
  }

  if (identical(recipe, "multi_omics_double_matrix")) {
    matrix_item <- gallery_registry_item_by_name(registry, "gallery_matrix")
    matrix_b_item <- gallery_registry_item_by_name(registry, "gallery_matrix_b")
    if (is.null(matrix_item) || is.null(matrix_b_item)) {
      return(app_failure("Load gallery examples before running this recipe."))
    }
    graph_result <- safe_graph_builder(
      "double_matrix",
      inputs = list(matrix_a = matrix_item$data, matrix_b = matrix_b_item$data),
      params = list(module.method = "Fast_greedy")
    )
    if (!graph_result$ok) {
      return(graph_result)
    }
    graph_item <- add_recipe_item(
      "gallery_recipe_multi_omics_double_graph",
      "graph",
      graph_result$value,
      paste(matrix_item$id, matrix_b_item$id, sep = ","),
      list(kind = "double_matrix_graph", blocks = c("microbiome", "transcriptome"))
    )

    plot_result <- safe_plot_ggnetview(graph_result$value, params = list(layout = "circle"))
    if (!plot_result$ok) {
      return(plot_result)
    }
    plot_item <- add_recipe_item(
      "gallery_recipe_multi_omics_double_plot",
      "plot",
      plot_result$value,
      graph_item$id,
      list(layout = "circle", graph = graph_item$name)
    )
    return(app_success(list(items = list(graph_item, plot_item))))
  }

  if (identical(recipe, "multi_omics_environment_blocks")) {
    matrix_item <- gallery_registry_item_by_name(registry, "gallery_matrix")
    matrix_b_item <- gallery_registry_item_by_name(registry, "gallery_matrix_b")
    if (is.null(matrix_item) || is.null(matrix_b_item)) {
      return(app_failure("Load gallery examples before running this recipe."))
    }
    fixture <- gallery_multi_omics_environment_fixture(matrix_item$data, matrix_b_item$data)
    result <- safe_environment_heatmap(
      env = fixture$env,
      spec = fixture$spec,
      env_select = fixture$env_select,
      spec_select = fixture$spec_select,
      env_spec_pairs = fixture$env_spec_pairs,
      params = list(
        relation_method = "correlation",
        cor.method = "pearson",
        orientation = c("top_right", "bottom_right"),
        group_layout = "row",
        r = 2
      )
    )
    if (!result$ok) {
      return(result)
    }
    source_ids <- paste(matrix_item$id, matrix_b_item$id, sep = ",")
    plot_item <- add_recipe_item(
      "gallery_recipe_multi_omics_environment_heatmap",
      "plot",
      result$value$plot,
      source_ids,
      list(kind = "multi_omics_environment_heatmap", relation_method = "correlation")
    )
    stats_item <- add_recipe_item(
      "gallery_recipe_multi_omics_environment_stats",
      "result",
      result$value$stats,
      source_ids,
      list(
        kind = "multi_omics_environment_stats",
        env_blocks = names(fixture$env_select),
        spec_blocks = names(fixture$spec_select)
      )
    )
    return(app_success(list(items = list(plot_item, stats_item))))
  }

  if (identical(recipe, "environment_collapsed_core")) {
    matrix_item <- gallery_registry_item_by_name(registry, "gallery_matrix")
    if (is.null(matrix_item)) {
      return(app_failure("Load gallery examples before running this recipe."))
    }
    fixture <- gallery_environment_fixture(matrix_item$data)
    fixture$env$conductivity <- c(100, 105, 112, 119, 126)
    result <- safe_environment_heatmap(
      env = fixture$env,
      spec = fixture$spec,
      env_blocks = "Climate: temperature,pH\nWater: moisture,conductivity",
      spec_blocks = "Early: OTU1,OTU2,OTU3\nLate: OTU4,OTU5,OTU6",
      env_spec_pairs = "Climate,Early\nWater,Late",
      params = list(
        relation_method = "correlation",
        cor.method = "pearson",
        spec_collapse = TRUE,
        orientation = c("top_right", "bottom_right"),
        group_layout = "row",
        anchor_dist = 4,
        distance = 2,
        r = 0.1,
        CorePointSize = 10,
        HeatmapPointSize = 4
      )
    )
    if (!result$ok) {
      return(result)
    }
    plot_item <- add_recipe_item(
      "gallery_recipe_environment_collapsed_core_heatmap",
      "plot",
      result$value$plot,
      matrix_item$id,
      list(
        kind = "environment_collapsed_core_heatmap",
        relation_method = "correlation",
        spec_collapse = TRUE,
        env_blocks = c("Climate", "Water"),
        spec_blocks = c("Early", "Late")
      )
    )
    stats_item <- add_recipe_item(
      "gallery_recipe_environment_collapsed_core_stats",
      "result",
      result$value$stats,
      matrix_item$id,
      list(
        kind = "environment_collapsed_core_stats",
        relation_method = "correlation",
        spec_collapse = TRUE
      )
    )
    return(app_success(list(items = list(plot_item, stats_item))))
  }

  if (identical(recipe, "environment_arc_collapsed_core")) {
    matrix_item <- gallery_registry_item_by_name(registry, "gallery_matrix")
    if (is.null(matrix_item)) {
      return(app_failure("Load gallery examples before running this recipe."))
    }
    fixture <- gallery_environment_fixture(matrix_item$data)
    fixture$env$conductivity <- c(100, 105, 112, 119, 126)
    result <- safe_environment_heatmap(
      env = fixture$env,
      spec = fixture$spec,
      env_blocks = "Climate: temperature,pH\nWater: moisture,conductivity",
      spec_blocks = "Early: OTU1,OTU2,OTU3\nLate: OTU4,OTU5,OTU6",
      env_spec_pairs = "Climate,Early\nWater,Late",
      params = list(
        relation_method = "correlation",
        cor.method = "pearson",
        spec_collapse = TRUE,
        orientation = c("top_right", "bottom_right"),
        group_layout = "arc",
        group_angle = 45,
        group_arc_angle = 120,
        anchor_dist = 4,
        distance = -1,
        r = 0.1,
        CorePointSize = 10,
        HeatmapPointSize = 4
      )
    )
    if (!result$ok) {
      return(result)
    }
    plot_item <- add_recipe_item(
      "gallery_recipe_environment_arc_collapsed_core_heatmap",
      "plot",
      result$value$plot,
      matrix_item$id,
      list(
        kind = "environment_arc_collapsed_core_heatmap",
        relation_method = "correlation",
        spec_collapse = TRUE,
        group_layout = "arc",
        group_angle = 45,
        group_arc_angle = 120,
        distance = -1,
        env_blocks = c("Climate", "Water"),
        spec_blocks = c("Early", "Late")
      )
    )
    stats_item <- add_recipe_item(
      "gallery_recipe_environment_arc_collapsed_core_stats",
      "result",
      result$value$stats,
      matrix_item$id,
      list(
        kind = "environment_arc_collapsed_core_stats",
        relation_method = "correlation",
        spec_collapse = TRUE,
        group_layout = "arc"
      )
    )
    return(app_success(list(items = list(plot_item, stats_item))))
  }

  app_failure(sprintf("Gallery recipe is not implemented: %s", recipe))
}
