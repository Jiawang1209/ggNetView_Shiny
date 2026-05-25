gallery_example_paths <- function(root = getOption("ggnetview.app_root", getwd())) {
  extdata <- file.path(root, "inst", "extdata")
  list(
    matrix = file.path(extdata, "phase2_example_matrix.csv"),
    matrix_b = file.path(extdata, "phase2_example_matrix_b.csv"),
    edges = file.path(extdata, "phase2_example_edges.csv"),
    modules = file.path(extdata, "phase2_example_modules.csv"),
    adjacency = file.path(extdata, "phase2_example_adjacency.csv"),
    tom = file.path(extdata, "phase2_example_tom.csv")
  )
}

load_gallery_example_tables <- function(root = getOption("ggnetview.app_root", getwd())) {
  paths <- gallery_example_paths(root)
  list(
    matrix = utils::read.csv(paths$matrix, row.names = 1, check.names = FALSE),
    matrix_b = utils::read.csv(paths$matrix_b, row.names = 1, check.names = FALSE),
    edges = utils::read.csv(paths$edges, check.names = FALSE),
    modules = utils::read.csv(paths$modules, check.names = FALSE),
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
      "double_matrix_graph",
      "multi_matrix_graph",
      "wgcna_tom_graph",
      "consensus_graph"
    ),
    manual_area = c(
      "Build graph from matrix",
      "Build graph from data frame with module",
      "Build graph from adjacency matrix",
      "Build graph from double matrix",
      "Build graph from multi matrix",
      "Build graph from WGCNA/TOM",
      "Build graph from consensus"
    ),
    stringsAsFactors = FALSE
  )
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

  invisible(items)
}
