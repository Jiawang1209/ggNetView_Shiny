# Run with: R_PROFILE_USER=/dev/null /usr/local/bin/Rscript tests/run_shiny_manual_workflow_smoke.R
# or: /usr/local/bin/Rscript --vanilla tests/run_shiny_manual_workflow_smoke.R

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
script_path <- if (length(file_arg)) {
  normalizePath(sub("^--file=", "", file_arg[[1]]), mustWork = TRUE)
} else {
  normalizePath(file.path("tests", "run_shiny_manual_workflow_smoke.R"), mustWork = TRUE)
}
repo_root <- normalizePath(file.path(dirname(script_path), ".."), mustWork = TRUE)
setwd(repo_root)
options(ggnetview.app_root = repo_root)

source_repo_file <- function(...) {
  path <- file.path(repo_root, ...)
  if (!file.exists(path)) {
    stop("Cannot source missing file: ", path, call. = FALSE)
  }
  sys.source(path, envir = globalenv())
}

description_imports <- function() {
  desc <- read.dcf(file.path(repo_root, "DESCRIPTION"))[1, , drop = TRUE]
  import_text <- desc[["Imports"]]
  if (is.null(import_text) || is.na(import_text) || !nzchar(import_text)) {
    return(character())
  }

  imports <- unlist(strsplit(import_text, ",", fixed = TRUE), use.names = FALSE)
  imports <- trimws(gsub("\\s*\\([^)]*\\)", "", imports))
  imports[nzchar(imports)]
}

recommended_or_base_packages <- function() {
  installed <- utils::installed.packages()
  rownames(installed)[installed[, "Priority"] %in% c("base", "recommended")]
}

report_missing_imports <- function() {
  imports <- setdiff(description_imports(), recommended_or_base_packages())
  missing <- imports[!vapply(imports, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing)) {
    message(
      "Missing non-base/non-recommended DESCRIPTION Imports: ",
      paste(missing, collapse = ", ")
    )
  } else {
    message("All non-base/non-recommended DESCRIPTION Imports are installed.")
  }
  invisible(missing)
}

try_load_namespace <- function() {
  if (requireNamespace("pkgload", quietly = TRUE)) {
    return(tryCatch(
      {
        pkgload::load_all(repo_root, quiet = TRUE)
        TRUE
      },
      error = function(e) {
        message("Could not load ggNetView namespace with pkgload: ", conditionMessage(e))
        FALSE
      }
    ))
  }

  message("pkgload is not installed; cannot fully load ggNetView namespace for smoke workflow.")
  FALSE
}

assert_app_ok <- function(result, step) {
  if (isTRUE(result$ok)) {
    return(invisible(result$value))
  }

  details <- c(
    paste0(step, " failed"),
    if (!is.null(result$message)) paste0("message: ", result$message),
    if (!is.null(result$trace)) paste0("trace: ", result$trace)
  )
  stop(paste(details, collapse = "\n"), call. = FALSE)
}

assert_file_nonempty <- function(path) {
  if (!file.exists(path) || file.info(path)$size <= 0) {
    stop("Expected non-empty file: ", path, call. = FALSE)
  }
  invisible(path)
}

first_registry_item <- function(registry, type = NULL, name = NULL) {
  listed <- shiny::isolate(registry_list(registry, type = type))
  if (!is.null(name)) {
    listed <- listed[listed$name == name, , drop = FALSE]
  }
  if (!nrow(listed)) {
    stop("No matching registry item found.", call. = FALSE)
  }
  shiny::isolate(registry_get(registry, listed$id[[1]]))
}

read_phase2_fixture <- function(name, row_names = TRUE) {
  path <- file.path(repo_root, "inst", "extdata", name)
  if (row_names) {
    utils::read.csv(path, row.names = 1, check.names = FALSE)
  } else {
    utils::read.csv(path, check.names = FALSE)
  }
}

phase2_env_spec <- function() {
  spec <- t(as.matrix(read_phase2_fixture("phase2_example_matrix.csv")))
  env <- data.frame(
    temperature = c(12, 13, 14, 16, 18),
    pH = c(6.8, 6.9, 7.1, 7.2, 7.4),
    moisture = c(30, 31, 35, 36, 40),
    row.names = rownames(spec),
    check.names = FALSE
  )
  list(spec = as.data.frame(spec, check.names = FALSE), env = env)
}

report_missing_imports()
namespace_loaded <- try_load_namespace()
if (!namespace_loaded) {
  message("Continuing through app adapters to expose the first real workflow failure.")
}

source_repo_file("R", "app_validation.R")
source_repo_file("R", "app_registry.R")
source_repo_file("R", "app_adapters.R")
source_repo_file("R", "app_graph_builders.R")
source_repo_file("R", "app_graph_inspect.R")
source_repo_file("R", "app_topology_adapters.R")
source_repo_file("R", "app_compare_environment.R")
source_repo_file("R", "app_gallery_presets.R")
source_repo_file("R", "app_exports.R")
source_repo_file("R", "apply_transform_method.R")

registry <- registry_new()
register_gallery_examples(registry, root = repo_root)
listed <- shiny::isolate(registry_list(registry))

required_types <- c("matrix", "edge_table", "module_table", "sample_metadata", "adjacency", "wgcna_tom", "result", "graph")
missing_types <- setdiff(required_types, unique(listed$type))
if (length(missing_types)) {
  stop("Gallery did not register required object types: ", paste(missing_types, collapse = ", "), call. = FALSE)
}

matrix_item <- first_registry_item(registry, type = "matrix", name = "gallery_matrix")
sample_metadata_item <- first_registry_item(registry, type = "sample_metadata", name = "gallery_sample_metadata")
graph_item <- first_registry_item(registry, type = "graph", name = "gallery_matrix_graph")
graph <- graph_item$data

graph_info <- safe_graph_info(graph)
assert_app_ok(graph_info, "graph info")

modules <- graph_module_choices(graph)
if (length(modules)) {
  module_subgraph <- safe_module_subgraph(graph, select_module = unname(modules[[1]]))
  assert_app_ok(module_subgraph, "module subgraph")
  selected <- subgraph_selected_graph(module_subgraph$value)
  if (!inherits(selected, "igraph")) {
    stop("Module subgraph did not return an igraph selection.", call. = FALSE)
  }
}

sample_subgraph <- safe_sample_subgraph(
  graph,
  matrix = matrix_item$data,
  select_sample = colnames(matrix_item$data)[[1]],
  min_abundance = 0,
  combine = "union"
)
assert_app_ok(sample_subgraph, "sample subgraph")

plot_nicely <- safe_plot_ggnetview(graph, params = list(layout = "nicely"))
assert_app_ok(plot_nicely, "Visual Lab nicely layout")

plot_circle <- safe_plot_ggnetview(graph, params = list(layout = "circle"))
assert_app_ok(plot_circle, "Visual Lab circle layout")

visual_lab_manual_layouts <- list(
  circle_outline = list(layout = "circle_outline", layout.module = "adjacent"),
  square_outline = list(layout = "square_outline", layout.module = "adjacent"),
  circular_modules_petal = list(layout = "circular_modules_equal_petal_layout", layout.module = "order"),
  circular_modules_star = list(layout = "circular_modules_star_layout", layout.module = "order"),
  rightiso_layers = list(layout = "rightiso_layers", layout.module = "adjacent")
)
for (layout_name in names(visual_lab_manual_layouts)) {
  layout_result <- safe_plot_ggnetview(graph, params = visual_lab_manual_layouts[[layout_name]])
  assert_app_ok(layout_result, paste("Visual Lab manual layout", layout_name))
}

topology <- safe_topology(graph)
assert_app_ok(topology, "network topology")

centrality <- safe_node_centrality(
  graph,
  measures = c("Betweenness", "Closeness", "PageRank"),
  weighted = FALSE
)
assert_app_ok(centrality, "node centrality")

zipi <- safe_zipi(graph)
assert_app_ok(zipi, "Zi-Pi classification")

ivi <- safe_node_ivi(graph, ncores = 1L)
if (!isTRUE(ivi$ok) && !grepl("influential", ivi$trace %||% "", fixed = TRUE)) {
  assert_app_ok(ivi, "node IVI")
}

mat_b <- read_phase2_fixture("phase2_example_matrix_b.csv")
graph_b_result <- safe_graph_builder(
  "matrix",
  inputs = list(matrix = mat_b),
  params = list(method = "cor", r.threshold = 0.2, p.threshold = 1)
)
graph_b <- assert_app_ok(graph_b_result, "second matrix graph")

multi <- safe_multi_network_compare(list(A = graph, B = graph_b))
assert_app_ok(multi, "multi-network comparison")

multi_group <- safe_multi_group_network(
  matrix_item$data,
  group_info = sample_metadata_item$data,
  params = list(r.threshold = 0.2, p.threshold = 1)
)
assert_app_ok(multi_group, "custom metadata grouped network")

env_spec <- phase2_env_spec()
environment <- safe_environment_link(
  env = env_spec$env,
  spec = env_spec$spec,
  env_select = list(Environment = seq_len(ncol(env_spec$env))),
  spec_select = list(Species = seq_len(ncol(env_spec$spec)))
)
assert_app_ok(environment, "environment link")

triple_environment <- safe_environment_triple_heatmap(
  env = env_spec$env,
  experiment = env_spec$spec,
  graph = graph,
  params = list(feature_count = 3L, r = 6)
)
assert_app_ok(triple_environment, "triple environment heatmap")

mantel <- safe_mantel_pairwise(
  spec = env_spec$spec[, 1:2],
  env = env_spec$env[, 1:2],
  params = list(permutations = 9L)
)
if (!isTRUE(mantel$ok) && !(requireNamespace("vegan", quietly = TRUE) == FALSE && grepl("vegan", mantel$trace %||% ""))) {
  assert_app_ok(mantel, "Mantel pairwise")
}

tmpdir <- tempfile("ggnetview-manual-smoke-")
dir.create(tmpdir)
write_registry_object(graph, file.path(tmpdir, "graph.rds"))
write_graph_nodes_csv(graph, file.path(tmpdir, "nodes.csv"))
write_graph_edges_csv(graph, file.path(tmpdir, "edges.csv"))
write_graph_adjacency_csv(graph, file.path(tmpdir, "adjacency.csv"))
write_registry_params(list(builder = "manual_gallery", layout = "nicely"), file.path(tmpdir, "params.json"))
write_plot_png(plot_nicely$value, file.path(tmpdir, "plot.png"), width = 6, height = 4, dpi = 96)

invisible(vapply(
  file.path(tmpdir, c("graph.rds", "nodes.csv", "edges.csv", "adjacency.csv", "params.json", "plot.png")),
  assert_file_nonempty,
  character(1)
))

cat("manual workflow smoke passed\n")
