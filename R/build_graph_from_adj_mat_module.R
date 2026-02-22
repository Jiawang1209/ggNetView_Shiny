#' Build a graph object from a adjacency matrix and module Info
#'
#' @param adjacency_matrix Numeric matrix.
#' A numeric matrix with adjacency matrix.
#' @param node_annotation Data Frame
#' #' The annotation file of nodes in network contain \code{node} and \code{Modularity}
#' @param directed Logical (default: \code{FALSE}).
#' Whether edges between nodes are directed.
#' @param top_modules Integer.
#' Number of top-ranked modules to select.
#' @param seed Integer (default = 1115).
#' Random seed for reproducibility.
#'
#' @returns An graph object representing the correlation network.
#' @export
#'
#' @examples NULL
build_graph_from_adj_mat_module <- function(adjacency_matrix,
                                            node_annotation = NULL,
                                            directed = F,
                                            top_modules = 15,
                                            seed = 1115){
  set.seed(seed)

  # argument check
  if (is.data.frame(adjacency_matrix)){
    adjacency_matrix <- as.matrix(adjacency_matrix)
  }

  if (!is.matrix(adjacency_matrix) || !is.numeric(adjacency_matrix)) {
    stop("`adjacency_matrix` must be numeric matrix.", call. = FALSE)
  }

  if (any(!is.finite(adjacency_matrix))) {
    stop("`adjacency_matrix` dont contain any NA/NaN/Inf. please check mat.", call. = FALSE)
  }

  if (is.null(colnames(adjacency_matrix))) {
    stop("`adjacency_matrix` must contains colnames.", call. = FALSE)
  }

  if (anyDuplicated(colnames(adjacency_matrix))) {
    dup <- unique(colnames(adjacency_matrix)[duplicated(colnames(adjacency_matrix))])
    stop(sprintf("`adjacency_matrix` must contain only colname. The duplicated colname: %s", paste(dup, collapse = ", ")), call. = FALSE)
  }

  if (length(top_modules) != 1L || !is.numeric(top_modules) || top_modules < 1) {
    stop("`top_modules` must be a single numeric value >= 1.", call. = FALSE)
  }
  top_modules <- as.integer(top_modules)
  if (length(seed) != 1L || !is.numeric(seed)) {
    stop("`seed` must be a single numeric.", call. = FALSE)
  }

  if (!is.null(node_annotation)) {
    if (!is.data.frame(node_annotation)) {
      stop("`annotation` must be a data.frame / tibble.", call. = FALSE)
    }
    if (ncol(node_annotation) < 2) {
      stop("`annotation` requires at least two columns (the first is the name, the rest are the annotation columns to be merged).", call. = FALSE)
    }
  }

  # df = ppi_module$ppi
  # node_annotation = ppi_module$annotation
  # directed = F

  # 构建igraph对象
  # create igraph object
  g <- igraph::graph_from_adjacency_matrix(adjacency_matrix, weighted = TRUE, mode = 'undirected')

  tmp_g <- tidygraph::as_tbl_graph(g) %>%
    tidygraph::left_join(node_annotation %>%
                           purrr::set_names(c("name", colnames(node_annotation)[-1])),
                         by = "name")

  g <- tidygraph::as.igraph(tmp_g)

  # remove self correlation
  g <- igraph::simplify(g)

  # delect single node
  g <- igraph::delete_vertices(g, which(igraph::degree(g)==0))

  ## set weight
  igraph::E(g)$correlation <- igraph::E(g)$weight
  igraph::E(g)$weight <- abs(igraph::E(g)$weight)
  igraph::E(g)$corr_direction <- ifelse(igraph::E(g)$correlation > 0, "Positive", "Negative")

  # 模块化 是自身提供的

  # igraph::V(g)$modularity  <- membership_vec
  igraph::V(g)$modularity2 <- as.character(igraph::V(g)$Modularity)

  table(igraph::V(g)$modularity2) %>% sort(., decreasing = T)
  factor_levels <- table(igraph::V(g)$modularity2) %>% sort(., decreasing = T) %>% names()

  # max model length
  max_model <- length(table(igraph::V(g)$modularity2) %>% sort(., decreasing = T))

  if (max_model < top_modules) {

    message(paste("The max module in network is", max_model, "we use the", max_model, " modules for next analysis"))
    modularity_top_15 <- igraph::V(g)$modularity2 %>% table() %>% sort(., decreasing = T) %>% .[1:max_model] %>% names()

  }else if (max_model >= top_modules) {

    modularity_top_15 <- igraph::V(g)$modularity2 %>% table() %>% sort(., decreasing = T) %>% .[1:top_modules] %>% names()
  }

  igraph::V(g)$modularity2 <- ifelse(igraph::V(g)$modularity2 %in% modularity_top_15, igraph::V(g)$modularity2, "Others")

  # 构建ggraph对象
  graph_obj <- tidygraph::as_tbl_graph(g) %>%
    tidygraph::mutate(Modularity = factor(Modularity, levels = factor_levels, ordered = T),
                      modularity2 = factor(modularity2, levels = factor_levels, ordered = T),
                      modularity3 = as.character(modularity2),
                      Modularity = modularity2,
                      Degree = tidygraph::centrality_degree(mode = "out"),
                      Segree = tidygraph::centrality_degree(mode = "out"),
                      Strength = tidygraph::centrality_degree(weights = weight)
    ) %>%
    tidygraph::arrange(modularity2, desc(Degree))

  return(graph_obj)

}
