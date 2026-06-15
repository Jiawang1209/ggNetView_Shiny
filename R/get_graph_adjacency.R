#' Extract adjacency matrix from graph object
#'
#' Returns the adjacency matrix of the graph with node names as rownames and
#' colnames. Non-zero entries indicate edges; values correspond to edge weights
#' when the graph is weighted.
#'
#' @param graph_obj A graph object from \code{build_graph_from_mat} or \code{build_graph_from_df}.
#'
#' @returns A numeric matrix with rownames and colnames set to node IDs (\code{name}).
#'
#' @export
#'
#' @examples
#' data(ppi_example)
#' obj <- build_graph_from_df(
#'   df              = ppi_example$ppi,
#'   node_annotation = ppi_example$annotation
#' )
#' adj <- get_graph_adjacency(obj)
#' dim(adj)
#' adj[1:3, 1:3]
get_graph_adjacency <- function(graph_obj) {
  nodes <- tidygraph::as_tibble(graph_obj)
  ig <- tidygraph::as.igraph(graph_obj)
  weight_attr <- if ("weight" %in% igraph::edge_attr_names(ig)) "weight" else NULL
  adj_mat <- as.matrix(igraph::as_adjacency_matrix(ig, attr = weight_attr, sparse = FALSE))
  rownames(adj_mat) <- colnames(adj_mat) <- nodes$name
  adj_mat
}
