#' Extract node table from graph object
#'
#' Returns the node (vertex) table as a data frame, including attributes such as
#' \code{name}, \code{Modularity}, \code{Degree}, \code{Strength}, etc.
#'
#' @param graph_obj A graph object from \code{build_graph_from_mat} or \code{build_graph_from_df}.
#'
#' @returns A data frame with one row per node; columns include node attributes.
#'
#' @export
#'
#' @examples
#' data(ppi_example)
#' obj <- build_graph_from_df(
#'   df              = ppi_example$ppi,
#'   node_annotation = ppi_example$annotation
#' )
#' head(get_graph_nodes(obj))
get_graph_nodes <- function(graph_obj) {
  as.data.frame(tidygraph::as_tibble(graph_obj))
}
