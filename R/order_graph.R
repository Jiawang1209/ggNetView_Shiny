#' Reorder the Modularity from graph object
#'
#' @param graph_obj An graph object from build_graph_from_mat or build_graph_from_df.
#' The network object to be visualized.
#' @param order a character vectors
#'
#' @returns  An graph object with reorder
#' @export
#'
#' @examples
#' data(ppi_example)
#' obj <- build_graph_from_df(
#'   df              = ppi_example$ppi,
#'   node_annotation = ppi_example$annotation
#' )
#' current <- levels(get_graph_nodes(obj)$Modularity)
#' obj2 <- order_graph(obj, order = rev(current))
#' levels(get_graph_nodes(obj2)$Modularity)
order_graph <- function(graph_obj, order){
  graph_obj <- graph_obj %>%
    tidygraph::mutate(Modularity = factor(Modularity,
                                          levels = order,
                                          ordered = T))
  return(graph_obj)
}
