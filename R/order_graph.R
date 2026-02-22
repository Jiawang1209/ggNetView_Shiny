#' Reorder the Modularity from graph object
#'
#' @param graph_obj An graph object from build_graph_from_mat or build_graph_from_df.
#' The network object to be visualized.
#' @param order a character vectors
#'
#' @returns  An graph object with reorder
#' @export
#'
#' @examples NULL
order_graph <- function(graph_obj, order){
  graph_obj <- graph_obj %>%
    tidygraph::mutate(Modularity = factor(Modularity,
                                          levels = order,
                                          ordered = T))
  return(graph_obj)
}
