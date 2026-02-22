#' Extract node and edge information from graph object
#'
#' @param graph_obj An graph object from build_graph_from_mat or build_graph_from_df.
#' The network object to be visualized.
#'
#' @returns a list
#' @export
#'
#' @examples NULL
get_info_from_graph <- function(graph_obj){
  # node info
  node_info <-  graph_obj %>%
    tidygraph::activate(nodes) %>%
    tidygraph::as_tibble() %>%
    dplyr::select(-modularity,
                  -modularity2,
                  -modularity3
                  )

  node_info2 <- graph_obj %>%
    tidygraph::activate(nodes) %>%
    tidygraph::as_tibble() %>%
    dplyr::mutate(id = 1:dplyr::n()) %>%
    dplyr::select(name, id)


  # edge info
  edge_info <- graph_obj %>%
    tidygraph::activate(edges) %>%
    tidygraph::as_tibble() %>%
    dplyr::left_join(node_info2,
                     by = c("from" = "id")) %>%
    dplyr::rename(name_from = name) %>%
    dplyr::left_join(node_info2,
                     by = c("to" = "id")) %>%
    dplyr::rename(name_to = name) %>%
    dplyr::select(name_from, name_to, weight, correlation, corr_direction) %>%
    dplyr::rename(from = name_from,
                  to = name_to)

  return(
    list(node_info = node_info,
         edge_info = edge_info
         )
  )

}
