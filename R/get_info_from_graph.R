#' Extract node and edge information from graph object
#'
#' @param graph_obj An graph object from build_graph_from_mat or build_graph_from_df.
#' The network object to be visualized.
#'
#' @returns a list
#' @export
#'
#' @examples
#' data(adjacency_matrix_example)
#' set.seed(1)
#' idx <- sample(ncol(adjacency_matrix_example), 80)
#' obj <- build_graph_from_adj_mat(adjacency_matrix_example[idx, idx])
#' info <- get_info_from_graph(obj)
#' names(info)
#' head(info$node_info)
#' head(info$edge_info)
get_info_from_graph <- function(graph_obj){
  # node info
  node_info <-  graph_obj %>%
    tidygraph::activate(nodes) %>%
    tidygraph::as_tibble() %>%
    dplyr::select(-dplyr::any_of(c("modularity", "modularity2", "modularity3")))

  node_info2 <- graph_obj %>%
    tidygraph::activate(nodes) %>%
    tidygraph::as_tibble() %>%
    dplyr::mutate(id = 1:dplyr::n()) %>%
    dplyr::select(name, id)


  # edge info
  edge_cols <- c("name_from", "name_to", "weight", "correlation", "corr_direction")
  edge_info <- graph_obj %>%
    tidygraph::activate(edges) %>%
    tidygraph::as_tibble() %>%
    dplyr::left_join(node_info2,
                     by = c("from" = "id")) %>%
    dplyr::rename(name_from = name) %>%
    dplyr::left_join(node_info2,
                     by = c("to" = "id")) %>%
    dplyr::rename(name_to = name) %>%
    dplyr::select(dplyr::any_of(edge_cols)) %>%
    dplyr::rename(from = name_from,
                  to = name_to)

  return(
    list(node_info = node_info,
         edge_info = edge_info
         )
  )

}
