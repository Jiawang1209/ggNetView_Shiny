stat_graph <- function(graph_obj, mapping_line){

  # node number
  node_n <- graph_obj %>%
    tidygraph::activate(nodes) %>%
    tidygraph::as_tibble() %>%
    nrow()

  # edge number
  edge_n <- graph_obj %>%
    tidygraph::activate(edges) %>%
    tidygraph::as_tibble() %>%
    nrow()

  if (isTRUE(mapping_line)) {
    # position edge number
    position_edge_n <- graph_obj %>%
      tidygraph::activate(edges) %>%
      tidygraph::as_tibble() %>%
      dplyr::filter(corr_direction == "Positive") %>%
      nrow()


    # negative edge number
    negative_edge_n <- graph_obj %>%
      tidygraph::activate(edges) %>%
      tidygraph::as_tibble() %>%
      dplyr::filter(corr_direction == "Negative") %>%
      nrow()

    stat_out <- data.frame(
      node = node_n,
      edge = edge_n,
      position_edge = position_edge_n,
      negative_edge = negative_edge_n
    )

  }else{
    stat_out <- data.frame(
      node = node_n,
      edge = edge_n
    )
  }


  return(stat_out)

}
