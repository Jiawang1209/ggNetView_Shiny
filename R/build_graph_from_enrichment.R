#' Build a  graph object from GO enrichment analysis
#'
#' @param df Data frame.
#' The output from GO enrichmen analysis in clusterProfiler
#' @returns An graph object with GO enrichment analysis
#' @export
#'
#' @examples
#' \dontrun{
#' # Requires clusterProfiler enrichment result
#' ego <- clusterProfiler::enrichGO(gene, OrgDb = org.Hs.eg.db, ont = "ALL")
#' obj <- build_graph_from_enrichGO(df = as.data.frame(ego))
#' }
build_graph_from_enrichGO <- function(df){

  # node file
  GO_result_edge <- df %>%
    dplyr::select(ONTOLOGY,ID,Count) %>%
    dplyr::mutate(ID2 = str_c(ONTOLOGY, ID, sep = "_")) %>%
    dplyr::mutate(Type = "GO") %>%
    dplyr::select(Type, everything())

  node_file1 <- GO_result_edge %>%
    dplyr::group_by(Type) %>%
    dplyr::summarise(Type_sum = sum(Count)) %>%
    purrr::set_names(c("node", "node_size")) %>%
    dplyr::mutate(node_level = "Type") %>%
    dplyr::mutate(type = node)

  node_file2 <- GO_result_edge %>%
    dplyr::group_by(ONTOLOGY) %>%
    dplyr::summarise(ONTOLOGY_sum = sum(Count)) %>%
    purrr::set_names(c("node", "node_size")) %>%
    dplyr::mutate(node_level = "ONTOLOGY") %>%
    dplyr::mutate(type = node)

  node_file3 <- GO_result_edge %>%
    dplyr::group_by(ID2) %>%
    dplyr::summarise(ID2_sum = sum(Count)) %>%
    purrr::set_names(c("node", "node_size")) %>%
    dplyr::mutate(node_level = "ID") %>%
    dplyr::mutate(type = str_remove(string = node, pattern = "_.*")) %>%
    dplyr::mutate(node = str_remove(string = node, pattern = "^.*_"))

  node_file <- dplyr::bind_rows(node_file1, node_file2, node_file3)

  # edge file
  # distinct() is what the previous summarise(count = n()) + select(1,2) was
  # actually trying to express; using it directly avoids the silently-dropped
  # `count` column and the unnecessary dplyr grouping.
  edge_file1 <- GO_result_edge %>%
    dplyr::distinct(Type, ONTOLOGY) %>%
    purrr::set_names(c("from", "to"))

  edge_file2 <- GO_result_edge %>%
    dplyr::distinct(ONTOLOGY, ID) %>%
    purrr::set_names(c("from", "to"))

  edge_file <- dplyr::bind_rows(edge_file1, edge_file2)

  # create graph object
  graph_obj <- tidygraph::tbl_graph(node_file,edge_file)

  return(graph_obj)

}
