#' Extract subgrah from graph object
#'
#' @param graph_obj An graph object from build_graph_from_mat or build_graph_from_df.
#' The network object to be visualized.
#' @param select_module a character vectors
#' Select the module name in graph object
#'
#' @returns list
#' @export
#'
#' @examples
#' data(ppi_example)
#' obj <- build_graph_from_df(
#'   df              = ppi_example$ppi,
#'   node_annotation = ppi_example$annotation
#' )
#' sg <- get_subgraph(obj, select_module = "1")
#' names(sg)
#' sg$stat_module
get_subgraph <- function(graph_obj, select_module = NULL){

  # get obj
  obj <- graph_obj

  # validate Modularity column exists
  node_tbl_full <- obj %>%
    tidygraph::activate(nodes) %>%
    tidygraph::as_tibble()

  if (!"Modularity" %in% names(node_tbl_full)) {
    stop("graph_obj must have a 'Modularity' node column.", call. = FALSE)
  }

  # add node index for subgraph extraction
  node_tbl_full$.node_idx <- seq_len(nrow(node_tbl_full))

  # get module name — works for factor, character, or numeric Modularity
  module_name <- sort(unique(as.character(node_tbl_full$Modularity)))

  # split node table by module (group by character Modularity to handle any type)
  split_groups <- dplyr::group_split(node_tbl_full, as.character(Modularity))

  # get module list (node tables per module)
  module_list <- purrr::map(split_groups, ~.x)
  names(module_list) <- module_name

  # get module ID (integer node indices for igraph::subgraph)
  id_list <- purrr::map(split_groups, ~.x$.node_idx)
  names(id_list) <- module_name

  # create sub_graph object
  sub_graph <- list()

  for (i in module_name) {

    sub_graph[[i]] <- igraph::subgraph(tidygraph::as.igraph(obj),
                                       id_list[[i]]) %>%
      tidygraph::as_tbl_graph()

  }

  # stat
  stat_module <- purrr::map(id_list, ~length(.x)) %>%
    do.call(rbind, .) %>%
    as.data.frame() %>%
    tibble::rownames_to_column(var = "Module") %>%
    dplyr::rename(Number = V1)

  # use message() instead of print() so non-interactive use (knitr, plumber,
  # tests) does not have its stdout polluted with the module summary table.
  message(paste(utils::capture.output(stat_module), collapse = "\n"))


  if (!is.null(select_module)) {
    graph_select <- obj %>%
      tidygraph::filter(as.character(Modularity) %in% select_module)

    # warn if the requested module(s) produced an empty subgraph
    if (igraph::gorder(tidygraph::as.igraph(graph_select)) == 0L) {
      warning(
        "select_module '", paste(select_module, collapse = "', '"),
        "' produced an empty subgraph (0 nodes). ",
        "Check that the module name(s) exist in the Modularity column.",
        call. = FALSE
      )
    }
  }else{
    graph_select <- NULL
  }


  return(list(sub_graph_all = sub_graph,
              stat_module = stat_module,
              sub_graph_select = graph_select))

}
