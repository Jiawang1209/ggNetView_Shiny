#' Trans adjacency matrix to edge data frame
#'
#' @param adjacency_matrix Numeric matrix.
#' A numeric matrix with adjacency matrix.
#'
#' @returns A data frame with columns `from`, `to`, `weight` describing the
#' edges of the adjacency matrix (one row per edge of the undirected graph).
#' @export
#'
#' @examples
#' data(adjacency_matrix_example)
#' set.seed(1)
#' idx <- sample(ncol(adjacency_matrix_example), 50)
#' edge_df <- trans_adjacency_matrix_to_df(adjacency_matrix_example[idx, idx])
#' head(edge_df)
trans_adjacency_matrix_to_df <- function(adjacency_matrix){
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

  # create igraph object
  g <- igraph::graph_from_adjacency_matrix(adjacency_matrix, weighted = TRUE, mode = 'undirected')

  # remove self correlation
  g <- igraph::simplify(g)

  # delect single node
  g <- igraph::delete_vertices(g, which(igraph::degree(g)==0))

  # The graph was built with `weighted = TRUE`; retain the edge weights so the
  # caller can reconstruct the original matrix entries (previously dropped).
  edge_df <- data.frame(
    igraph::as_edgelist(g),
    weight = igraph::E(g)$weight,
    stringsAsFactors = FALSE
  ) %>%
    purrr::set_names(c("from", "to", "weight"))


  return(edge_df)

}
