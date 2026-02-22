#' Build a graph object from a data frame and module Info
#'
#' @param df Data frame.
#' Edge list with columns \code{from}, \code{to}, and optionally \code{weight}.
#' If \code{weight} is absent, an unweighted graph is constructed.
#' @param node_annotation Data Frame
#' The annotation file of nodes in network contain \code{node} and \code{Modularity}
#' @param directed Logical (default: \code{FALSE}).
#' Whether edges between nodes are directed.
#' @param top_modules  Integer.
#' Number of top-ranked modules to select.
#' @param seed Integer (default = 1115).
#' Random seed for reproducibility.
#'
#' @returns An graph object representing the correlation network.
#' Node/edge attributes include correlation statistics and (optionally) module labels.
#' @export
#'
#' @examples NULL
build_graph_from_module <- function(df,
                                    node_annotation = NULL,
                                    directed = F,
                                    top_modules = 15,
                                    seed = 1115){

  set.seed(seed)

  # df = ppi_module$ppi
  # node_annotation = ppi_module$annotation
  # directed = F

  # 构建igraph对象
  g <- igraph::graph_from_data_frame(
    d = df,
    vertices = node_annotation,
    directed = directed
  )

  # 删除自相关
  g <- igraph::simplify(g)

  # 删除孤立节点
  g <- igraph::delete_vertices(g, which(igraph::degree(g)==0))

  ## 设置网络的weight，为计算模块性做准备
  igraph::E(g)$correlation <- igraph::E(g)$weight
  igraph::E(g)$weight <- abs(igraph::E(g)$weight)

  # 模块化 是自身提供的

  # igraph::V(g)$modularity  <- membership_vec
  igraph::V(g)$modularity2 <- as.character(igraph::V(g)$Modularity)

  table(igraph::V(g)$modularity2) %>% sort(., decreasing = T)
  factor_levels <- table(igraph::V(g)$modularity2) %>% sort(., decreasing = T) %>% names()

  # max model length
  max_model <- length(table(igraph::V(g)$modularity2) %>% sort(., decreasing = T))

  if (max_model < top_modules) {

    message(paste("The max module in network is", max_model, "we use the", max_model, " modules for next analysis"))
    modularity_top_15 <- igraph::V(g)$modularity2 %>% table() %>% sort(., decreasing = T) %>% .[1:max_model] %>% names()

  }else if (max_model >= top_modules) {

    modularity_top_15 <- igraph::V(g)$modularity2 %>% table() %>% sort(., decreasing = T) %>% .[1:top_modules] %>% names()
  }

  igraph::V(g)$modularity2 <- ifelse(igraph::V(g)$modularity2 %in% modularity_top_15, igraph::V(g)$modularity2, "Others")

  # 构建ggraph对象
  graph_obj <- tidygraph::as_tbl_graph(g) %>%
    tidygraph::mutate(Modularity = factor(Modularity, levels = factor_levels, ordered = T),
                      modularity2 = factor(modularity2, levels = factor_levels, ordered = T),
                      modularity3 = as.character(modularity2),
                      Modularity = modularity2,
                      Degree = tidygraph::centrality_degree(mode = "out"),
                      Segree = tidygraph::centrality_degree(mode = "out"),
                      Strength = tidygraph::centrality_degree(weights = weight)
    ) %>%
    tidygraph::arrange(modularity2, desc(Degree))

  return(graph_obj)
}
