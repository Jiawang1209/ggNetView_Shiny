#' Build a graph object from Two numeric matrix
#'
#' @param mat1 Numeric matrix.
#' A numeric matrix with samples in colums and variables in rows
#' @param mat2 Numeric matrix.
#' A numeric matrix with samples in colums and variables in rows
#' @param node_annotation Data frame.
#' Optional node annotation table, containing metadata such as taxonomy or functional categories.
#' @param directed Logical (default: \code{FALSE}).
#' Whether edges between nodes are directed.
#' @param top_modules  Integer.
#' Number of top-ranked modules to retain for downstream visualization or analysis.
#' @param seed Integer (default = 1115).
#' Random seed for reproducibility.
#'
#' @returns A graph object representing the correlation-based two numeric matrix.
#' @export
#'
#' @examples NULL
build_graph_from_double_mat_with_module <- function(mat1,
                                                    mat2,
                                                    node_annotation = NULL,
                                                    directed = F,
                                                    top_modules = 15,
                                                    seed = 1115){

  df1 = mat1 %>% t() %>% as.data.frame()
  df2 = mat2 %>% t() %>% as.data.frame()

  cor_out_odata <- psych::corr.test(df1, df2)

  cor_out_odata_r <- cor_out_odata$r %>%
    as.data.frame() %>%
    tibble::rownames_to_column(var = "from") %>%
    tidyr::pivot_longer(cols = -from, names_to = "to", values_to = "Correlation")

  cor_out_odata_p <- cor_out_odata$p %>%
    as.data.frame() %>%
    tibble::rownames_to_column(var = "from") %>%
    tidyr::pivot_longer(cols = -from, names_to = "to", values_to = "Pvalue") %>%
    dplyr::mutate(signif = case_when(
      Pvalue > 0.05 ~ "",
      Pvalue > 0.01 & Pvalue < 0.05 ~ "*",
      Pvalue < 0.01 & Pvalue > 0.001 ~ "**",
      Pvalue < 0.001 ~ "***"
    ))

  cor_out <- cor_out_odata_r %>%
    dplyr::left_join(cor_out_odata_p, by = c("from", "to")) %>%
    purrr::set_names(c("from", "to", "Correlation", "Pvalue", "Signif")) %>%
    dplyr::mutate(Correlated = case_when(
      Correlation > 0 ~ "Positive",
      Correlation < 0 ~ "Negative"
    )) %>%
    dplyr::mutate(Signif2 = case_when(
      Pvalue > 0.05 ~ "P > 0.05",
      Pvalue > 0.01 & Pvalue < 0.05 ~ "0.01 < P < 0.05",
      Pvalue < 0.01 & Pvalue > 0.001 ~ "0.001 < P < 0.01",
      Pvalue < 0.001 ~ "P < 0.001"
    ))

  df <- cor_out %>%
    dplyr::select(from, to, Correlation) %>%
    dplyr::rename(weight = Correlation)

  g <- igraph::graph_from_data_frame(
    d = df,
    vertices = node_annotation,
    directed = directed
  )

  # 构建igraph对象
  g <- igraph::graph_from_data_frame(
    d = df,
    vertices = node_annotation,
    directed = F
  )

  # 删除自相关
  g <- igraph::simplify(g)

  # 删除孤立节点
  g <- igraph::delete_vertices(g, which(igraph::degree(g)==0))

  ## 设置网络的weight，为计算模块性做准备
  igraph::E(g)$correlation <- igraph::E(g)$weight
  igraph::E(g)$weight <- abs(igraph::E(g)$weight)
  igraph::E(g)$corr_direction <- ifelse(igraph::E(g)$correlation > 0, "Positive", "Negative")

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

