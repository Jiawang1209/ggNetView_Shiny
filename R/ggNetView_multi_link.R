#' Visualize multiple networks and show the connections between them to highlight key members within the networks
#'
#' @param mat Numeric matrix.
#' A numeric matrix with samples in rows and variables in columns.
#' @param group_info DataFrame
#' The group information contains: Sample and Group
#' @param transfrom.method Character.
#'Data transformation methods applied before correlation analysis.
#' Options include:
#' "none" (raw data),
#' "scale" (z-score standardization),
#' "center" (mean centering only),
#' "log2" (log2 transfrom),
#' "log10" (log10 transfrom),
#' "ln" (natural transfrom ),
#' "rrarefy" (random rarefaction using \code{vegan::rrarefy}),
#' "rrarefy_relative" (rarefy then convert to relative abundance).
#' @param r.threshold Numeric.
#' Correlation coefficient threshold; edges are kept only if |r| >= r.threshold.
#' @param p.threshold Numeric.
#' #' Significance threshold for correlations; edges are kept only if p < p.threshold.
#' @param method Character.
#' Relationship analysis methods.
#' Options include: "WGCNA", "SpiecEasi", "SPARCC" and "cor".
#' @param cor.method Character.
#' Correlation analysis method.
#' Options include "pearson", "kendall", and "spearman".
#' @param proc  Character.
#' Correlation p-value adjustment methods.
#' Options include:
#' "Bonferroni", "Holm", "Hochberg", "
#' SidakSS", "SidakSD","BH",
#' "BY", "ABH", and "TSBH".
#' @param module.method Character.
#' Network community detection (module identification) method.
#' Options include "Fast_greedy", "Walktrap", "Edge_betweenness", and "Spinglass".
#' @param SpiecEasi.method Character.
#' Method used in \code{SpiecEasi} network inference; options include "mb" and "glasso".
#' @param node_annotation Data frame.
#' Optional node annotation table, containing metadata such as taxonomy or functional categories.
#' @param top_modules Integer.
#' Number of top-ranked modules to retain for downstream visualization or analysis.
#' @param layout Character string.
#' Custom layouts; one of "gephi", "square", "square2", "petal",
#' "petal2", "heart_centered","diamond", "star", "star_concentric","rectangle,
#' "rightiso_layers" etc.
#' @param node_add Integer (default = 7).
#' Number of nodes to add in each layer of the layout.
#' @param ring_n Numeric (default = 7)
#' Numbers of ring in rings layout.
#' @param r Numeric (default = 1).
#' Radius increment for concentric or layered layouts.
#' @param center Logical (default = TRUE).
#' Whether to place a node at the center of the layout.
#' @param idx Optional.
#' Index of nodes to be emphasized or centered in the layout
#' @param shrink Numeric (default = 1).
#' Shrinkage factor applied to the center points.
#' @param k_nn Numeric (default = 8).
#' Number of nearest neighbors used to build the local adjacency graph.
#' @param push_others_delta Numeric (default = 0).
#' Radial offset applied to the "Others" module to slightly
#' @param layout.module Character  (default = "random")
#‘ - random : modules are distributed more randomly and independently.
#' - adjacent : modules are positioned close to each other, minimizing inter-module gaps.
#' - order : modules are distributed by order, applicable to `Bipartite, Tripartite, Quadripartite, Multipartite, Pentapartite Layout`
#' @param pointstroke Integer  (default = 0.3).
#' @param group.by Character (default = "Modularity").
#' Change group for nodes
#' @param fill.by Character (default = "Modularity").
#' Change fill for nodes
#' @param jitter Logical (default = FALSE).
#' Whether to apply jitter to points.
#' @param jitter_sd  Integer  (default = 0.1).
#' The standard deviation of the jitter applied when `jitter = TRUE`.
#' @param mapping_line Logical (default = FALSE).
#' Whether to mapping line in ggNetView.
#' @param linealpha  Integer  (default = 0.25).
#' Change  line alpha.
#' @param linecolor Character  (default = "grey70").
#' Change  line color.
#' @param scale Logical  (default = T).
#' modules applicable to `Bipartite, Tripartite, Quadripartite, Multipartite, Pentapartite Layout` to scale the radius
#' @param orientation Character string.
#' Custom orientation; one of "up","down","left","right".
#' @param angle Integer  (default = 0).
#' Change  orientation angle.
#' @param anchor_dist Integer (default = 2)
#' the distance of each modules, applicable to `Bipartite, Tripartite, Quadripartite, Multipartite, Pentapartite Layout`
#' @param seed Integer (default = 1115).
#' Random seed for reproducibility.
#'
#' @returns A ggplot object representing the network visualization.
#' @export
#'
#' @examples NULL
ggNetView_multi_link <- function(mat,
                                 group_info,
                                 transfrom.method = c("none", "scale", "center", "log2", "log10", "ln", "rrarefy", "rrarefy_relative"),
                                 r.threshold = 0.7,
                                 p.threshold = 0.05,
                                 method = c("WGCNA", "SpiecEasi", "SPARCC", "cor"),
                                 cor.method = c("pearson", "kendall", "spearman"),
                                 proc = c("Bonferroni", "Holm", "Hochberg", "SidakSS", "SidakSD", "BH", "BY", "ABH", "TSBH"),
                                 module.method = c("Fast_greedy", "Walktrap", "Edge_betweenness", "Spinglass"),
                                 SpiecEasi.method = c("mb", "glasso"),
                                 node_annotation = NULL,
                                 top_modules = 15,
                                 layout = NULL,
                                 node_add = 7,
                                 ring_n = NULL,
                                 r = 1,
                                 center = TRUE,
                                 idx = NULL,
                                 shrink = 1,
                                 k_nn = 12,
                                 push_others_delta = 0,
                                 layout.module = c("random", "adjacent", "order"),
                                 group.by = "Modularity",
                                 fill.by = "Modularity",
                                 jitter = FALSE,
                                 jitter_sd = 0.01,
                                 mapping_line = FALSE,
                                 linealpha = 0.25,
                                 linecolor = "grey70",
                                 scale = T,
                                 orientation = "up",
                                 angle = 0,
                                 anchor_dist = 6,
                                 seed = 1115
){
  # 参数

  color_v <- c('#1f78b4', '#e7298a', '#41ab5d', '#807dba',
               '#fb9a99', '#4eb3d3', '#bc80bd', '#e31a1c',
               '#a6cee3', '#b2df8a', '#33a02c', '#fdbf6f',
               '#ff7f00', '#cab2d6', '#6a3d9a', '#ffff99',
               '#8dd3c7', '#ffffb3', '#bebada', '#fb8072',
               '#80b1d3', '#fdb462', '#b3de69', '#fccde5',
               '#d9d9d9', '#bc80bd', '#ccebc5', '#ffed6f'
               )

  graph_list <- list()

  graph_info <- list()

  graph_stat <- list()

  group_info = group_info

  for (g in unique(group_info$Group)) {
    print(g)
    group_info_sub <- group_info %>%
      dplyr::filter(Group %in% g)

    mat_sub <- mat %>%
      as.data.frame() %>%
      dplyr::select(all_of(group_info_sub$Sample)) %>%
      tibble::rownames_to_column(var = "ID") %>%
      dplyr::rowwise() %>%
      dplyr::mutate(sum = sum(dplyr::c_across(where(is.numeric)))) %>%
      dplyr::ungroup() %>%
      dplyr::filter(sum != 0) %>%
      dplyr::select(-sum) %>%
      tibble::column_to_rownames(var = "ID")

    graph <- build_graph_from_mat(
      mat = mat_sub,
      transfrom.method = transfrom.method,
      r.threshold = r.threshold,
      p.threshold = p.threshold,
      method = method,
      cor.method = cor.method,
      proc = proc,
      module.method = module.method,
      SpiecEasi.method = SpiecEasi.method,
      node_annotation = node_annotation,
      top_modules = top_modules,
      seed = seed
    )

    # 添加布局
    # find layout function
    func_name <- paste0("create_layout_", layout)

    # find layout functions from ggNetView package
    lay_func <- utils::getFromNamespace(func_name, "ggNetView")

    # get ly1
    ly1 = lay_func(graph_obj = graph,
                   node_add = node_add,
                   r = r,
                   scale = scale,
                   anchor_dist = anchor_dist,
                   orientation = orientation,
                   angle = angle)

    # get ly1_1
    # 圆形布局 添加模块化 获取模块
    if (layout.module == "random") {
      ly1_1 <- module_layout(graph,
                             layout = ly1,
                             center = center,
                             idx = idx,
                             shrink = shrink,
                             jitter,
                             jitter_sd# ,
                             # seed = seed
      )
    }

    if (layout.module == "adjacent") {
      ly1_1 <- module_layout3(graph,
                              layout = ly1,
                              center = center,
                              k_nn = k_nn,
                              push_others_delta = push_others_delta,
                              shrink = shrink,
                              jitter,
                              jitter_sd
                              # seed = seed
      )
    }

    if (layout.module == "order" & func_name != "create_layout_rings") {
      ly1_1 <- module_layout4(graph,
                              layout = ly1,
                              center = center,
                              k_nn = k_nn,
                              push_others_delta = push_others_delta,
                              shrink = shrink,
                              jitter,
                              jitter_sd
                              # seed = seed
      )
    }

    if (layout.module == "order" & func_name == "create_layout_multirings") {
      ly1_1 <- module_layout5(graph,
                              layout = ly1,
                              center = center,
                              k_nn = k_nn,
                              push_others_delta = push_others_delta,
                              shrink = shrink,
                              jitter,
                              jitter_sd
                              # seed = seed
      )
    }

    graph_list[[g]] <- graph

    graph_info[[g]] <- ly1_1$ggplot_data

    graph_stat[[g]] <- stat_graph(graph, mapping_line)
  }

  graph_list_length <- length(graph_list)
  graph_info_length <- length(graph_info)
  graph_stat_length <- length(graph_stat)

  # 首先对3个进行比较
  names(graph_list)

  # 获区比较的分组
  compare_matrix <- utils::combn(names(graph_list), 2)

  compare_out_list <- list()
  for (i in 1:dim(compare_matrix)[2]) {
    print(compare_matrix[1,i])
    print(compare_matrix[2,i])
    tmp <- compare_modules_by_overlap(graph_info[[compare_matrix[1,i]]]$ggplot_node_df %>%
                                        dplyr::select(name, Modularity) %>%
                                        dplyr::mutate(Group = compare_matrix[1,i]),
                                      graph_info[[compare_matrix[2,i]]]$ggplot_node_df %>%
                                        dplyr::select(name, Modularity) %>%
                                        dplyr::mutate(Group = compare_matrix[2,i])) %>%
      dplyr::mutate(Group = stringr::str_c(compare_matrix[1,i],
                                           "to",
                                           compare_matrix[2,i],
                                           sep = "_"))

    compare_out_list[[stringr::str_c(compare_matrix[1,i],
                                     "to",
                                     compare_matrix[2,i],
                                     sep = "_")]] <- tmp

  }

  Module_information <- do.call(rbind, compare_out_list) %>%
    dplyr::filter(pvalue < 0.05) %>%
    tidyr::separate(col = Group, sep = "_", into = c("GroupA", "to", "GroupB"), remove = F) %>%
    dplyr::select(-to)

  Module_information

  # if (isTRUE(scale)) {
  #   anchor_dist = 1
  # }else{
  #   anchor_dist = anchor_dist * 30
  # }

  # 寻找点
  angles <- pi/2 - 2 * pi * (0:(graph_list_length - 1)) / graph_list_length
  anchors <- lapply(angles, function(a) {
    c(anchor_dist * cos(a), anchor_dist * sin(a))
  })

  anchors_df <- do.call(rbind, anchors) %>%
    as.data.frame() %>%
    purrr::set_names(c("x", "y"))


  # 是否是等齐的, 同时进行位置调整
  if (isTRUE(scale)) {
    # 如果需要做标准化的话, 把坐标都要归一化一下
    for (i in seq_along(names(graph_info))) {
      graph_info[[i]]$ggplot_node_df <- graph_info[[i]]$ggplot_node_df %>%
        dplyr::mutate(Group = names(graph_info)[i]) %>%
        # dplyr::select(name, x, y, Group) %>%
        dplyr::mutate(
          ymin = min(y),
          ymax = max(y),
          xmin = min(x),
          xmax = max(x),
          xmind = (max(x) + min(x)) / 2,
          ymind = (max(y) + min(y)) / 2,
          scale_v = max(xmax - xmin, ymax - ymin),
          x = (x - xmind)/scale_v,
          y = (y - xmind)/scale_v
        ) %>%
        dplyr::mutate(
          x = x + anchors_df[i,1],
          y = y + anchors_df[i,2]
        )

      graph_info[[i]]$ggplot_edge_df <- graph_info[[i]]$ggplot_edge_df %>%
        dplyr::mutate(xmid = unique(graph_info[[i]]$ggplot_node_df$xmind),
                      ymid = unique(graph_info[[i]]$ggplot_node_df$ymind),
                      scale_v = unique(graph_info[[i]]$ggplot_node_df$scale_v),
                      from_x = (from_x - xmid)/scale_v,
                      from_y = (from_y - ymid)/scale_v,
                      to_x = (to_x - xmid)/scale_v,
                      to_y = (to_y - ymid)/scale_v) %>%
        dplyr::mutate(
          from_x = from_x + anchors_df[i,1],
          from_y = from_y + anchors_df[i,2],
          to_x = to_x + anchors_df[i,1],
          to_y = to_y + anchors_df[i,2]
        )

    }
    # jitter TRUE
    if (isTRUE(jitter)) {
      for (i in seq_along(names(graph_info))) {
        graph_info[[i]]$ggplot_node_df <- graph_info[[i]]$ggplot_node_df %>%
          dplyr::mutate(
            x = x + stats::rnorm(dplyr::n(), mean = 0, sd = jitter_sd),
            y = y + stats::rnorm(dplyr::n(), mean = 0, sd = jitter_sd)
          )

        graph_info[[i]]$ggplot_edge_df <- graph_info[[i]]$ggplot_edge_df %>%
          dplyr::select(from, to, weight, correlation, corr_direction, from_id, to_id) %>%
          dplyr::left_join(graph_info[[i]]$ggplot_node_df %>%
                             dplyr::select(name, x, y),
                           by = c("from_id" = "name")) %>%
          dplyr::rename(from_x = x,
                        from_y = y) %>%
          dplyr::left_join(graph_info[[i]]$ggplot_node_df %>%
                             dplyr::select(name, x, y),
                           by = c("to_id" = "name")) %>%
          dplyr::rename(to_x = x,
                        to_y = y)
      }
    }
  }

  if (!isTRUE(scale)) {
    for (i in seq_along(names(graph_info))) {
      graph_info[[i]]$ggplot_node_df <- graph_info[[i]]$ggplot_node_df %>%
        dplyr::mutate(
          x = x + anchors_df[i,1],
          y = y + anchors_df[i,2]
        )

      graph_info[[i]]$ggplot_edge_df <- graph_info[[i]]$ggplot_edge_df %>%
        dplyr::select(from, to, weight, correlation, corr_direction, from_id, to_id) %>%
        dplyr::left_join(graph_info[[i]]$ggplot_node_df %>%
                           dplyr::select(name, x, y),
                         by = c("from_id" = "name")) %>%
        dplyr::rename(from_x = x,
                      from_y = y) %>%
        dplyr::left_join(graph_info[[i]]$ggplot_node_df %>%
                           dplyr::select(name, x, y),
                         by = c("to_id" = "name")) %>%
        dplyr::rename(to_x = x,
                      to_y = y)

    # jitter TRUE
    if (isTRUE(jitter)) {
      for (i in seq_along(names(graph_info))) {
        graph_info[[i]]$ggplot_node_df <- graph_info[[i]]$ggplot_node_df %>%
          dplyr::mutate(
            x = x + stats::rnorm(dplyr::n(), mean = 0, sd = jitter_sd),
            y = y + stats::rnorm(dplyr::n(), mean = 0, sd = jitter_sd)
          )

        graph_info[[i]]$ggplot_edge_df <- graph_info[[i]]$ggplot_edge_df %>%
          dplyr::select(from, to, weight, correlation, corr_direction, from_id, to_id) %>%
          dplyr::left_join(graph_info[[i]]$ggplot_node_df %>%
                             dplyr::select(name, x, y),
                           by = c("from_id" = "name")) %>%
          dplyr::rename(from_x = x,
                        from_y = y) %>%
          dplyr::left_join(graph_info[[i]]$ggplot_node_df %>%
                             dplyr::select(name, x, y),
                           by = c("to_id" = "name")) %>%
          dplyr::rename(to_x = x,
                        to_y = y)
       }
     }
    }
  }




  # plot data
  # 那么可以直接进行可视化了
  # raw data to plot data

  p <- ggplot()

  # 我们先添加点和线
  for (index in seq_along(names(graph_info))) {
    print(index)
    print(names(graph_info[index]))
    # plot link
    p <- p +
      ggnewscale::new_scale_fill() +
      ggplot2::geom_segment(data = graph_info[[index]]$ggplot_edge_df,
                            mapping = ggplot2::aes(x = from_x,
                                                   xend = to_x,
                                                   y = from_y,
                                                   yend = to_y),
                            color = linecolor,
                            alpha = linealpha
                            ) +
      ggplot2::geom_point(data = graph_info[[index]]$ggplot_node_df,
                          mapping = ggplot2::aes(x = x,
                                                 y = y,
                                                 fill = Modularity,
                                                 size = Degree),
                          shape = 21) +
      ggnewscale::new_scale_fill() +
      ggforce::geom_mark_circle(data = graph_info[[index]]$ggplot_node_df %>%
                                  dplyr::filter(name %in% (graph_list[[index]] %>%
                                                             tidygraph::activate(nodes) %>%
                                                             tidygraph::as_tibble() %>%
                                                             dplyr::mutate(Modularity = as.character(Modularity)) %>%
                                                             dplyr::filter(Modularity %in% (
                                                               Module_information %>%
                                                                 dplyr::filter(str_detect(Group, pattern = names(graph_list)[index])) %>%
                                                                 dplyr::filter(GroupA == names(graph_list)[index] | GroupB == names(graph_list)[index]) %>%
                                                                 dplyr::mutate(mod_target = dplyr::case_when(
                                                                   GroupA == names(graph_list)[index] ~ modA,
                                                                   GroupB == names(graph_list)[index] ~ modB,
                                                                   .default = NA
                                                                 )) %>%
                                                                 dplyr::pull(mod_target) %>%
                                                                 unique() %>%
                                                                 .[.!= "Others"])) %>%
                                                             dplyr::pull(name))),
                                mapping = aes(x = x,
                                              y = y,
                                              fill = Modularity),
                                n = 100,
                                expand = unit(1, "mm")
                                ) +
      ggplot2::annotate(geom = "text",
                        x = mean(graph_info[[index]]$ggplot_node_df$x),
                        y = max(graph_info[[index]]$ggplot_node_df$y) + anchor_dist/30,
                        label = paste0("Group = ",names(graph_info[index]), "\n",
                                       "Node = ", graph_stat[[index]]$node, "\n",
                                       "Edge = ", graph_stat[[index]]$edge, "\n"),
                        size = 4,
                        fontface = "bold")

  }

  # 增加连线
  tmpi = 1
  for (link_type in names(table(Module_information$Group))) {

    # get module
    modA_tmp <- Module_information %>%
      dplyr::filter(Group == link_type) %>%
      dplyr::filter(modA!= "Others" | modB != "Others") %>%
      dplyr::select(modA, GroupA) %>%
      dplyr::pull(modA)

    GroupA_tmp <- Module_information %>%
      dplyr::filter(Group == link_type) %>%
      dplyr::filter(modA!= "Others" | modB != "Others") %>%
      dplyr::select(modA, GroupA) %>%
      dplyr::pull(GroupA) %>%
      unique()

    modB_tmp <- Module_information %>%
      dplyr::filter(Group == link_type) %>%
      dplyr::filter(modA!= "Others" | modB != "Others") %>%
      dplyr::select(modB, GroupB) %>%
      dplyr::pull(modB)

    GroupB_tmp <- Module_information %>%
      dplyr::filter(Group == link_type) %>%
      dplyr::filter(modA!= "Others" | modB != "Others") %>%
      dplyr::select(modB, GroupB) %>%
      dplyr::pull(GroupB) %>%
      unique()

    # compute location
    Module_location <- Module_information %>%
      dplyr::filter(Group == link_type) %>%
      dplyr::filter(modA!= "Others" | modB != "Others") %>%
      dplyr::left_join(
        graph_info[[GroupA_tmp]]$ggplot_node_df %>%
          dplyr::mutate(Modularity = as.character(Modularity)) %>%
          dplyr::filter(Modularity %in% modA_tmp) %>%
          dplyr::select(x, y, Modularity) %>%
          dplyr::group_by(Modularity) %>%
          dplyr::summarise(x_center = mean(x),
                           y_center = mean(y)) %>%
          purrr::set_names(c("GroupA_Module", "GroupA_x_center", "GroupA_y_center")),
        by = c("modA" = "GroupA_Module")
      ) %>%
      dplyr::left_join(
        graph_info[[GroupB_tmp]]$ggplot_node_df %>%
          dplyr::mutate(Modularity = as.character(Modularity)) %>%
          dplyr::filter(Modularity %in% modB_tmp) %>%
          dplyr::select(x, y, Modularity) %>%
          dplyr::group_by(Modularity) %>%
          dplyr::summarise(x_center = mean(x),
                           y_center = mean(y)) %>%
          purrr::set_names(c("GroupB_Module", "GroupB_x_center", "GroupB_y_center")),
        by = c("modB" = "GroupB_Module")
      )

    Module_location

    p <- p + ggplot2::geom_segment(data = Module_location,
                                   mapping = ggplot2::aes(x = GroupA_x_center,
                                                          xend = GroupB_x_center,
                                                          y = GroupA_y_center,
                                                          yend = GroupB_y_center),
                                   linetype = 2,
                                   linewidth = 1,
                                   color = color_v[tmpi])


    tmpi = tmpi + 1

  }

  p <- p +
    coord_fixed(clip = F) +
    theme_ggnetview() +
    theme(legend.position = "none")


  return(
    list(p = p,
         info = as_tibble(Module_information)
         )
    )


}
