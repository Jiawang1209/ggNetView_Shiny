#' Visualize network with custom layouts in different samples
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
#' @param p.threshold p.threshold
#' Significance threshold for correlations; edges are kept only if p < p.threshold.
#' @param method Character.
#' Relationship analysis methods.
#' Options include: "WGCNA", "SpiecEasi", "SPARCC" and "cor".
#' @param cor.method Character.
#' Correlation analysis method.
#' Options include "pearson", "kendall", and "spearman".
#' @param proc Character.
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
#' @param shape Integer  (default = 21).
#' The point shape likely in ggplot2.
#' @param pointalpha Integer  (default = 1).
#' The point alpha
#' @param pointsize Vector (default =  c(1,10))
#' The point size rang.
#' @param pointstroke Integer  (default = 0.3).
#' @param group.by Character (default = "Modularity").
#' Change group for nodes
#' @param fill.by Character (default = "Modularity").
#' Change fill for nodes
#' @param color.by Character (default = NULL).
#' Change color for nodes
#' @param fill Named vector of colors for node fill.
#' If \code{NULL} (default), uses \code{scale_fill_ggnetview}; if provided, uses \code{scale_fill_manual(values = fill)}.
#' @param color Named vector of colors for node/edge/label color.
#' If \code{NULL} (default), uses \code{scale_color_ggnetview}; if provided, uses \code{scale_color_manual(values = color)}.
#' @param jitter Logical (default = FALSE).
#' Whether to apply jitter to points.
#' @param jitter_sd  Integer  (default = 0.1).
#' The standard deviation of the jitter applied when `jitter = TRUE`.
#' @param plot_line  Logical (default = TRUE).
#' Whether to plot line in net plot.
#' @param mapping_line  Logical (default = FALSE).
#' Whether to mapping line in ggNetView.
#' @param curve  Logical (default = FALSE).
#' Whether to plot curve line in net plot.
#' @param curvature Integer (default = 0.25)
#' The curve level of curve line when curve is TRUE
#' @param linealpha  Integer  (default = 0.25).
#' Change  line alpha.
#' @param linecolor Character  (default = "grey70").
#' Change  line color.
#' @param label Logical (default = FALSE).
#' Whether to display node labels in the center points.
#' @param labelsize Integer  (default = 10).
#' Change Module label size.
#' @param labelsegmentsize Integer  (default = 1).
#' Change  label segment size.
#' @param labelsegmentalpha Integer  (default = 1).
#' Change  label segment alpha.
#' @param add_outer Logical (default = FALSE).
#' Whether to add an outer circle/border around the layout.
#' @param outerwidth Integer  (default = 1.25).
#' Change  outer linewidth.
#' @param outerlinetype Integer  (default = 2).
#' Change  outer linetype.
#' @param outeralpha Integer  (default = 0.5).
#' Change  outer alpha.
#' @param nodelabsize Integer  (default = 5).
#' Change  node label size.
#' @param remove Logical (default = FALSE).
#' Delect nodes that are not modules.
#' @param orientation Character string.
#' Custom orientation; one of "up","down","left","right".
#' @param angle Integer  (default = 0).
#' Change  orientation angle.
#' @param scale Logical  (default = T).
#' modules applicable to `Bipartite, Tripartite, Quadripartite, Multipartite, Pentapartite Layout` to scale the radius
#' @param anchor_dist Integer (default = 6)
#' the distance of each modules, applicable to `Bipartite, Tripartite, Quadripartite, Multipartite, Pentapartite Layout`
#' @param seed Integer (default = 1115).
#' Random seed for reproducibility.
#' @param nrwo Integer (default = NULL).
#' Then nrow of combinme plot
#'
#' @returns  A ggplot object representing the network visualization.
#' @export
#'
#' @examples NULL
ggNetView_multi <- function(mat,
                            group_info,
                            transfrom.method = c("none", "scale", "center", "log2", "log10", "ln", "rrarefy",
                                                 "rrarefy_relative"),
                            r.threshold = 0.7,
                            p.threshold = 0.05,
                            method = c("WGCNA", "SpiecEasi", "SPARCC", "cor"),
                            cor.method = c("pearson", "kendall", "spearman"),
                            proc = c("Bonferroni", "Holm", "Hochberg", "SidakSS", "SidakSD", "BH", "BY", "ABH",
                                     "TSBH"),
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
                            shape = 21,
                            pointalpha = 1,
                            pointsize = c(1,10),
                            pointstroke = 0.3,
                            group.by = "Modularity",
                            fill.by = "Modularity",
                            color.by = NULL,
                            fill = NULL,
                            color = NULL,
                            jitter = FALSE,
                            jitter_sd = 0.1,
                            plot_line = TRUE,
                            mapping_line = FALSE,
                            curve = F,
                            curvature = 0.25,
                            linealpha = 0.25,
                            linecolor = "grey70",
                            label = FALSE,
                            labelsize = 10,
                            labelsegmentsize = 1,
                            labelsegmentalpha = 1,
                            add_outer = FALSE,
                            outerwidth = 1.25,
                            outerlinetype = 2,
                            outeralpha = 0.5,
                            nodelabsize = 5,
                            remove = FALSE,
                            orientation = "up",
                            angle = 0,
                            scale = T,
                            anchor_dist = 6,
                            seed = 1115,
                            nrow = NULL
                            ){

  p_list <- list()

  for (g in unique(group_info$Group)) {
    print(g)
    group_info_sub <- group_info %>%
      dplyr::filter(Group %in% g)

    mat_sub <- mat %>%
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

    p <- ggNetView(
      graph_obj = graph,
      layout = layout,
      node_add = node_add,
      ring_n = ring_n,
      r = r,
      center = center,
      idx = idx,
      shrink = shrink,
      k_nn = k_nn,
      push_others_delta = push_others_delta,
      layout.module = layout.module,
      shape = shape,
      pointalpha = pointalpha,
      pointsize = pointsize,
      pointstroke = pointstroke,
      group.by = group.by,
      fill.by = fill.by,
      color.by = color.by,
      fill = fill,
      color = color,
      jitter = jitter,
      jitter_sd = jitter_sd,
      plot_line = plot_line,
      mapping_line = mapping_line,
      curve = curve,
      curvature = curvature,
      linealpha = linealpha,
      linecolor = linecolor,
      label = label,
      labelsize = labelsize,
      labelsegmentsize = labelsegmentsize,
      labelsegmentalpha = labelsegmentalpha,
      add_outer = add_outer,
      outerwidth = outerwidth,
      outerlinetype = outerlinetype,
      outeralpha = outeralpha,
      nodelabsize = nodelabsize,
      remove = remove,
      orientation = orientation,
      angle = angle,
      scale = scale,
      anchor_dist = anchor_dist,
      seed = seed
    )

    p_list[[g]] <- p

  }

  p_out <- patchwork::wrap_plots(p_list,
                                 # guides = "collect",
                                 nrow = nrow)

  return(p_out)

}
