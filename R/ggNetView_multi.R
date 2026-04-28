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
#' "holm", "hochberg", "hommel", "bonferroni",
#' "BH", "BY", "fdr", and "none".
#' @param module.method Character.
#' Network community detection (module identification) method.
#' Options include "Fast_greedy", "Walktrap", "Edge_betweenness", and "Spinglass".
#' @param SpiecEasi.method Character.
#' Method used in \code{SpiecEasi} network inference; options include "mb" and "glasso".
#' @param sparcc_R Integer.
#' Number of bootstrap/permutation replicates for SparCC p-values (when \code{method = "SPARCC"}).
#' Default 20.
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
#' @param inner_shrink Numeric (default = 1).
#' Intra-module compactness factor for \code{layout = "WGCNA"} only.
#' See \code{\link{ggNetView}} for details.  Ignored by other layouts.
#' @param k_nn Numeric (default = 8).
#' Number of nearest neighbors used to build the local adjacency graph.
#' @param push_others_delta Numeric (default = 0).
#' Radial offset applied to the "Others" module to slightly
#' @param layout.module Character  (default = "random")
#' - random : modules are distributed more randomly and independently.
#' - adjacent : modules are positioned close to each other, minimizing inter-module gaps.
#' - order : modules are distributed by order, applicable to `Bipartite, Tripartite, Quadripartite, Multipartite, Pentapartite Layout`
#' @param shape Integer  (default = 21).
#' The point shape likely in ggplot2.
#' @param pointalpha Integer  (default = 1).
#' The point alpha
#' @param pointsize Vector (default =  c(1,10))
#' The point size rang.
#' @param pointstroke Integer  (default = 0.3).
#' @param pointlabel Character (default = NULL).
#' Optional node label mode for top Degree nodes within each module.
#' Supported values: \code{"topN"} (e.g. \code{"top1"}, \code{"top7"}, \code{"top20"})
#' and \code{"ALL"}.
#' @param pointlabelsize Integer (default = 5).
#' Change point label size.
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
#' @param label Logical or Character (default = FALSE).
#' Whether to display module labels. If a character string, used as legend prefix.
#' @param labelsize Integer  (default = 10).
#' Change Module label size.
#' @param labelsegmentsize Integer  (default = 1).
#' Change  label segment size.
#' @param labelsegmentalpha Integer  (default = 1).
#' Change  label segment alpha.
#' @param add_group_outer Logical (default = FALSE).
#' Whether to add a circle boundary around the entire network (mimics \code{ggforce::geom_mark_circle}).
#' @param add_group_outer_expand Numeric (default = 2).
#' Expansion in mm for the group circle; passed to \code{geom_mark_circle(expand = ...)}.
#' @param add_group_outer_color Character (default = "grey50").
#' Color of the group outer circle border.
#' @param add_group_outer_fill Character or NULL (default = NULL).
#' Fill color of the group outer circle. \code{NULL} = no fill (transparent).
#' @param add_group_outer_fill_alpha Numeric (default = 0.2).
#' Alpha (transparency) of the group outer circle fill.
#' @param add_group_outer_linetype Integer or character (default = 1).
#' Linetype of the group outer circle (e.g. 1 = solid, 2 = dashed).
#' @param add_group_outer_linewidth Numeric (default = 0.5).
#' Line width of the group outer circle.
#' @param add_outer Logical (default = FALSE).
#' Whether to add an outer circle/border around each module.
#' @param q_outer Numeric (default = 0.88).
#' Quantile of radial distance used to construct the smooth outer boundary for each module.
#' @param expand_outer Numeric (default = 1.02).
#' Global scaling factor applied to the smoothed radial distances when drawing the outer boundary.
#' @param outerwidth Integer  (default = 1.25).
#' Change  outer linewidth.
#' @param outerlinetype Integer  (default = 2).
#' Change  outer linetype.
#' @param outeralpha Integer  (default = 0.5).
#' Change  outer alpha.
#' @param nodelabsize Integer  (default = 5).
#' Change  node label size.
#' @param remove Logical (default = FALSE).
#' Remove nodes that are not modules.
#' @param dropOthers Logical (default = FALSE).
#' If TRUE, remove nodes in the \code{"Others"} module before layout and visualization.
#' @param orientation Character string.
#' Custom orientation; one of "up","down","left","right".
#' @param angle Integer  (default = 0).
#' Change  orientation angle.
#' @param scale Logical  (default = T).
#' modules applicable to `Bipartite, Tripartite, Quadripartite, Multipartite, Pentapartite Layout` to scale the radius
#' @param anchor_dist Integer (default = 6)
#' the distance of each modules, applicable to `Bipartite, Tripartite, Quadripartite, Multipartite, Pentapartite Layout`
#' @param layout_nrow Integer (default = NULL).
#' Number of layout rows passed to \code{ggNetView} when using consensus-module grid layouts.
#' @param layout_ncol Integer (default = NULL).
#' Number of layout columns passed to \code{ggNetView} when using consensus-module grid layouts.
#' @param seed Integer (default = 1115).
#' Random seed for reproducibility.
#' @param nrow Integer (default = NULL).
#' Number of rows in the combined patchwork plot.
#' @param ncol Integer (default = NULL).
#' Number of columns in the combined patchwork plot.
#'
#' @returns  A ggplot object representing the network visualization.
#' @export
#'
#' @examples
#' \dontrun{
#' # `mat` is a numeric matrix (features x samples) and
#' # `group_info` is a data frame with columns Sample and Group.
#' p <- ggNetView_multi(
#'   mat        = mat,
#'   group_info = group_info,
#'   method     = "cor",
#'   layout     = "fr"
#' )
#' }
ggNetView_multi <- function(mat,
                            group_info,
                            transfrom.method = c("none", "scale", "center", "log2", "log10", "ln", "rrarefy",
                                                 "rrarefy_relative"),
                            r.threshold = 0.7,
                            p.threshold = 0.05,
                            method = c("WGCNA", "SpiecEasi", "SPARCC", "cor"),
                            cor.method = c("pearson", "kendall", "spearman"),
                            proc = c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none"),
                            module.method = c("Fast_greedy", "Walktrap", "Edge_betweenness", "Spinglass"),
                            SpiecEasi.method = c("mb", "glasso"),
                            sparcc_R = 20,
                            node_annotation = NULL,
                            top_modules = 15,
                            layout = NULL,
                            node_add = 7,
                            ring_n = NULL,
                            r = 1,
                            center = TRUE,
                            idx = NULL,
                            shrink = 1,
                            inner_shrink = 1,
                            k_nn = 12,
                            push_others_delta = 0,
                            layout.module = c("random", "adjacent", "order"),
                            shape = 21,
                            pointalpha = 1,
                            pointsize = c(1,10),
                            pointstroke = 0.3,
                            pointlabel = NULL,
                            pointlabelsize = 5,
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
                            add_group_outer = FALSE,
                            add_group_outer_expand = 2,
                            add_group_outer_color = "grey50",
                            add_group_outer_fill = NULL,
                            add_group_outer_fill_alpha = 0.2,
                            add_group_outer_linetype = 1,
                            add_group_outer_linewidth = 0.5,
                            add_outer = FALSE,
                            q_outer = 0.88,
                            expand_outer = 1.02,
                            outerwidth = 1.25,
                            outerlinetype = 2,
                            outeralpha = 0.5,
                            nodelabsize = 5,
                            remove = FALSE,
                            dropOthers = FALSE,
                            orientation = "up",
                            angle = 0,
                            scale = T,
                            anchor_dist = 6,
                            layout_nrow = NULL,
                            layout_ncol = NULL,
                            seed = 1115,
                            nrow = NULL,
                            ncol = NULL
                            ){

  method <- match.arg(method)
  p_list <- list()

  for (g in unique(group_info$Group)) {
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
      sparcc_R = sparcc_R,
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
      inner_shrink = inner_shrink,
      k_nn = k_nn,
      push_others_delta = push_others_delta,
      layout.module = layout.module,
      shape = shape,
      pointalpha = pointalpha,
      pointsize = pointsize,
      pointstroke = pointstroke,
      pointlabel = pointlabel,
      pointlabelsize = pointlabelsize,
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
      add_group_outer = add_group_outer,
      add_group_outer_expand = add_group_outer_expand,
      add_group_outer_color = add_group_outer_color,
      add_group_outer_fill = add_group_outer_fill,
      add_group_outer_fill_alpha = add_group_outer_fill_alpha,
      add_group_outer_linetype = add_group_outer_linetype,
      add_group_outer_linewidth = add_group_outer_linewidth,
      add_outer = add_outer,
      q_outer = q_outer,
      expand_outer = expand_outer,
      outerwidth = outerwidth,
      outerlinetype = outerlinetype,
      outeralpha = outeralpha,
      nodelabsize = nodelabsize,
      remove = remove,
      dropOthers = dropOthers,
      orientation = orientation,
      angle = angle,
      scale = scale,
      anchor_dist = anchor_dist,
      nrow = layout_nrow,
      ncol = layout_ncol,
      seed = seed
    )

    p_list[[g]] <- p

  }

  p_out <- patchwork::wrap_plots(p_list,
                                 # guides = "collect",
                                 nrow = nrow,
                                 ncol = ncol)

  return(p_out)

}
