#' Visualize a network with custom layouts
#'
#' @param graph_obj An graph object from build_graph_from_mat or build_graph_from_df.
#' The network object to be visualized.
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
#' Controls how tightly nodes fill each module's allocated disc during
#' the WGCNA bubble-pack layout: at \code{1} (default) nodes spread to
#' fill 95\% of the disc (original behaviour); smaller values
#' (e.g. \code{0.65}) contract the FR/uniform fill toward each module
#' centre, exposing hub/periphery structure and producing visible
#' inter-module whitespace.  Module disc centres and radii are
#' invariant under \code{inner_shrink}; this parameter only affects
#' the point cloud inside each module.  Ignored by all other layouts.
#' @param k_nn Numeric (default = 8).
#' Number of nearest neighbors used to build the local adjacency graph.
#' @param push_others_delta Numeric (default = 0).
#' Radial offset applied to the "Others" module to slightly
#' @param layout.module Character  (default = "random")
#' - random : modules are distributed more randomly and independently.
#' - adjacent : modules are positioned close to each other, minimizing inter-module gaps.
#' - order : modules are distributed by order, applicable to `Bipartite, Tripartite, Quadripartite, Multipartite, Pentapartite Layout`
#' @param shape Integer  (default = 21).
#' The point shape likely in ggplot2. If a character string is provided,
#' it must be a variable name in graph_object for point shape mapping.
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
#' Change color for nodes. If provided, must be a variable name in
#' graph_object. Numeric uses \code{scale_color_gradient},
#' otherwise uses \code{scale_color_manual} or \code{scale_color_ggnetview}.
#' @param fill Named vector of colors for node fill (e.g. \code{c("M1" = "red", "M2" = "blue")}).
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
#' Whether to mapping line in ggNetView. If a character string is provided,
#' it must be a variable name in graph_object for line color mapping.
#' @param curve  Logical (default = FALSE).
#' Whether to plot curve line in net plot.
#' @param curvature Integer (default = 0.25)
#' The curve level of curve line when curve is TRUE
#' @param linealpha  Integer  (default = 0.25).
#' Change  line alpha.
#' @param linecolor Character  (default = "grey70").
#' Change  line color.
#' @param label Logical or Character (default = \code{FALSE}).
#' Controls module label text and module legend prefix.
#' If \code{FALSE}, module text labels are not drawn and legend prefix uses
#' \code{"Modularity"}.
#' If \code{TRUE}, module text labels are drawn and legend prefix uses
#' \code{"Modularity"}.
#' If a character string, module text labels are drawn and that string is used
#' as prefix for module text and module legend labels.

#' @param labelsize Integer  (default = 10).
#' Change Module label size.
#' @param labelsegmentsize Integer  (default = 1).
#' Change  label segment size.
#' @param labelsegmentalpha Integer  (default = 1).
#' Change  label segment alpha.
#' @param label_layout Character (default = \code{"two_column"}).
#' Strategy used to place module text labels when \code{label} is not
#' \code{FALSE}. One of:
#' \itemize{
#'   \item \code{"two_column"} (default; backward compatible): modules whose
#'     centroid \code{x} is left of the network median go to a fixed left
#'     column at \code{x = xr[1] - dx}, the rest to a fixed right column at
#'     \code{x = xr[2] + dx}, with labels evenly distributed along \code{y}.
#'     Best for very crowded networks or layouts that read left-to-right
#'     (bipartite, grid).
#'   \item \code{"two_column_follow"}: 360-degree ring layout with
#'     two-segment "L-shape" leaders. Modules are sorted by their
#'     actual angle from the network centroid and assigned angularly
#'     equispaced target angles around the full \code{2*pi}, so
#'     labels are evenly distributed around the network while
#'     preserving the angular neighbour order. Each label is
#'     projected onto an outer ellipse whose semi-axes are
#'     \code{(1 + label_outer_pad)} times the network's half-width /
#'     half-height. The leader has two segments: the first leg is
#'     drawn manually via \code{geom_segment} from the module
#'     centroid to an elbow on the network boundary at the module's
#'     actual angle; the second leg is drawn by \code{ggrepel} from
#'     the label (which it may push tangentially with \code{force = 1}
#'     to avoid overlaps) back to that elbow. Use this when you need
#'     the L bend to be clearly visible and labels to never overlap.
#'   \item \code{"label_circle"}: 360-degree ring layout drawn
#'     entirely by \code{ggrepel}. Same equispacing as
#'     \code{"two_column_follow"} (so labels track module angular
#'     order around the outer ellipse), but no manual first leg --
#'     \code{ggrepel} handles both placement and the connecting
#'     segment, with \code{segment.square = TRUE} and
#'     \code{segment.squareShape = 0} so the leader is an L-shape
#'     between label and module centroid. Code is much simpler than
#'     \code{"two_column_follow"} and works well when modules are
#'     roughly evenly distributed around the network; the trade-off
#'     is that the L bend can visually collapse when a label happens
#'     to sit on its module's radial line (rare with full
#'     equispacing). Requires \code{ggrepel >= 0.9.4}.
#' }
#' @param label_wrap_width Integer or NULL (default = \code{NULL}).
#' If a positive integer, module text labels are wrapped to roughly that
#' many characters per line via \code{stringr::str_wrap()} before
#' rendering. Useful for long pathway / taxonomy names. Applies to every
#' \code{label_layout}.
#' @param label_outer_pad Numeric (default = \code{0.25}).
#' Fractional outward push of the label anchors, expressed as a multiple
#' of the network's \code{x}-range (for \code{"two_column"}) or as the
#' fractional enlargement of the outer label-anchor ellipse (for
#' \code{"two_column_follow"}). Larger values move labels further
#' away from the network and make the slanted second leg of the
#' \code{"two_column_follow"} L-shape leader more pronounced. Try
#' \code{0.20} for a tight layout that almost hugs the network,
#' \code{0.40} for the default breathing room, \code{0.55+} when
#' modules sit inside a thick \code{add_group_outer} ring or labels
#' are long.
#' @param add_group_outer Logical (default = FALSE).
#' Whether to add a circle boundary around the entire network (mimics \code{ggforce::geom_mark_circle}).
#' @param add_group_outer_expand Numeric (default = 2).
#' Expansion in mm for the group circle to account for point size; passed to \code{geom_mark_circle(expand = ...)}.
#' @param add_group_outer_color Character (default = "grey50").
#' Color of the group outer circle border.
#' @param add_group_outer_fill Character or NULL (default = NULL).
#' Fill color of the group outer circle. \code{NULL} = no fill (transparent).
#' @param add_group_outer_fill_alpha Numeric (default = 0.2).
#' Alpha (transparency) of the group outer circle fill; 0 = fully transparent, 1 = opaque.
#' @param add_group_outer_linetype Integer or character (default = 1).
#' Linetype of the group outer circle (e.g. 1 = solid, 2 = dashed).
#' @param add_group_outer_linewidth Numeric (default = 0.5).
#' Line width of the group outer circle.
#' @param add_outer Logical (default = FALSE).
#' Whether to draw a smooth outer boundary around each module. The boundary is
#' computed by 2D kernel density estimation followed by a Highest-Density-Region
#' (HDR) contour: it encloses the densest portion of each module rather than
#' all of its points. As a side effect, a single module whose points fall into
#' two well-separated clouds may produce two disconnected polygons (this is by
#' design and matches the behaviour of e.g. `mascarade`).
#' @param q_outer Numeric (default = 0.88).
#' HDR coverage of the outer boundary: the contour is drawn at the density
#' level whose iso-density region contains a fraction `q_outer` of the module's
#' empirical probability mass. Higher values make the boundary more inclusive
#' (closer to the convex hull of the module); lower values make it tighter
#' around the densest core. Sparse outliers/satellites typically fall outside
#' the contour and remain visible as bare nodes.
#' Note: modules with fewer than 10 nodes bypass the KDE path entirely and are
#' enclosed by a convex hull, so `q_outer` (and `bandwidth_scale`) have no
#' effect on them.
#' @param expand_outer Numeric (default = 1.02).
#' Multiplicative scaling applied to each polygon from its own centroid after
#' the HDR contour is drawn. Values > 1 slightly expand the boundary, values
#' < 1 slightly shrink it. Useful for adding a small visual breathing room
#' between the boundary and the nodes.
#' @param bandwidth_scale Numeric (default = 2.0).
#' Multiplier on the robust normal-reference 2D KDE bandwidth used to build the
#' outer boundary. Values > 1 produce smoother, wider contours (and may merge
#' nearby sub-clusters of a module into a single component); values < 1
#' produce tighter, more wiggly contours that follow local point density more
#' closely. Has no effect when \code{add_outer = FALSE} or when a module falls
#' back to the convex-hull path (very small clusters).
#' @param outerwidth Integer  (default = 1.25).
#' Change  outer linewidth.
#' @param outerlinetype Integer  (default = 2).
#' Change  outer linetype.
#' @param outeralpha Integer  (default = 0.5).
#' Change  outer alpha.
#' @param nodelabsize Integer  (default = 5).
#' Change  node label size.
#' @param remove Logical (default = FALSE).
#' Remove \code{"Others"} only at the visualization stage (post-layout),
#' so the layout of remaining modules is kept unchanged.
#' @param dropOthers Logical (default = FALSE).
#' If TRUE, remove nodes in the \code{"Others"} module from \code{graph_obj}
#' before layout and visualization, then recompute layout/plot from the
#' updated graph.
#' @param orientation Character string.
#' Custom orientation; one of "up","down","left","right".
#' @param angle Integer  (default = 0).
#' Change  orientation angle.
#' @param scale Logical  (default = T).
#' modules applicable to `Bipartite, Tripartite, Quadripartite, Multipartite, Pentapartite Layout` to scale the radius
#' @param anchor_dist Integer (default = 6)
#' the distance of each modules, applicable to `Bipartite, Tripartite, Quadripartite, Multipartite, Pentapartite Layout`
#' @param nrow Integer (default = NULL).
#' the nrow of network with layout is "consensus_module_equal_gephi" or "consensus_module_gephi"
#' @param ncol Integer (default = NULL).
#' the ncol of network with layout is "consensus_module_equal_gephi" or "consensus_module_gephi"
#' @param seed Integer (default = 1115).
#' Random seed for reproducibility.
#' @param scale_radius Numeric or NULL (default = NULL).
#' When non-NULL, scale the layout so the network fits within this radius.
#' Used by \code{ggnetview_modularity_heatmaps} for coordinate alignment.
#' @param return_layout Logical (default = FALSE).
#' When TRUE, return a list with \code{$plot} (ggplot) and \code{$layout_data}
#' (graph_ly_final, graph_obj, ggplot_data, module_centroids) for downstream
#' use (e.g. adding heatmaps and links).
#'
#' @returns A ggplot object, or when \code{return_layout = TRUE}, a list with
#' \code{$plot} and \code{$layout_data}.
#' @export
#'
#' @examples
#' data(ppi_example)
#' obj <- build_graph_from_df(
#'   df              = ppi_example$ppi,
#'   node_annotation = ppi_example$annotation,
#'   module.method   = "Fast_greedy",
#'   top_modules     = 5
#' )
#'
#' ggNetView(
#'   graph_obj     = obj,
#'   layout        = "fr",
#'   layout.module = "adjacent",
#'   pointsize     = c(3, 8),
#'   seed          = 1115
#' )
#' \donttest{
#' ggNetView(
#'   graph_obj       = obj,
#'   layout          = "gephi",
#'   layout.module   = "adjacent",
#'   pointsize       = c(3, 8),
#'   label           = TRUE,
#'   add_group_outer = TRUE,
#'   seed            = 1115
#' )
#' }
ggNetView <- function(graph_obj,
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
                      label_layout = c("two_column", "two_column_follow", "label_circle"),
                      label_wrap_width = NULL,
                      label_outer_pad = 0.25,
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
                      bandwidth_scale = 2,
                      outerwidth = 1,
                      outerlinetype = 1,
                      outeralpha = 0.5,
                      nodelabsize = 5,
                      remove = FALSE,
                      dropOthers = FALSE,
                      orientation = "up",
                      angle = 0,
                      scale = T,
                      anchor_dist = 6,
                      nrow = NULL,
                      ncol = NULL,
                      seed = 1115,
                      scale_radius = NULL,
                      return_layout = FALSE
                      ){

  layout.module <- match.arg(layout.module)
  label_layout <- match.arg(label_layout)

  if (!is.null(label_wrap_width)) {
    if (!is.numeric(label_wrap_width) || length(label_wrap_width) != 1 ||
        is.na(label_wrap_width) || label_wrap_width < 1) {
      stop("`label_wrap_width` must be NULL or a single positive integer.")
    }
    label_wrap_width <- as.integer(label_wrap_width)
  }

  if (!is.numeric(label_outer_pad) || length(label_outer_pad) != 1 ||
      is.na(label_outer_pad) || label_outer_pad < 0) {
    stop("`label_outer_pad` must be a single non-negative numeric value.")
  }

  set.seed(seed)

  # dropOthers acts on the source graph_obj BEFORE layout:
  # it removes "Others" nodes first, then downstream layout/plot are rebuilt.
  if (isTRUE(dropOthers)) {
    node_tbl <- graph_obj %>%
      tidygraph::activate(nodes) %>%
      tidygraph::as_tibble()

    module_candidates <- c("Modularity", "modularity3", "modularity2")
    module_col <- module_candidates[module_candidates %in% colnames(node_tbl)]
    module_col <- if (length(module_col) > 0) module_col[[1]] else NULL

    if (!is.null(module_col)) {
      if ("name" %in% colnames(node_tbl)) {
        keep_names <- node_tbl %>%
          dplyr::filter(as.character(.data[[module_col]]) != "Others") %>%
          dplyr::pull(name)

        graph_obj <- graph_obj %>%
          tidygraph::activate(nodes) %>%
          tidygraph::filter(name %in% keep_names)
      } else {
        graph_obj <- graph_obj %>%
          tidygraph::activate(nodes) %>%
          tidygraph::filter(as.character(.data[[module_col]]) != "Others")
      }
    } else {
      warning("`dropOthers = TRUE` but no module column found in `graph_obj` nodes.")
    }
  }

  if (is.logical(label)) {
    if (length(label) != 1 || is.na(label)) {
      stop("`label` must be a single logical or character string.")
    }
    show_module_label <- isTRUE(label)
    module_label_prefix <- "Modularity"
  } else if (is.character(label)) {
    if (length(label) != 1 || is.na(label)) {
      stop("`label` must be a single logical or character string.")
    }
    module_label_prefix <- trimws(label)
    if (identical(module_label_prefix, "")) {
      module_label_prefix <- "Modularity"
    }
    show_module_label <- TRUE
  } else {
    stop("`label` must be a single logical or character string.")
  }

  module_label_fun <- function(x) {
    x_chr <- as.character(x)
    ifelse(x_chr == "Others", "Others", paste0(module_label_prefix, x_chr))
  }

  is_module_field <- function(var_name) {
    is.character(var_name) &&
      length(var_name) == 1 &&
      !is.na(var_name) &&
      tolower(var_name) %in% c("modularity", "modularity2", "modularity3")
  }

  # Point-level aesthetics (`shape`, `fill.by`) should reflect raw node attributes
  # by default, but if mapping a modularity field, use the same prefix style
  # as module labels regardless of `label` visibility.
  point_legend_label_fun <- function(x, var_name) {
    if (is_module_field(var_name)) {
      module_label_fun(x)
    } else {
      as.character(x)
    }
  }

  if (isTRUE(mapping_line)) {
    # stat graph
    stat_graph <- stat_graph(graph_obj, mapping_line)
  }else{
    stat_graph <- stat_graph(graph_obj, mapping_line)
  }


  # find layout function
  func_name <- paste0("create_layout_", layout)

  # find layout functions from ggNetView package
  lay_func <- utils::getFromNamespace(func_name, "ggNetView")

  # get ly1
  if (layout == "consensus_module_equal_gephi" | layout == "consensus_module_gephi") {
    ly1 = lay_func(graph_obj = graph_obj,
                   node_add = node_add,
                   r = r,
                   scale = scale,
                   anchor_dist = anchor_dist,
                   orientation = orientation,
                   angle = angle,
                   nrow = nrow,
                   ncol = ncol)
  }else if (layout == "WGCNA") {
    # WGCNA layout has an extra `inner_shrink` parameter that controls
    # intra-module compactness independently of inter-module spacing.
    # Other layouts do not accept this argument, so we dispatch it here
    # only.  Default `inner_shrink = 1` reproduces historical behaviour.
    ly1 = lay_func(graph_obj = graph_obj,
                   node_add = node_add,
                   r = r,
                   inner_shrink = inner_shrink,
                   scale = scale,
                   anchor_dist = anchor_dist,
                   orientation = orientation,
                   angle = angle)
  }else{
    ly1 = lay_func(graph_obj = graph_obj,
                   node_add = node_add,
                   r = r,
                   scale = scale,
                   anchor_dist = anchor_dist,
                   orientation = orientation,
                   angle = angle)
  }



  # get ly1_1

  if (layout.module == "random") {
    ly1_1 <- module_layout(graph_obj,
                           layout = ly1,
                           center = center,
                           idx = idx,
                           shrink = shrink,
                           jitter = jitter,
                           jitter_sd = jitter_sd# ,
                           # seed = seed
    )
  }

  if (layout.module == "adjacent") {
    k_nn_cap <- max(1, nrow(ly1) - 1)
    k_nn_try <- min(k_nn, k_nn_cap)   # clamp first attempt to n-1 (audit H1)
    ly1_1 <- NULL
    while (is.null(ly1_1)) {
      ly_try <- tryCatch(
        module_layout3(graph_obj,
                       layout = ly1,
                       center = center,
                       k_nn = k_nn_try,
                       push_others_delta = push_others_delta,
                       shrink = shrink,
                       jitter = jitter,
                       jitter_sd = jitter_sd
                       # seed = seed
        ),
        error = function(e) e
      )

      if (!inherits(ly_try, "error")) {
        ly1_1 <- ly_try
        break
      }

      err_msg <- conditionMessage(ly_try)
      is_slot_error <- grepl("consecutive slots", err_msg, ignore.case = TRUE)
      if (!is_slot_error || k_nn_try >= k_nn_cap) {
        stop(ly_try)
      }

      next_k <- min(k_nn_cap, max(k_nn_try + 20, ceiling(k_nn_try * 1.25)))
      if (next_k <= k_nn_try) {
        stop(ly_try)
      }
      warning(sprintf(
        "`layout.module = 'adjacent'` failed at k_nn = %d; retrying with k_nn = %d.",
        k_nn_try, next_k
      ))
      k_nn_try <- next_k
    }
  }

  if (layout.module == "order" & func_name != "create_layout_multirings") {
    ly1_1 <- module_layout4(graph_obj,
                            layout = ly1,
                            center = center,
                            k_nn = k_nn,
                            push_others_delta = push_others_delta,
                            shrink = shrink,
                            jitter = jitter,
                            jitter_sd = jitter_sd
                            # seed = seed
    )
  }

  if (layout.module == "order" & func_name == "create_layout_multirings") {
    ly1_1 <- module_layout5(graph_obj,
                            layout = ly1,
                            center = center,
                            k_nn = k_nn,
                            push_others_delta = push_others_delta,
                            shrink = shrink,
                            jitter = jitter,
                            jitter_sd = jitter_sd
                            # seed = seed
    )
  }

  # Normal layout


  if (group.by != "pie") {

    # Optional: scale layout to fit in radius (for use with ggnetview_modularity_heatmaps)
    if (!is.null(scale_radius) && is.finite(scale_radius) && scale_radius > 0) {
      xr_net <- range(ly1_1$graph_ly_final$x, na.rm = TRUE)
      yr_net <- range(ly1_1$graph_ly_final$y, na.rm = TRUE)
      scale_net <- max(diff(xr_net), diff(yr_net), 1e-8)
      cx <- mean(xr_net)
      cy <- mean(yr_net)
      ly1_1[["graph_ly_final"]] <- ly1_1[["graph_ly_final"]] %>%
        dplyr::mutate(
          x = (x - cx) / scale_net * scale_radius,
          y = (y - cy) / scale_net * scale_radius
        )
      ly1_1[["layout"]] <- dplyr::select(ly1_1[["graph_ly_final"]], x, y)
      ly1_1[["ggplot_data"]] <- get_location(ly1_1[["graph_ly_final"]], ly1_1[["graph_obj"]])
    }



    module_number <- ly1_1$graph_ly_final$Modularity %>% as.character() %>% unique() %>% length()

    # module info
    if (module_number == 1) {
      module_info <-  ly1_1$graph_ly_final$Modularity %>% as.character() %>% unique()

      ly1_1[["graph_ly_final"]] <- ly1_1[["graph_ly_final"]] %>%
        dplyr::mutate(Modularity = as.character(Modularity)) %>%
        dplyr::mutate(Modularity = factor(Modularity))

      ly1_1[["graph_obj"]] <- ly1_1[["graph_obj"]] %>%
        tidygraph::mutate(Modularity = as.character(Modularity)) %>%
        tidygraph::mutate(Modularity = factor(Modularity, ordered = T))

    }else{
      module_info <- levels(ly1_1$graph_ly_final$Modularity)
      module_info <- module_info[module_info!="Others"]
    }

    # remove acts only at the visualization stage (post-layout):
    # it drops "Others" from rendered data while keeping the existing layout.
    if (isFALSE(remove)) {
      ly1_1 <- ly1_1
    }else{
      ly1_1[["graph_ly_final"]] <- ly1_1[["graph_ly_final"]] %>%
        dplyr::filter(as.character(Modularity) %in% module_info)

      ly1_1[["graph_obj"]] <- ly1_1[["graph_obj"]] %>%
        tidygraph::filter(name %in% ly1_1[["graph_ly_final"]]$name)


      ly1_1[["layout"]] <- dplyr::select(ly1_1[["graph_ly_final"]], x, y)

      ly1_1[["ggplot_data"]] <- get_location(ly1_1[["graph_ly_final"]],
                                             ly1_1[["graph_obj"]])
    }

    xr <- NULL; yr <- NULL; x_mid <- NULL
    dx <- NULL; pad <- NULL; lab_df <- NULL
    plot_xlim <- NULL; plot_ylim <- NULL; label_force <- NULL
    # Non-NULL -> render code adds a geom_segment first-leg from module
    # centroid to the elbow on the network boundary (used by
    # two_column_follow). NULL -> no manual first leg.
    lab_leader_df <- NULL
    # TRUE -> pass segment.square = TRUE to geom_text_repel so ggrepel
    # draws an L-shape leader. Requires ggrepel >= 0.9.4. Used by
    # label_circle. Combined with segment.squareShape = 0 to force the
    # elbow at the true L corner instead of collapsing to the label end.
    label_segment_square       <- FALSE
    label_segment_square_shape <- 1
    # point.padding passed to geom_text_repel. 0 closes any gap at the
    # aes point -- needed by two_column_follow whose aes point IS the
    # elbow shared with the manual first leg (any padding leaves a
    # visible disconnect at the elbow).
    label_point_padding        <- 0.15

    .build_label_location <- function(){
      # ---- basic geometry shared by both strategies ----
      xr <<- range(ly1_1[["layout"]]$x)
      yr <<- range(ly1_1[["layout"]]$y)
      x_mid <<- stats::median(ly1_1[["layout"]]$x)
      dx <<- diff(xr) * label_outer_pad
      pad <<- dx * 1.2

      # text-wrap helper (no-op when label_wrap_width is NULL)
      .wrap_label <- function(txt) {
        if (is.null(label_wrap_width)) {
          as.character(txt)
        } else {
          stringr::str_wrap(as.character(txt), width = label_wrap_width)
        }
      }

      # Partial equispacing: enforce a minimum angular gap between
      # neighbouring labels (analog of p2's rank-rescaling within a
      # side, but only "spreading where needed"). Sparse modules keep
      # theta_target = theta_actual (zero displacement); clustered
      # modules get pushed apart just enough to clear min_gap. The
      # circle is "broken" at the largest natural gap so naturally
      # close-but-wrapped modules (near +/- pi) see each other.
      # Re-centring after the forward pass minimises overall drift so
      # the cluster's mean angle is preserved.
      #   min_gap = 2*pi / (2*N) = half the average per-label slice.
      .partial_equispace <- function(theta_in) {
        n <- length(theta_in)
        if (n < 2) return(theta_in)
        sort_order  <- order(theta_in)
        sorted_th   <- theta_in[sort_order]
        gaps        <- c(diff(sorted_th),
                         sorted_th[1] + 2 * pi - sorted_th[n])
        max_gap_idx <- which.max(gaps)
        if (max_gap_idx < n) {
          rotation     <- c((max_gap_idx + 1):n, seq_len(max_gap_idx))
          rotated_th   <- sorted_th[rotation]
          wrap_n       <- max_gap_idx
          rotated_th[(n - wrap_n + 1):n] <-
            rotated_th[(n - wrap_n + 1):n] + 2 * pi
          rotated_orig <- sort_order[rotation]
        } else {
          rotated_th   <- sorted_th
          rotated_orig <- sort_order
        }
        min_gap  <- 2 * pi / (2 * n)
        adjusted <- rotated_th
        for (i in 2:n) {
          if (adjusted[i] - adjusted[i - 1] < min_gap) {
            adjusted[i] <- adjusted[i - 1] + min_gap
          }
        }
        # re-centre so the spread sequence's mean matches the originals'
        adjusted <- adjusted + (mean(rotated_th) - mean(adjusted))
        # normalise into [-pi, pi]
        adjusted <- atan2(sin(adjusted), cos(adjusted))
        out <- numeric(n)
        out[rotated_orig] <- adjusted
        out
      }

      # shared base: one row per module (excluding "Others"), with side / y_rank
      base_df <- ly1_1[["graph_ly_final"]] %>%
        dplyr::distinct(modularity3, .keep_all = T) %>%
        dplyr::filter(modularity3 != "Others") %>%
        dplyr::mutate(side = ifelse(x < x_mid, "left", "right")) %>%
        dplyr::group_by(side) %>%
        dplyr::arrange(y, .by_group = TRUE) %>%
        dplyr::mutate(
          y_rank   = dplyr::row_number(),
          y_target = scales::rescale(y_rank, to = yr)
        ) %>%
        dplyr::ungroup()

      if (identical(label_layout, "two_column")) {
        # ===== two fixed columns: x_anchor pinned to xr[1] - dx / xr[2] + dx
        lab_df <<- base_df %>%
          dplyr::mutate(
            x_anchor = dplyr::if_else(side == "left", xr[1] - dx, xr[2] + dx),
            y_anchor = y_target,
            nudge_x  = x_anchor - x,
            nudge_y  = y_target - y,
            hjust    = dplyr::if_else(side == "left", 1, 0),
            vjust    = 0.5,
            .label_text = .wrap_label(module_label_fun(modularity3))
          )

        plot_xlim                  <<- c(xr[1] - pad, xr[2] + pad)
        plot_ylim                  <<- yr
        label_force                <<- 0.05
        lab_leader_df              <<- NULL    # no manual first leg
        label_segment_square       <<- FALSE   # straight ggrepel segment
        label_segment_square_shape <<- 1
        label_point_padding        <<- 0.15    # default gap from node
      } else if (identical(label_layout, "two_column_follow")) {
        # ===== two_column_follow: label at module's actual angle ==========
        # No equispacing -- each label sits at its module's REAL angular
        # position on the outer ellipse, so left-side modules get
        # left-side labels and right-side modules get right-side labels.
        # Leader has two segments:
        #   - first leg (manual geom_segment in the render code) from
        #     module centroid to an elbow on the network's bounding
        #     ellipse at the same theta_actual -- this is the radial
        #     "exit the network" stub
        #   - second leg drawn by ggrepel from label back to the elbow.
        # ggrepel force = 1 lets it slide overlapping labels (along the
        # tangent direction in practice) without breaking the leader,
        # because ggrepel always retargets its segment to the elbow.
        centroids <- ly1_1[["graph_ly_final"]] %>%
          dplyr::filter(modularity3 != "Others") %>%
          dplyr::group_by(modularity3) %>%
          dplyr::summarise(mx = mean(x), my = mean(y), .groups = "drop")

        cx <- mean(xr)
        cy <- mean(yr)
        R_x_net <- (xr[2] - xr[1]) / 2
        R_y_net <- (yr[2] - yr[1]) / 2
        if (!is.finite(R_x_net) || R_x_net <= 0) R_x_net <- 1
        if (!is.finite(R_y_net) || R_y_net <= 0) R_y_net <- 1
        R_x_outer <- R_x_net * (1 + label_outer_pad)
        R_y_outer <- R_y_net * (1 + label_outer_pad)

        base_follow <- ly1_1[["graph_ly_final"]] %>%
          dplyr::distinct(modularity3, .keep_all = T) %>%
          dplyr::filter(modularity3 != "Others") %>%
          dplyr::left_join(centroids, by = "modularity3") %>%
          dplyr::mutate(theta_actual = atan2(my - cy, mx - cx))

        # Partial equispacing of theta_target (closer to p2's intent):
        # sparse modules keep theta_target = theta_actual so their
        # labels sit exactly above/right/etc. of their modules;
        # clustered modules get pushed apart just enough to clear the
        # min angular gap so labels don't overlap. The L bend will be
        # visible only for clustered modules where theta_target
        # actually differs from theta_actual -- which is exactly where
        # an L is needed to distinguish neighbouring labels.
        theta_target_ord <- .partial_equispace(base_follow$theta_actual)

        lab_df <<- base_follow %>%
          dplyr::mutate(
            theta_target = theta_target_ord,
            # label sits on the outer ellipse at the equispaced
            # theta_target
            x_anchor = cx + R_x_outer * cos(theta_target),
            y_anchor = cy + R_y_outer * sin(theta_target),
            # elbow on the network's bounding ellipse at the module's
            # ACTUAL angle: first leg of the L goes straight radially
            # from the module centroid out to the network boundary.
            # Because theta_target != theta_actual (thanks to
            # equispacing), the second leg ggrepel draws -- from the
            # label back to this elbow -- is slanted, giving a clear L.
            elbow_x  = cx + R_x_net * cos(theta_actual),
            elbow_y  = cy + R_y_net * sin(theta_actual),
            # ggrepel's "point" = the elbow. ggrepel will draw the
            # second leg from the label back to this elbow.
            x        = elbow_x,
            y        = elbow_y,
            nudge_x  = x_anchor - elbow_x,
            nudge_y  = y_anchor - elbow_y,
            # hjust / vjust fan the text outward along theta_target
            hjust    = (1 - cos(theta_target)) / 2,
            vjust    = (1 - sin(theta_target)) / 2,
            .label_text = .wrap_label(module_label_fun(modularity3))
          )

        # lab_leader_df is just a flag (re-using lab_df) so the render
        # code knows to draw the manual first leg via geom_segment from
        # the module centroid (mx, my) to the elbow (elbow_x, elbow_y).
        lab_leader_df <<- lab_df

        # plot_xlim matches two_column (left/right labels fit in the
        # 0.6 R_x_net side pad). plot_ylim must extend to the outer
        # ellipse so top/bottom label anchors don't sit outside the
        # plot panel -- two_column doesn't need this because its
        # labels never leave yr in the y direction. The theme's 20pt
        # plot.margin covers the remaining text-height overflow above
        # / below each anchor.
        plot_xlim   <<- c(xr[1] - pad, xr[2] + pad)
        plot_ylim   <<- c(cy - R_y_outer, cy + R_y_outer)
        # force = 0: labels stay exactly at the outer ellipse position
        # we computed. Using force = 1 here can push labels back INTO
        # the network when ggrepel resolves label-label overlaps, which
        # the user explicitly forbids. If labels overlap because too
        # many modules sit at similar angles, increase label_outer_pad
        # so the outer ring has more tangential room.
        label_force                <<- 0
        label_segment_square       <<- FALSE   # ggrepel draws single line
        label_segment_square_shape <<- 1
        # ★ no padding around aes point: the aes IS the elbow shared
        # with the manual first leg, so ggrepel's segment must reach
        # it exactly or you see a visible gap at the L corner
        label_point_padding        <<- 0
      } else {
        # ===== label_circle: pure ggrepel + module's actual angle =========
        # Same "label at module's actual angle on the outer ellipse" as
        # two_column_follow, but the leader is drawn entirely by ggrepel
        # (no manual first leg). ggrepel's aes(x, y) is the module
        # centroid, nudge places the label initially at outer ellipse at
        # theta_actual, force = 1 lets ggrepel spread overlapping labels.
        # segment.square = TRUE (with squareShape = 0) makes ggrepel draw
        # an L-shape leader. Simpler code than two_column_follow but the
        # L bend can visually collapse when label and module sit on the
        # same radial. Requires ggrepel >= 0.9.4.
        centroids <- ly1_1[["graph_ly_final"]] %>%
          dplyr::filter(modularity3 != "Others") %>%
          dplyr::group_by(modularity3) %>%
          dplyr::summarise(mx = mean(x), my = mean(y), .groups = "drop")

        cx <- mean(xr)
        cy <- mean(yr)
        R_x_net <- (xr[2] - xr[1]) / 2
        R_y_net <- (yr[2] - yr[1]) / 2
        if (!is.finite(R_x_net) || R_x_net <= 0) R_x_net <- 1
        if (!is.finite(R_y_net) || R_y_net <= 0) R_y_net <- 1
        R_x_outer <- R_x_net * (1 + label_outer_pad)
        R_y_outer <- R_y_net * (1 + label_outer_pad)

        base_follow <- ly1_1[["graph_ly_final"]] %>%
          dplyr::distinct(modularity3, .keep_all = T) %>%
          dplyr::filter(modularity3 != "Others") %>%
          dplyr::left_join(centroids, by = "modularity3") %>%
          dplyr::mutate(theta_actual = atan2(my - cy, mx - cx))

        # Partial equispacing: sparse modules keep theta_target =
        # theta_actual, clustered modules get pushed apart just
        # enough to clear the min angular gap. See two_column_follow
        # for the algorithm rationale.
        theta_target_ord <- .partial_equispace(base_follow$theta_actual)

        lab_df <<- base_follow %>%
          dplyr::mutate(
            theta_target = theta_target_ord,
            x_anchor     = cx + R_x_outer * cos(theta_target),
            y_anchor     = cy + R_y_outer * sin(theta_target),
            # ggrepel's "point" = module centroid; nudge places the
            # label on the outer ellipse at the equispaced theta_target
            x        = mx,
            y        = my,
            nudge_x  = x_anchor - mx,
            nudge_y  = y_anchor - my,
            hjust    = (1 - cos(theta_target)) / 2,
            vjust    = (1 - sin(theta_target)) / 2,
            .label_text = .wrap_label(module_label_fun(modularity3))
          )

        # plot_xlim matches two_column; plot_ylim must extend to the
        # outer ellipse so top/bottom label anchors fit inside the
        # panel (theme's 20pt margin covers the remaining text height).
        plot_xlim   <<- c(xr[1] - pad, xr[2] + pad)
        plot_ylim   <<- c(cy - R_y_outer, cy + R_y_outer)
        lab_leader_df              <<- NULL    # ggrepel draws everything
        # force = 0: same reason as two_column_follow -- ggrepel's
        # collision avoidance would otherwise pull labels back toward
        # their aes point (the module centroid, inside the network).
        # Keep labels exactly at the outer ellipse; if they overlap
        # because of clustered angles, bump label_outer_pad.
        label_force                <<- 0
        # segment.square = FALSE so ggrepel draws a single straight
        # slanted line from label to module. With segment.square = TRUE,
        # the L would visually collapse here because the label sits on
        # the module's radial line (label, network centre, module are
        # collinear -- the bend has no perpendicular component to show).
        # Using a single straight line makes label_circle visually
        # distinct from two_column_follow's right-angle L.
        label_segment_square       <<- FALSE
        label_segment_square_shape <<- 1       # unused when square=FALSE
        label_point_padding        <<- 0.15    # default gap from module
      }
    }



    # outlier df location
    .build_mask_table <- function(){
      maskTable <- generateMask_ggnetview(
        dims = ly1_1[["layout"]],
        clusters = ly1_1[["graph_obj"]] %>%
          tidygraph::activate(nodes) %>%
          tidygraph::as_tibble() %>%
          dplyr::pull(modularity3),
        q = q_outer,
        expand = expand_outer,
        bandwidth_scale = bandwidth_scale
      )

      return(maskTable)
    }

    ####----Plot----####
    # base plot
    p1_1 <- ggplot2::ggplot()


    if (isTRUE(add_group_outer) && nrow(ly1_1[["ggplot_data"]][[1]]) > 0) {
      group_circle_df <- ly1_1[["ggplot_data"]][[1]] %>%
        dplyr::mutate(.group_outer = 1L)
      circle_n_grp <- max(40, min(300, as.integer(round(8 * sqrt(nrow(group_circle_df))))))
      fill_grp <- if (is.null(add_group_outer_fill) || length(add_group_outer_fill) == 0L) NA else add_group_outer_fill[1L]
      alpha_grp <- if (is.na(fill_grp)) 1 else add_group_outer_fill_alpha
      p1_1 <- p1_1 +
        ggforce::geom_mark_circle(
          data = group_circle_df,
          mapping = ggplot2::aes(x = x, y = y, group = .group_outer),
          fill = fill_grp,
          alpha = alpha_grp,
          color = add_group_outer_color,
          linetype = add_group_outer_linetype,
          linewidth = add_group_outer_linewidth,
          n = circle_n_grp,
          expand = grid::unit(add_group_outer_expand, "mm")
        )
    }

  # line parameter
  line_color_by <- NULL
  line_scale <- NULL
  if (isTRUE(plot_line)) {
    if (is.character(mapping_line)) {
      if (length(mapping_line) != 1) {
        stop("`mapping_line` must be a variable name in graph_object.")
      }
      if (!mapping_line %in% colnames(ly1_1[["ggplot_data"]][[2]])) {
        stop("`mapping_line` must be a variable name in graph_object.")
      }
      line_color_by <- mapping_line
      line_values <- ly1_1[["ggplot_data"]][[2]][[line_color_by]]
      if (is.numeric(line_values)) {
        line_scale <- ggplot2::scale_color_gradient(low = "#4393c3", high = "#d6604d")
      } else {
        line_scale <- if (is.null(color)) {
          ggplot2::scale_color_manual(values = c("Positive" = "#d6604d", "Negative" = "#4393c3"))
        } else {
          ggplot2::scale_color_manual(values = color)
        }
      }
    }
    if (isFALSE(curve)) {
      if (isFALSE(mapping_line) && is.null(line_color_by)) {
        p1_1 <- p1_1 +
          ggplot2::geom_segment(data = ly1_1[["ggplot_data"]][[2]],
                                mapping = ggplot2::aes(x = from_x,
                                                       xend = to_x,
                                                       y = from_y,
                                                       yend = to_y),
                                alpha = linealpha,
                                colour = linecolor) +
          theme_ggnetview()

      }else if (!is.null(line_color_by)){
        p1_1 <- p1_1 +
          ggplot2::geom_segment(data = ly1_1[["ggplot_data"]][[2]],
                                mapping = ggplot2::aes(x = from_x,
                                                       xend = to_x,
                                                       y = from_y,
                                                       yend = to_y,
                                                       colour = .data[[line_color_by]]),
                                alpha = linealpha) +
          line_scale +
          ggnewscale::new_scale_color() +
          theme_ggnetview()
      }else{
        p1_1 <- p1_1 +
          ggplot2::geom_segment(data = ly1_1[["ggplot_data"]][[2]],
                                mapping = ggplot2::aes(x = from_x,
                                                       xend = to_x,
                                                       y = from_y,
                                                       yend = to_y,
                                                       colour = corr_direction),
                                alpha = linealpha) +
          ggplot2::scale_color_manual(values = c("Positive" = "#d6604d", "Negative" = "#4393c3")) +
          ggnewscale::new_scale_color() +
          # ggplot2::coord_fixed() +
          theme_ggnetview()
      }
    }else{
      if (isFALSE(mapping_line) && is.null(line_color_by)) {
        p1_1 <- p1_1 +
          ggplot2::geom_curve(data = ly1_1[["ggplot_data"]][[2]],
                              mapping = ggplot2::aes(x = from_x,
                                                     xend = to_x,
                                                     y = from_y,
                                                     yend = to_y),
                              alpha = linealpha,
                              colour = linecolor,
                              curvature = curvature
                              ) +
          theme_ggnetview()

      }else if (!is.null(line_color_by)){
        p1_1 <- p1_1 +
          ggplot2::geom_curve(data = ly1_1[["ggplot_data"]][[2]],
                              mapping = ggplot2::aes(x = from_x,
                                                     xend = to_x,
                                                     y = from_y,
                                                     yend = to_y,
                                                     colour = .data[[line_color_by]]),
                              alpha = linealpha,
                              curvature = curvature) +
          line_scale +
          ggnewscale::new_scale_color() +
          # ggplot2::coord_fixed() +
          theme_ggnetview()
      }else{
        p1_1 <- p1_1 +
          ggplot2::geom_curve(data = ly1_1[["ggplot_data"]][[2]],
                              mapping = ggplot2::aes(x = from_x,
                                                     xend = to_x,
                                                     y = from_y,
                                                     yend = to_y,
                                                     colour = corr_direction),
                              alpha = linealpha,
                              curvature = curvature) +
          ggplot2::scale_color_manual(values = c("Positive" = "#d6604d", "Negative" = "#4393c3")) +
          ggnewscale::new_scale_color() +
          # ggplot2::coord_fixed() +
          theme_ggnetview()
      }
    }
  }



    # point paramers
    if (is.character(shape)) {
      if (length(shape) != 1) {
        stop("`shape` must be a variable name in graph_object.")
      }
      if (!shape %in% colnames(ly1_1[["ggplot_data"]][[1]])) {
        stop("`shape` must be a variable name in graph_object.")
      }
    }
    if (!is.null(color.by)) {
      if (length(color.by) != 1) {
        stop("`color.by` must be a single variable name in graph_object.")
      }
      if (!color.by %in% colnames(ly1_1[["ggplot_data"]][[1]])) {
        stop("`color.by` must be a variable name in graph_object.")
      }
    }
    point_label_df <- NULL
    point_label_col <- NULL
    if (!is.null(pointlabel)) {
      if (!is.character(pointlabel) || length(pointlabel) != 1) {
        stop("`pointlabel` must be NULL, 'ALL', or 'topN' (N is a positive integer, e.g. 'top7').")
      }
      pointlabel_clean <- toupper(trimws(pointlabel))
      is_all <- identical(pointlabel_clean, "ALL")
      is_top_n <- grepl("^TOP[1-9][0-9]*$", pointlabel_clean)
      if (!is_all && !is_top_n) {
        stop("`pointlabel` must be NULL, 'ALL', or 'topN' (N is a positive integer, e.g. 'top7').")
      }

      point_data <- ly1_1[["ggplot_data"]][[1]]
      if (!"Degree" %in% colnames(point_data)) {
        stop("`Degree` column is required in `ly1_1[['ggplot_data']][[1]]` for `pointlabel`.")
      }

      group_col <- if (group.by %in% colnames(point_data)) {
        group.by
      } else if ("Modularity" %in% colnames(point_data)) {
        "Modularity"
      } else {
        stop("No valid module column found for `pointlabel` grouping.")
      }

      point_label_col <- if ("ID" %in% colnames(point_data)) {
        "ID"
      } else if ("name" %in% colnames(point_data)) {
        "name"
      } else {
        stop("`pointlabel` requires an `ID` or `name` column in point data.")
      }

      if (is_all) {
        point_label_df <- point_data
      } else {
        top_n <- as.integer(sub("^TOP", "", pointlabel_clean))

        point_label_df <- point_data %>%
          dplyr::group_by(.data[[group_col]]) %>%
          dplyr::slice_max(order_by = Degree, n = top_n, with_ties = FALSE) %>%
          dplyr::ungroup()
      }
    }
    fill_classes <- .ggnv_class_order(ly1_1[["graph_ly_final"]][[fill.by]])
    merge_point_legends <- is.character(shape) && identical(shape, fill.by)
    fill_scale_points <- if (is.null(fill)) {
      scale_fill_ggnetview(fill_classes,
                           breaks = fill_classes,
                           labels = function(x) point_legend_label_fun(x, fill.by),
                           guide = ggplot2::guide_legend(ncol = 1, order = 1))
    } else {
      ggplot2::scale_fill_manual(values = fill,
                                 breaks = fill_classes,
                                 labels = function(x) point_legend_label_fun(x, fill.by),
                                 guide = ggplot2::guide_legend(ncol = 1, order = 1))
    }
    color_scale_points <- NULL
    same_fill_color_mapping <- !is.null(color.by) && identical(color.by, fill.by)
    if (!is.null(color.by)) {
      color_values <- ly1_1[["ggplot_data"]][[1]][[color.by]]
      if (is.numeric(color_values)) {
        color_scale_points <- ggplot2::scale_color_gradient(
          low = "#4393c3",
          high = "#d6604d",
          guide = if (same_fill_color_mapping) "none" else "legend"
        )
      } else if (is.null(color)) {
        color_scale_points <- scale_color_ggnetview(
          .ggnv_class_order(color_values),
          labels = function(x) point_legend_label_fun(x, color.by),
          guide = if (same_fill_color_mapping) "none" else ggplot2::guide_legend(ncol = 1, order = 2)
        )
      } else {
        color_scale_points <- ggplot2::scale_color_manual(
          values = color,
          labels = function(x) point_legend_label_fun(x, color.by),
          guide = if (same_fill_color_mapping) "none" else ggplot2::guide_legend(ncol = 1, order = 2)
        )
      }
    }
    shape_scale_points <- NULL
    if (is.character(shape)) {
      shape_classes <- .ggnv_class_order(ly1_1[["graph_ly_final"]][[shape]])
      shape_values <- rep(21:25, length.out = length(shape_classes))
      shape_scale_points <- ggplot2::scale_shape_manual(
        values = shape_values,
        breaks = shape_classes,
        labels = function(x) point_legend_label_fun(x, shape),
        guide = ggplot2::guide_legend(ncol = 1, order = if (merge_point_legends) 1 else 2)
      )
    }
    size_guide_points <- NULL
    if (isTRUE(pointstroke == 0)) {
      size_guide_points <- ggplot2::guides(
        size = ggplot2::guide_legend(
          ncol = 1,
          order = 3,
          override.aes = list(
            shape = 21,
            fill = "grey70",
            colour = "grey70",
            stroke = 0.3
          )
        )
      )
    }
    fill_guide_points <- NULL
    if (is.null(color.by) && !merge_point_legends) {
      fill_guide_points <- ggplot2::guides(
        fill = ggplot2::guide_legend(
          ncol = 1,
          order = 1,
          override.aes = list(
            shape = 21,
            colour = "grey40",
            stroke = pointstroke
          )
        )
      )
    }
    if (is.character(shape) && !is.null(color.by)) {
      point_mapping <- ggplot2::aes(x = x, y = y,
                                    fill = .data[[fill.by]],
                                    size = Degree,
                                    shape = .data[[shape]],
                                    color = .data[[color.by]])
    } else if (is.character(shape)) {
      point_mapping <- ggplot2::aes(x = x, y = y,
                                    fill = .data[[fill.by]],
                                    size = Degree,
                                    shape = .data[[shape]])
    } else if (!is.null(color.by)) {
      point_mapping <- ggplot2::aes(x = x, y = y,
                                    fill = .data[[fill.by]],
                                    size = Degree,
                                    color = .data[[color.by]])
    } else {
      point_mapping <- ggplot2::aes(x = x, y = y, fill = .data[[fill.by]], size = Degree)
    }
    if (isFALSE(jitter)) {
      if (is.character(shape)) {
        p1_1 <- p1_1 +
          ggplot2::geom_point(data = ly1_1[["ggplot_data"]][[1]],
                              mapping = point_mapping,
                              alpha = pointalpha,
                              stroke = pointstroke)
      } else {
        p1_1 <- p1_1 +
          ggplot2::geom_point(data = ly1_1[["ggplot_data"]][[1]],
                              mapping = point_mapping,
                              shape = shape,
                              alpha = pointalpha,
                              stroke = pointstroke)
      }
      p1_1 <- p1_1 +
        ggplot2::scale_size(range = pointsize, guide = ggplot2::guide_legend(ncol = 1, order = 3)) +
        ggplot2::coord_fixed() +
        theme_ggnetview() +
        fill_scale_points +
        color_scale_points +
        shape_scale_points +
        size_guide_points +
        fill_guide_points
    }else{
      # p1_1 <- p1_1 +
      #   ggplot2::geom_jitter(data = ly1_1[["ggplot_data"]][[1]],
      #                        mapping = ggplot2::aes(x = x, y = y, fill = .data[[fill.by]], size = Degree),
      #                        shape = shape,
      #                        alpha = pointalpha,
      #                        stroke = pointstroke,
      #                        position = ggplot2::position_jitter(width = jitter_sd, height = jitter_sd, seed = seed)) +
      #   ggplot2::scale_size(range = pointsize) +
      #   ggplot2::coord_fixed() +
      #   theme_ggnetview() +
      #   scale_fill_ggnetview(unique(ly1_1[["graph_ly_final"]][[fill.by]]))
      if (is.character(shape)) {
        p1_1 <- p1_1 +
          ggplot2::geom_point(data = ly1_1[["ggplot_data"]][[1]],
                              mapping = point_mapping,
                              alpha = pointalpha,
                              stroke = pointstroke)
      } else {
        p1_1 <- p1_1 +
          ggplot2::geom_point(data = ly1_1[["ggplot_data"]][[1]],
                              mapping = point_mapping,
                              shape = shape,
                              alpha = pointalpha,
                              stroke = pointstroke)
      }
      p1_1 <- p1_1 +
        ggplot2::scale_size(range = pointsize, guide = ggplot2::guide_legend(ncol = 1, order = 3)) +
        ggplot2::coord_fixed() +
        theme_ggnetview() +
        fill_scale_points +
        color_scale_points +
        shape_scale_points +
        size_guide_points +
        fill_guide_points

    }

    # label = F add_outer = F
    if (isFALSE(show_module_label) & isFALSE(add_outer)) {
      p1_1 <- p1_1

    }

    # label = T add_outer = F
    if (isTRUE(show_module_label) & isFALSE(add_outer)) {

      .build_label_location()

      lab_classes <- .ggnv_class_order(lab_df$Modularity)
      color_scale_lab <- if (is.null(color)) scale_color_ggnetview(lab_classes, labels = module_label_fun) else ggplot2::scale_color_manual(values = color, labels = module_label_fun)

      p1_1 <- p1_1 +
        ggnewscale::new_scale_color() +
        # First leg of the two-segment leader (two_column_follow only):
        # from module centroid (mx, my) to the elbow on the network
        # boundary (elbow_x, elbow_y). The second leg is drawn by
        # ggrepel (from label to elbow), so even if ggrepel pushes the
        # label to avoid overlap, the leader stays connected.
        (if (!is.null(lab_leader_df))
           ggplot2::geom_segment(
             data = lab_leader_df,
             mapping = ggplot2::aes(x = mx, y = my,
                                    xend = elbow_x, yend = elbow_y,
                                    color = .data[[group.by]]),
             linewidth = labelsegmentsize,
             alpha     = labelsegmentalpha,
             lineend   = "round",
             show.legend = FALSE
           )
         else NULL) +
        ggrepel::geom_text_repel(data = lab_df,
                                 mapping = ggplot2::aes(x = x,
                                               y = y,
                                               label = .label_text,
                                               color = .data[[group.by]]),
                                 size = labelsize,
                                 nudge_x = lab_df$nudge_x,
                                 nudge_y = lab_df$nudge_y,
                                 hjust   = lab_df$hjust,
                                 vjust   = lab_df$vjust,
                                 # ggrepel draws the second leg of the
                                 # two_column_follow leader (and the full
                                 # leader for two_column / label_circle).
                                 # min.segment.length = 0 forces it on.
                                 min.segment.length = 0,
                                 segment.size  = labelsegmentsize,
                                 segment.alpha = labelsegmentalpha,
                                 # segment.square (>= ggrepel 0.9.4) is
                                 # TRUE only for label_circle so ggrepel
                                 # draws an L-shape there; FALSE elsewhere
                                 segment.square      = label_segment_square,
                                 segment.squareShape = label_segment_square_shape,
                                 max.overlaps = Inf,
                                 box.padding = 0.15,
                                 point.padding = label_point_padding,
                                 force = label_force,
                                 show.legend = F
        ) +
        color_scale_lab +
        ggplot2::coord_equal(clip = "off",
                             xlim = plot_xlim,
                             ylim = plot_ylim) +
        theme_ggnetview()
    }

    # label = F add_outer = T
    if (isFALSE(show_module_label) & isTRUE(add_outer)) {

      maskTable <- .build_mask_table()

      maskTable <- maskTable %>% dplyr::mutate(cluster = factor(cluster, levels = levels(ly1_1[["graph_ly_final"]]$Modularity), ordered = T))

      mask_classes <- .ggnv_class_order(maskTable$cluster)
      fill_scale_mask <- if (is.null(fill)) scale_fill_ggnetview(mask_classes, labels = module_label_fun) else ggplot2::scale_fill_manual(values = fill, labels = module_label_fun)
      color_scale_mask <- if (is.null(color)) scale_color_ggnetview(mask_classes, labels = module_label_fun) else ggplot2::scale_color_manual(values = color, labels = module_label_fun)

      p1_1 <- p1_1 +
        ggnewscale::new_scale_fill() +
        ggnewscale::new_scale_color() +
        ggplot2::geom_polygon(data=maskTable %>%
                                dplyr::filter(cluster != "Others"),
                              mapping = ggplot2::aes(x = x, y = y,
                                                     group = interaction(cluster, polygon_id),
                                                     fill = cluster, color = cluster),
                              linewidth = outerwidth,
                              linetype = outerlinetype,
                              alpha = outeralpha,
                              show.legend = F) +
        fill_scale_mask +
        color_scale_mask +
        ggplot2::coord_equal(clip = "off") +
        theme_ggnetview()
    }

    # label = T add_outer = T
    if (isTRUE(show_module_label) & isTRUE(add_outer)) {

      .build_label_location()
      maskTable <- .build_mask_table()

      maskTable <- maskTable %>% dplyr::mutate(cluster = factor(cluster, levels = levels(ly1_1[["graph_ly_final"]]$Modularity), ordered = T))

      lab_classes_outer <- .ggnv_class_order(lab_df$Modularity)
      mask_classes_outer <- .ggnv_class_order(maskTable$cluster)
      color_scale_lab_outer <- if (is.null(color)) scale_color_ggnetview(lab_classes_outer, labels = module_label_fun) else ggplot2::scale_color_manual(values = color, labels = module_label_fun)
      fill_scale_mask_outer <- if (is.null(fill)) scale_fill_ggnetview(mask_classes_outer, na_value = NA, labels = module_label_fun) else ggplot2::scale_fill_manual(values = fill, labels = module_label_fun)
      color_scale_mask_outer <- if (is.null(color)) scale_color_ggnetview(mask_classes_outer, na_value = NA, labels = module_label_fun) else ggplot2::scale_color_manual(values = color, labels = module_label_fun)

      p1_1 <- p1_1 +
        ggnewscale::new_scale_color() +
        # First leg of the two-segment leader (two_column_follow only):
        # from module centroid (mx, my) to the elbow on the network
        # boundary (elbow_x, elbow_y). The second leg is drawn by
        # ggrepel (from label to elbow), so even if ggrepel pushes the
        # label to avoid overlap, the leader stays connected.
        (if (!is.null(lab_leader_df))
           ggplot2::geom_segment(
             data = lab_leader_df,
             mapping = ggplot2::aes(x = mx, y = my,
                                    xend = elbow_x, yend = elbow_y,
                                    color = modularity2),
             linewidth = labelsegmentsize,
             alpha     = labelsegmentalpha,
             lineend   = "round",
             show.legend = FALSE
           )
         else NULL) +
        ggrepel::geom_text_repel(data = lab_df,
                                 mapping = ggplot2::aes(x = x,
                                               y = y,
                                               label = .label_text,
                                               color = modularity2),
                                 size = labelsize,
                                 nudge_x = lab_df$nudge_x,
                                 nudge_y = lab_df$nudge_y,
                                 hjust   = lab_df$hjust,
                                 vjust   = lab_df$vjust,
                                 # ggrepel draws the second leg of the
                                 # two_column_follow leader (and the full
                                 # leader for two_column / label_circle).
                                 # min.segment.length = 0 forces it on.
                                 min.segment.length = 0,
                                 segment.size  = labelsegmentsize,
                                 segment.alpha = labelsegmentalpha,
                                 # segment.square (>= ggrepel 0.9.4) is
                                 # TRUE only for label_circle so ggrepel
                                 # draws an L-shape there; FALSE elsewhere
                                 segment.square      = label_segment_square,
                                 segment.squareShape = label_segment_square_shape,
                                 max.overlaps = Inf,
                                 box.padding = 0.15,
                                 point.padding = label_point_padding,
                                 force = label_force,
                                 show.legend = F
        ) +
        color_scale_lab_outer +
        ggnewscale::new_scale_fill() +
        ggnewscale::new_scale_color() +
        ggplot2::geom_polygon(data= maskTable %>% dplyr::filter(cluster != "Others"),
                              mapping = ggplot2::aes(x = x, y = y,
                                                     group = interaction(cluster, polygon_id),
                                                     fill = cluster, color = cluster),
                              linewidth = outerwidth,
                              linetype = outerlinetype,
                              alpha = outeralpha,
                              show.legend = F) +
        fill_scale_mask_outer +
        color_scale_mask_outer +
        ggplot2::coord_equal(clip = "off",
                             xlim = plot_xlim,
                             ylim = plot_ylim) +
        theme_ggnetview()
    }

    # add point labels after module boundary/text layers
    if (!is.null(point_label_df) && nrow(point_label_df) > 0) {
      p1_1 <- p1_1 +
        ggplot2::geom_text(
          data = point_label_df,
          mapping = ggplot2::aes(x = x, y = y, label = .data[[point_label_col]]),
          size = pointlabelsize,
          show.legend = FALSE
        )
    }


    if (isTRUE(mapping_line)) {
      gglabel = paste0("Node = ", stat_graph$node, "\n",
                       "Edge = ", stat_graph$edge, "\n",
                       "Positive = ", stat_graph$position_edge, "\n",
                       "Negative = ", stat_graph$negative_edge)
    }else{
      gglabel = paste0("Node = ", stat_graph$node, "\n",
                       "Edge = ", stat_graph$edge, "\n")
    }

    p1_1 <- p1_1 +
      ggplot2::ggtitle(label = gglabel) +
      ggplot2::theme(
        legend.box = "horizontal",
        legend.box.just = "left"
      )

  }

  # specific layout dendrogram
  if (layout == "dendrogram") {
    color_default_dendro <- c('#66c2a5','#fc8d62','#a6d854','#e78ac3')
    color_scale_dendro <- if (is.null(color)) {
      color_default_dendro
    } else {
      color
    }
    p1_1 <- ggraph::ggraph(graph_obj,layout = layout, circular = TRUE) +
      ggraph::geom_node_point(ggplot2::aes(size=node_size, color=type),alpha=pointalpha) +
      ggraph::geom_edge_diagonal(ggplot2::aes(color = node1.node), alpha=linealpha) +
      ggraph::scale_edge_color_manual(values = color_scale_dendro) +
      ggplot2::scale_color_manual(values = color_scale_dendro) +
      ggplot2::scale_size(range = c(3,15)) +
      ggraph::geom_node_text(
        ggplot2::aes(
          x = 1.0175 * x,
          y = 1.0175 * y,
          label = node,
          angle = -((-ggraph::node_angle(x, y) + 90) %% 180) + 90,
          filter = leaf,
          color = type
        ),
        size = 2, hjust = 'outward'
      ) +
      ggraph::geom_node_text(
        ggplot2::aes(label=node,
            filter = !leaf,
            color = type),
        fontface="bold",
        size=3,
        family="sans"
      ) +
      ggplot2::coord_fixed(clip = "off") +
      theme_ggnetview()

    return(p1_1)
  }

  # specific layout pie
  if (group.by == "pie") {

    ly <- ggraph::create_layout(graph_obj, layout = layout)

    col_index_start = which(colnames(ly) == "name")
    col_index_end = which(colnames(ly) == ".ggraph.orig_index")
    col_index = colnames(ly)[(col_index_start+1) : (col_index_end -1)]


    fill_default_pie <- c('#66c2a5','#fc8d62','#a6d854','#e78ac3')
    fill_scale_pie <- if (is.null(fill)) {
      ggplot2::scale_fill_manual(values = fill_default_pie)
    } else {
      ggplot2::scale_fill_manual(values = fill)
    }
    p1_1 <- ggraph::ggraph(ly, layout = "manual", x = ly[["x"]], y = ly[["y"]]) +
      ggraph::geom_edge_link(color = "#6baed6") +
      scatterpie::geom_scatterpie(
        data = ly,
        cols = col_index,
        colour = "#000000",
        pie_scale = 2
      ) +
      fill_scale_pie +
      ggplot2::coord_fixed() +
      theme_ggnetview()

    return(p1_1)
  }

  if (isTRUE(return_layout) && exists("ly1_1", inherits = FALSE) &&
      !is.null(ly1_1$graph_ly_final) && "Modularity" %in% colnames(ly1_1$graph_ly_final)) {
    module_centroids <- ly1_1$graph_ly_final %>%
      dplyr::filter(as.character(.data$Modularity) != "Others") %>%
      dplyr::group_by(.data$Modularity) %>%
      dplyr::summarise(x = mean(.data$x, na.rm = TRUE), y = mean(.data$y, na.rm = TRUE), .groups = "drop") %>%
      dplyr::mutate(ID = as.character(.data$Modularity)) %>%
      dplyr::select("ID", "x", "y")
    layout_data <- list(
      graph_ly_final = ly1_1$graph_ly_final,
      graph_obj = ly1_1$graph_obj,
      ggplot_data = ly1_1$ggplot_data,
      module_centroids = module_centroids
    )
    return(list(plot = p1_1, layout_data = layout_data))
  }

  return(p1_1)
}
