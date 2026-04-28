#' Visualize multi-orientation environmental-species correlation heatmaps
#'
#' @param env Data Frame
#' A data frame or matrix containing environmental variables.
#' Each column represents an environmental factor.
#' @param spec  Data Frame
#' A data frame or matrix containing species abundance or trait data.
#' Each column represents a species or taxonomic unit.
#' @param env_select Optional list specifying the column indices (or names)
#' of environmental variables to include in each environmental block.
#' Each list element corresponds to one quadrant of the heatmap layout.
#' @param spec_select Optional list specifying column indices (or names)
#' of species to include. If multiple elements are provided, they define
#' separate species clusters in the visualization.
#' @param spec_layout Character or character vector (default = \code{"circle_outline"}).
#' Spatial arrangement of species nodes for each central network.
#' If a single value, applied to all spec blocks.
#' If a vector, length must match the number of spec blocks; each element specifies
#' the layout for that block in order. Valid options: \code{"circle_outline"},
#' \code{"diamond_outline"}, \code{"rectangle_outline"}, \code{"square_outline"}.
#' @param spec_orientation Character
#' spec_oritation. Options include: "up","down","left","right"
#' @param spec_relation Logical (defalt = TRUE)
#' Whether to compulate the ralationship of spec
#' @param relation_method Character
#' Method for computing relationships between species and environmental factors. Options are `"correlation"` or `"mantel"`.
#' @param cor.method Character
#' Correlation method to use when `relation_method = "correlation"`.
#' One of `"pearson"`, `"kendall"`, or `"spearman"`.
#' @param cor.use Character
#' Method for handling missing values in correlation computation.
#' One of `"everything"`, `"all"`, `"complete"`, `"pairwise"`, or `"na"`.
#' @param mantel.method Character
#' Type of Mantel test to use when `relation_method = "mantel"`.
#' Options include `"mantel"`, `"mantel.partial"`, `"mantelhaen.test"`, and `"mantel.correlog"`.
#' @param mantel.method2 Character
#' Correlation coefficient used in the Mantel test.
#' One of `"pearson"`, `"kendall"`, or `"spearman"`.
#' @param mantel.alternative Character
#' Alternative hypothesis for Mantel test. One of `"two.sided"`, `"less"`, or `"greater"`.
#' @param drop_nonsig Logical
#' if `TRUE`, non-significant correlations are dropped from the final visualization.
#' @param comparisons Logical (default = TRUE).
#' Whether to perform species-environment correlation or Mantel analysis.
#' If \code{FALSE}, no spec-env links are computed or drawn.
#' @param comparisons_groups List or NULL (default = NULL).
#' When \code{comparisons = TRUE}, constrains which (env_block, spec_block) pairs are analyzed.
#' Each element must be a length-2 character vector: \code{c(env_block_name, spec_block_name)},
#' e.g. \code{list(c("Env01", "Spec01"), c("Env02", "Spec01"))}.
#' Block names must match \code{names(env_select)} and \code{names(spec_select)}.
#' If \code{NULL}, all env-spec block pairs are analyzed (default).
#' @param shape Intrger
#' Integer or numeric specifying the shape of species nodes in the plot (passed to `geom_point()`).
#' @param distance Numeric
#' the offset distance between central nodes and the environmental heatmaps.
#' @param orientation Character
#' Character vector defining which heatmap quadrants to display. Can include any combination of `"top_right"`, `"bottom_right"`, `"top_left"`, and `"bottom_left"`.
#' @param r Numeric
#' radius of the central species layout (in plot units).
#' @param group_layout Character (default = "circle").
#' Arrangement of multiple species networks when \code{spec_select} has multiple elements.
#' Options: \code{"circle"}, \code{"row"}, \code{"column"}, \code{"square"}, \code{"diamond"},
#' \code{"triangle"}, \code{"triangle_down"}, \code{"snake"}.
#' @param anchor_dist Numeric (default = 6).
#' Distance between species networks when multiple \code{spec_select} blocks are used.
#' @param scale_networks Logical (default = TRUE).
#' If \code{TRUE}, normalize each species network to the same scale (radius \code{r}) before placing on anchors.
#' If \code{FALSE}, \code{r} is the minimum network radius; larger networks scale proportionally by node count.
#' @param nrow Integer (default = NULL).
#' Number of rows for \code{group_layout = "row"}, \code{"column"}, or \code{"snake"}.
#' @param ncol Integer (default = NULL).
#' Number of columns for \code{group_layout = "row"}, \code{"column"}, or \code{"snake"}.
#' @param HeatmapLabelSize Numeric (default = 5)
#' Text size for heatmap axis labels (ID/Type).
#' @param HeatmapSigSize Numeric (default = 5)
#' Text size for significance symbols (e.g. `*`, `**`, `***`) inside heatmap tiles.
#' @param HeatmapColorBar NULL or list
#' Controls the colorbar palettes used by each heatmap quadrant.
#'
#' - If `NULL`, the built-in default palettes are used (same as current behavior).
#' - If a list of length 2 with names `low` and `high`, each should be a character
#'   vector of colors (recycled if shorter) used across quadrants.
#' - If a list of length equal to the number of quadrants, each element should be
#'   either `c(low, high)` or `list(low=..., high=...)` for that quadrant (in order).
#' - example `HeatmapColorBar = list(




#'  )`
#' @param HeatmapLabelOrient Numeric (default = 0)
#' Rotation angle (in degrees) for heatmap axis labels (ID/Type).
#' Use this to avoid overlap of the top/bottom labels; e.g. 45 or 90.
#'
#' @param SigLineWidth Numeric vector of length 2 (default = c(0.5, 2))
#' Controls the minimum and maximum line width for species-environment links,
#' scaled by significance (p-value). Smaller p leads to thicker lines.
#' @param SigLineColor Character vector (length 2, default = c("#fdbb84", "#d7301f"))
#' Colors used for the species-environment link color gradient, corresponding to
#' low and high correlation values respectively.
#' @param HeatmapPointSize Numeric (default = 5)
#' Point size for the heatmap diagonal nodes.
#' @param CorePointSize Numeric (default = 8.5)
#' Point size for the central species nodes.
#' @param HeatmapPointFill Character (default = "#de77ae")
#' Fill color for heatmap diagonal points.
#' @param CorePointFill Character (default = "#41b6c4")
#' Fill color for central species nodes.
#' @param HeatmapTileColor Character or NA (default = NA)
#' Border color for heatmap tiles (passed to `geom_tile(colour=...)`).
#' @param HeatmapTileSize Numeric (default = 0)
#' Border line width for heatmap tiles (passed to `geom_tile(size=...)`).
#' @param HeatmapScale Numeric (default = 1)
#' Global scale factor for the overall heatmap size. Values > 1 enlarge the
#' whole heatmap layout, while values < 1 shrink it.
#' @param SigLineAlpha Numeric (default = 0.5)
#' Transparency for species-network to heatmap link lines. Must be between 0 and 1.
#'
#' @param fontsize Numeric (default = 5)
#' (Deprecated) Use `HeatmapLabelSize` instead.
#'
#' @returns A list of length 3:
#' - [[1]]: ggplot object with straight link segments.
#' - [[2]]: ggplot object with curved link segments.
#' - [[3]]: data.frame of full species-environment correlation statistics
#'          (unfiltered, not affected by `drop_nonsig`), with columns
#'          \code{ID}, \code{Type}, \code{Correlation}, \code{Pvalue}, \code{spec_block},
#'          \code{env_block}, and \code{method} (e.g. \code{"correlation"} or \code{"mantel"}).
#' @export
#'
#' @examples
#' \dontrun{
#' # `env` and `spec` are environmental / species abundance data frames.
#' p <- gglink_heatmaps(
#'   env  = env,
#'   spec = spec
#' )
#' }
gglink_heatmaps <- function(
    env,
    spec,
    env_select = NULL,
    spec_select = NULL,
    spec_layout = "circle_outline",
    spec_orientation = c("up","down","left","right"),
    spec_relation = TRUE,
    relation_method = c("correlation", "mantel"),
    cor.method = c("pearson", "kendall", "spearman"),
    cor.use = c("everything", "all", "complete", "pairwise", "na"),
    mantel.method = c("mantel", "mantel.partial", "mantelhaen.test", "mantel.correlog"),
    mantel.method2 = c("pearson", "kendall", "spearman"),
    mantel.alternative = c("two.sided", "less", "greater"),
    drop_nonsig = FALSE,
    comparisons = TRUE,
    comparisons_groups = NULL,
    shape = 22,
    distance = 3,
    HeatmapLabelSize = 5,
    HeatmapSigSize = 5,
    HeatmapColorBar = NULL,
    HeatmapLabelOrient = 0,
    SigLineWidth = c(0.5, 2),
    SigLineColor = c("#fdbb84", "#d7301f"),
    HeatmapPointSize = 5,
    CorePointSize = 8.5,
    HeatmapPointFill = "#de77ae",
    CorePointFill = "#41b6c4",
    HeatmapTileColor = NA,
    HeatmapTileSize = 0,
    HeatmapScale = 1,
    SigLineAlpha = 0.5,
    fontsize = 5,
    orientation = c("top_right", "bottom_right", "top_left","bottom_left"),
    r = 6,
    group_layout = c("circle", "row", "column", "square", "diamond", "triangle", "triangle_down", "snake"),
    anchor_dist = 6,
    scale_networks = TRUE,
    nrow = NULL,
    ncol = NULL
){

  # argument test
  if (is.null(env_select) || is.null(spec_select)) {
    stop("`env_select` and `spec_select` must be provided (non-NULL lists).", call. = FALSE)
  }
  relation_method <- match.arg(relation_method)
  cor.method      <- match.arg(cor.method)
  cor.use         <- match.arg(cor.use)
  mantel.method   <- match.arg(mantel.method)
  mantel.method2  <- match.arg(mantel.method2)
  mantel.alternative <- match.arg(mantel.alternative)
  orientation     <- match.arg(orientation, several.ok = TRUE)
  group_layout    <- match.arg(group_layout)
  anchor_dist     <- as.numeric(anchor_dist)
  if (length(anchor_dist) != 1 || is.na(anchor_dist) || anchor_dist < 0) {
    stop("`anchor_dist` must be a single non-negative numeric value.", call. = FALSE)
  }



  # test
  # env = Envdf_4st
  # spec = Spedf
  # orientation = c("top_right", "bottom_right", "top_left","bottom_left")
  # # orientation = c("top_right","bottom_left")
  # # orientation = "top_right"
  # # orientation = "bottom_right"
  # cor.method = "pearson"
  # cor.use = "pairwise"
  # distance = 4
  # relation_method = "correlation"
  # r = 6
  # spec_relation = T
  # fontsize = 5




  radius = r

  HeatmapLabelSize <- as.numeric(HeatmapLabelSize)
  HeatmapSigSize   <- as.numeric(HeatmapSigSize)
  HeatmapLabelOrient <- as.numeric(HeatmapLabelOrient)
  SigLineWidth    <- as.numeric(SigLineWidth)
  SigLineColor    <- as.character(SigLineColor)
  HeatmapPointSize <- as.numeric(HeatmapPointSize)
  CorePointSize    <- as.numeric(CorePointSize)
  HeatmapPointFill <- as.character(HeatmapPointFill)
  CorePointFill    <- as.character(CorePointFill)
  HeatmapTileColor <- HeatmapTileColor
  HeatmapTileSize  <- as.numeric(HeatmapTileSize)
  HeatmapScale     <- as.numeric(HeatmapScale)
  SigLineAlpha     <- as.numeric(SigLineAlpha)
  if (is.na(HeatmapLabelSize) || length(HeatmapLabelSize) != 1 || HeatmapLabelSize <= 0) {
    stop("`HeatmapLabelSize` must be a single positive numeric value.")
  }
  if (is.na(HeatmapSigSize) || length(HeatmapSigSize) != 1 || HeatmapSigSize <= 0) {
    stop("`HeatmapSigSize` must be a single positive numeric value.")
  }
  if (length(HeatmapLabelOrient) != 1 || !is.finite(HeatmapLabelOrient)) {
    stop("`HeatmapLabelOrient` must be a single finite numeric value (angle in degrees).")
  }
  if (length(SigLineWidth) != 2 || any(!is.finite(SigLineWidth)) || any(SigLineWidth <= 0)) {
    stop("`SigLineWidth` must be a numeric vector of length 2 with positive values.")
  }
  SigLineWidth <- sort(SigLineWidth)
  if (length(SigLineColor) != 2L || any(!nzchar(SigLineColor))) {
    stop("`SigLineColor` must be a character vector of length 2 (low, high).")
  }
  if (length(HeatmapPointSize) != 1 || !is.finite(HeatmapPointSize) || HeatmapPointSize <= 0) {
    stop("`HeatmapPointSize` must be a single positive numeric value.")
  }
  if (length(CorePointSize) != 1 || !is.finite(CorePointSize) || CorePointSize <= 0) {
    stop("`CorePointSize` must be a single positive numeric value.")
  }
  if (length(HeatmapPointFill) != 1L || !nzchar(HeatmapPointFill)) {
    stop("`HeatmapPointFill` must be a non-empty character string.")
  }
  if (length(CorePointFill) != 1L || !nzchar(CorePointFill)) {
    stop("`CorePointFill` must be a non-empty character string.")
  }
  if (length(HeatmapTileSize) != 1 || !is.finite(HeatmapTileSize) || HeatmapTileSize < 0) {
    stop("`HeatmapTileSize` must be a single non-negative numeric value.")
  }
  if (length(HeatmapScale) != 1 || !is.finite(HeatmapScale) || HeatmapScale <= 0) {
    stop("`HeatmapScale` must be a single positive numeric value.")
  }
  if (length(SigLineAlpha) != 1 || !is.finite(SigLineAlpha) || SigLineAlpha < 0 || SigLineAlpha > 1) {
    stop("`SigLineAlpha` must be a single numeric value between 0 and 1.")
  }
  # if env_select = NULL & spec_select = NULL






  ####----split data----####

  # spec_select = list(Spec01 = 1:15)
  #
  # # different env
  # env_select = list(Env01 = 1:14,
  #                   Env02 = 15:26, # 15:28
  #                   Env03 = 29:38, # 29:42
  #                   Env04 = 43:50 # 43:56
  # )

  # equal env
  # env_select = list(Env01 = 1:14,
  #                   Env02 = 15:28,
  #                   Env03 = 29:42,
  #                   Env04 = 43:56)

  # split data
  env_list <- purrr::map(env_select, ~ env[, .x, drop = FALSE])
  spec_list <- purrr::map(spec_select, ~ spec[, .x, drop = FALSE])


  k_vec  <- purrr::map_int(env_list, function(x) ncol(x))
  k_ref  <- max(k_vec)

  # distance controls the extra gap between the central species layout (radius)
  # and the environmental heatmaps.
  distance <- as.numeric(distance)
  if (is.na(distance) || length(distance) != 1) {
    stop("`distance` must be a single numeric value.")
  }
  if (distance < 0) {
    stop("`distance` must be non-negative.")
  }

  length_dist <- max(k_vec)


  k_gap <- length_dist - k_vec


  # purrr::map(env_list, function(x){
  #   cor_out_self <- psych::corr.test(x)
  # })


  env_cor_self_list <- list()


  for (i in seq_along(orientation)) {
    # top_right
    if (orientation[i] == "top_right") {


      cor_out_self <- psych::corr.test(env_list[[i]], use = cor.use, method = cor.method)

      # correlation
      cor_self_r <- cor_out_self$r %>% as.data.frame()
      cor_self_r[upper.tri(cor_self_r)] <- NA

      # pvalue
      cor_self_p <- cor_out_self$p %>% as.data.frame()
      cor_self_p[upper.tri(cor_self_p)] <- NA

      # combine
      cor_self_r <- cor_self_r %>%
        tibble::rownames_to_column(var = "ID") %>%
        tidyr::pivot_longer(cols = -ID,
                            names_to = "Type",
                            values_to = "Correlation") %>%
        dplyr::mutate(ID = factor(ID, levels = unique(ID), ordered = T),
                      Type = factor(Type, levels = rev(unique(Type)), ordered = T),
                      ID2 = as.numeric(ID),
                      Type2 = as.numeric(Type)
        ) %>%
        stats::na.omit()

      cor_self_p <- cor_self_p %>%
        tibble::rownames_to_column(var = "ID") %>%
        tidyr::pivot_longer(cols = -ID,
                            names_to = "Type",
                            values_to = "Pvalue") %>%
        dplyr::mutate(ID = factor(ID, levels = unique(ID), ordered = T),
                      Type = factor(Type, levels = rev(unique(Type)), ordered = T),
                      ID2 = as.numeric(ID),
                      Type2 = as.numeric(Type)) %>%
        stats::na.omit() %>%
        dplyr::mutate(p_signif = dplyr::case_when(
          Pvalue > 0.05 ~ "",
          Pvalue > 0.01 & Pvalue <= 0.05 ~ "*",
          Pvalue < 0.01 & Pvalue >= 0.001 ~ "**",
          Pvalue < 0.001 ~ "***"
        ))

      cor_self_r_p <- cbind(cor_self_r %>% dplyr::select(1,2,4,5,3),
                            cor_self_p %>% dplyr::select(3,6))

      env_cor_self_list[[i]] <- cor_self_r_p

    }

    # bottom_right
    if (orientation[i] == "bottom_right") {

      cor_out_self <- psych::corr.test(env_list[[i]], use = cor.use, method = cor.method)


      # correlation
      cor_self_r <- cor_out_self$r %>% as.data.frame()
      cor_self_r[upper.tri(cor_self_r)] <- NA

      # pvalue
      cor_self_p <- cor_out_self$p %>% as.data.frame()
      cor_self_p[upper.tri(cor_self_p)] <- NA
      # combine
      cor_self_r <- cor_self_r %>%
        tibble::rownames_to_column(var = "ID") %>%
        tidyr::pivot_longer(cols = -ID,
                            names_to = "Type",
                            values_to = "Correlation") %>%
        dplyr::mutate(ID = factor(ID, levels = unique(ID), ordered = T),
                      Type = factor(Type, levels = unique(Type), ordered = T),
                      ID2 = as.numeric(ID),
                      Type2 = as.numeric(Type)
        ) %>%
        stats::na.omit()

      cor_self_p <- cor_self_p %>%
        tibble::rownames_to_column(var = "ID") %>%
        tidyr::pivot_longer(cols = -ID,
                            names_to = "Type",
                            values_to = "Pvalue") %>%
        dplyr::mutate(ID = factor(ID, levels = unique(ID), ordered = T),
                      Type = factor(Type, levels = unique(Type), ordered = T),
                      ID2 = as.numeric(ID),
                      Type2 = as.numeric(Type)) %>%
        stats::na.omit() %>%
        dplyr::mutate(p_signif = dplyr::case_when(
          Pvalue > 0.05 ~ "",
          Pvalue > 0.01 & Pvalue <= 0.05 ~ "*",
          Pvalue < 0.01 & Pvalue >= 0.001 ~ "**",
          Pvalue < 0.001 ~ "***"
        ))

      cor_self_r_p <- cbind(cor_self_r %>% dplyr::select(1,2,4,5,3),
                            cor_self_p %>% dplyr::select(3,6)
      )

      env_cor_self_list[[i]] <- cor_self_r_p
    }

    # top_left
    if (orientation[i] == "top_left") {

      cor_out_self <- psych::corr.test(env_list[[i]], use = cor.use, method = cor.method)

      # correlation
      cor_self_r <- cor_out_self$r %>% as.data.frame()
      cor_self_r[lower.tri(cor_self_r)] <- NA

      # pvalue
      cor_self_p <- cor_out_self$p %>% as.data.frame()
      cor_self_p[lower.tri(cor_self_p)] <- NA

      # combine
      cor_self_r <- cor_self_r %>%
        tibble::rownames_to_column(var = "ID") %>%
        tidyr::pivot_longer(cols = -ID,
                            names_to = "Type",
                            values_to = "Correlation") %>%
        dplyr::mutate(ID = factor(ID, levels = unique(ID), ordered = T),
                      Type = factor(Type, levels = unique(Type), ordered = T),
                      ID2 = as.numeric(ID),
                      Type2 = as.numeric(Type)
        ) %>%
        stats::na.omit()

      cor_self_p <- cor_self_p %>%
        tibble::rownames_to_column(var = "ID") %>%
        tidyr::pivot_longer(cols = -ID,
                            names_to = "Type",
                            values_to = "Pvalue") %>%
        dplyr::mutate(ID = factor(ID, levels = unique(ID), ordered = T),
                      Type = factor(Type, levels = unique(Type), ordered = T),
                      ID2 = as.numeric(ID),
                      Type2 = as.numeric(Type)) %>%
        stats::na.omit() %>%
        dplyr::mutate(p_signif = dplyr::case_when(
          Pvalue > 0.05 ~ "",
          Pvalue > 0.01 & Pvalue <= 0.05 ~ "*",
          Pvalue < 0.01 & Pvalue >= 0.001 ~ "**",
          Pvalue < 0.001 ~ "***"
        ))

      cor_self_r_p <- cbind(cor_self_r %>% dplyr::select(1,2,4,5,3),
                            cor_self_p %>% dplyr::select(3,6)
      )


      env_cor_self_list[[i]] <- cor_self_r_p
    }

    # bottom_left
    if (orientation[i] == "bottom_left") {

      cor_out_self <- psych::corr.test(env_list[[i]], use = cor.use, method = cor.method)

      # correlation
      cor_self_r <- cor_out_self$r %>% as.data.frame()
      cor_self_r[lower.tri(cor_self_r)] <- NA

      # pvalue
      cor_self_p <- cor_out_self$p %>% as.data.frame()
      cor_self_p[lower.tri(cor_self_p)] <- NA

      # combine
      cor_self_r <- cor_self_r %>%
        tibble::rownames_to_column(var = "ID") %>%
        tidyr::pivot_longer(cols = -ID,
                            names_to = "Type",
                            values_to = "Correlation") %>%
        dplyr::mutate(ID = factor(ID, levels = unique(ID), ordered = T),
                      Type = factor(Type, levels = rev(unique(Type)), ordered = T),
                      ID2 = as.numeric(ID),
                      Type2 = as.numeric(Type)
        ) %>%
        stats::na.omit()

      cor_self_p <- cor_self_p %>%
        tibble::rownames_to_column(var = "ID") %>%
        tidyr::pivot_longer(cols = -ID,
                            names_to = "Type",
                            values_to = "Pvalue") %>%
        dplyr::mutate(ID = factor(ID, levels = unique(ID), ordered = T),
                      Type = factor(Type, levels = rev(unique(Type)), ordered = T),
                      ID2 = as.numeric(ID),
                      Type2 = as.numeric(Type)) %>%
        stats::na.omit() %>%
        dplyr::mutate(p_signif = dplyr::case_when(
          Pvalue > 0.05 ~ "",
          Pvalue > 0.01 & Pvalue <= 0.05 ~ "*",
          Pvalue < 0.01 & Pvalue >= 0.001 ~ "**",
          Pvalue < 0.001 ~ "***"
        ))

      cor_self_r_p <- cbind(cor_self_r %>% dplyr::select(1,2,4,5,3),
                            cor_self_p %>% dplyr::select(3,6)
      )


      env_cor_self_list[[i]] <- cor_self_r_p
    }
  }

  env_cor_self_list


  # rename list based on orientation
  names(env_cor_self_list) <- orientation

  # rename k_gap based on orientation
  names(k_gap) <- orientation




  spec_block_names <- names(spec_list)
  if (is.null(spec_block_names)) spec_block_names <- paste0("Spec", seq_along(spec_list))

  env_block_names <- names(env_list)
  if (is.null(env_block_names)) env_block_names <- paste0("Env", seq_along(env_list))

  # comparisons & comparisons_groups: which (env_block, spec_block) pairs to analyze
  if (!is.logical(comparisons) || length(comparisons) != 1 || is.na(comparisons)) {
    stop("`comparisons` must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.null(comparisons_groups)) {
    if (!is.list(comparisons_groups)) {
      stop("`comparisons_groups` must be a list, e.g. list(c('Env01', 'Spec01'), c('Env02', 'Spec01')).", call. = FALSE)
    }
    for (comp in comparisons_groups) {
      if (!is.character(comp) || length(comp) != 2) {
        stop("Each element of `comparisons_groups` must be a character vector of length 2: c(env_block, spec_block).", call. = FALSE)
      }
      if (!comp[1] %in% env_block_names) {
        stop(sprintf("`comparisons_groups`: env_block '%s' not in env_select names.", comp[1]), call. = FALSE)
      }
      if (!comp[2] %in% spec_block_names) {
        stop(sprintf("`comparisons_groups`: spec_block '%s' not in spec_select names.", comp[2]), call. = FALSE)
      }
    }
  }

  # Build (env_block, spec_block) pairs to compute
  if (!isTRUE(comparisons)) {
    pairs_to_compute <- matrix(character(0), nrow = 2, ncol = 0)
  } else if (!is.null(comparisons_groups) && length(comparisons_groups) > 0) {
    pairs_to_compute <- do.call(cbind, lapply(comparisons_groups, function(x) matrix(x, nrow = 2)))
    # Remove duplicates and invalid pairs (validation already done above)
    pairs_to_compute <- unique(as.data.frame(t(pairs_to_compute)))
    pairs_to_compute <- as.matrix(pairs_to_compute)
    if (nrow(pairs_to_compute) > 0) {
      pairs_to_compute <- t(pairs_to_compute)
    } else {
      pairs_to_compute <- matrix(character(0), nrow = 2, ncol = 0)
    }
  } else {
    # Default: all pairs (env_block x spec_block)
    grid_df <- expand.grid(env_block = env_block_names, spec_block = spec_block_names, stringsAsFactors = FALSE)
    pairs_to_compute <- t(as.matrix(grid_df[, c("env_block", "spec_block")]))
  }

  if (relation_method == "correlation") {
    cor_spec_env_parts <- list()
    for (col in seq_len(ncol(pairs_to_compute))) {
      env_blk <- pairs_to_compute[1, col]
      spec_blk <- pairs_to_compute[2, col]
      j <- which(spec_block_names == spec_blk)
      p <- which(env_block_names == env_blk)
      if (length(j) != 1 || length(p) != 1) next
      cor_env_list_tmp <- psych::corr.test(spec_list[[j]], env_list[[p]])
        cor_env_list_tmp_r <- cor_env_list_tmp$r %>%
          as.data.frame() %>%
          tibble::rownames_to_column(var = "ID") %>%
          tidyr::pivot_longer(cols = -ID,
                              names_to = "Type",
                              values_to = "Correlation")
        cor_env_list_tmp_p <- cor_env_list_tmp$p %>%
          as.data.frame() %>%
          tibble::rownames_to_column(var = "ID") %>%
          tidyr::pivot_longer(cols = -ID,
                              names_to = "Type",
                              values_to = "Pvalue") %>%
          dplyr::mutate(p_signif = dplyr::case_when(
            Pvalue > 0.05 ~ "",
            Pvalue > 0.01 & Pvalue <= 0.05 ~ "*",
            Pvalue < 0.01 & Pvalue >= 0.001 ~ "**",
            Pvalue < 0.001 ~ "***"
          ))
        cor_env_list_tmp_r_p <- cbind(cor_env_list_tmp_r,
                                      cor_env_list_tmp_p %>% dplyr::select(3, 4)) %>%
          dplyr::mutate(spec_block = spec_blk, env_block = env_blk)
        cor_spec_env_parts[[length(cor_spec_env_parts) + 1L]] <- cor_env_list_tmp_r_p
    }
    cor_spec_env_list_out <- if (length(cor_spec_env_parts) > 0) {
      do.call(rbind, cor_spec_env_parts) %>%
        dplyr::mutate(method = "correlation")
    } else {
      tibble::tibble(ID = character(), Type = character(), Correlation = numeric(), Pvalue = numeric(),
                     p_signif = character(), spec_block = character(), env_block = character(), method = "correlation")
    }
  } else if (relation_method == "mantel") {
    cor_spec_env_parts <- list()
    for (col in seq_len(ncol(pairs_to_compute))) {
      env_blk <- pairs_to_compute[1, col]
      spec_blk <- pairs_to_compute[2, col]
      j <- which(spec_block_names == spec_blk)
      p <- which(env_block_names == env_blk)
      if (length(j) != 1 || length(p) != 1) next
      mout <- mantel_pairwise(
        spec_df = spec_list[[j]],
        env_df = env_list[[p]],
        method = mantel.method2,
        permutations = 999L,
        na_omit = TRUE
      )
      mout <- mout %>%
        dplyr::mutate(
          spec_block = spec_blk,
          env_block = env_blk,
          p_signif = dplyr::case_when(
            Pvalue > 0.05 ~ "",
            Pvalue > 0.01 & Pvalue <= 0.05 ~ "*",
            Pvalue < 0.01 & Pvalue >= 0.001 ~ "**",
            Pvalue < 0.001 ~ "***",
            TRUE ~ ""
          )
        )
      cor_spec_env_parts[[length(cor_spec_env_parts) + 1L]] <- mout
    }
    cor_spec_env_list_out <- if (length(cor_spec_env_parts) > 0) {
      do.call(rbind, cor_spec_env_parts) %>%
        dplyr::mutate(method = "mantel")
    } else {
      tibble::tibble(ID = character(), Type = character(), Correlation = numeric(), Pvalue = numeric(),
                     p_signif = character(), spec_block = character(), env_block = character(), method = "mantel")
    }
  }

  cor_spec_env_list <- lapply(seq_along(orientation), function(i) {
    cor_spec_env_list_out %>%
      dplyr::filter(.data$env_block == env_block_names[i])
  })
  names(cor_spec_env_list) <- orientation

  cor_spec_env_list_out

  # core location layout: one network per spec block
  n_spec <- length(spec_list)
  valid_spec_layouts <- c("circle_outline", "diamond_outline", "rectangle_outline", "square_outline")
  if (length(spec_layout) == 1) {
    spec_layout <- match.arg(spec_layout, valid_spec_layouts)
    spec_layout_vec <- rep(spec_layout, n_spec)
  } else {
    if (length(spec_layout) != n_spec) {
      stop(sprintf("`spec_layout` must be length 1 or length %d (number of spec blocks).", n_spec), call. = FALSE)
    }
    spec_layout_vec <- vapply(spec_layout, function(x) match.arg(x, valid_spec_layouts), character(1))
  }

  layout_list <- list()

  for (j in seq_len(n_spec)) {
    lay_name <- spec_layout_vec[j]
    func_name <- paste0("create_layout_", lay_name)
    lay_func <- utils::getFromNamespace(func_name, "ggNetView")
    spec_relation_df <- spec_list[[j]]
    n_spec_cols <- ncol(spec_relation_df)
    if (isTRUE(spec_relation) && n_spec_cols >= 2L) {
      spec_cor_out_self <- psych::corr.test(spec_relation_df, use = cor.use, method = cor.method)
      spec_cor_self_r <- spec_cor_out_self$r %>% as.data.frame()
      spec_cor_self_r[lower.tri(spec_cor_self_r)] <- NA
      spec_cor_self_p <- spec_cor_out_self$p %>% as.data.frame()
      spec_cor_self_p[lower.tri(spec_cor_self_p)] <- NA
      spec_cor_self_r <- spec_cor_self_r %>%
        tibble::rownames_to_column(var = "ID") %>%
        tidyr::pivot_longer(cols = -ID, names_to = "Type", values_to = "Correlation") %>%
        dplyr::mutate(ID = factor(ID, levels = unique(ID), ordered = TRUE),
                      Type = factor(Type, levels = unique(Type), ordered = TRUE),
                      ID2 = as.numeric(ID), Type2 = as.numeric(Type)) %>%
        stats::na.omit()
      spec_cor_self_p <- spec_cor_self_p %>%
        tibble::rownames_to_column(var = "ID") %>%
        tidyr::pivot_longer(cols = -ID, names_to = "Type", values_to = "Pvalue") %>%
        dplyr::mutate(ID = factor(ID, levels = unique(ID), ordered = TRUE),
                      Type = factor(Type, levels = unique(Type), ordered = TRUE),
                      ID2 = as.numeric(ID), Type2 = as.numeric(Type)) %>%
        stats::na.omit() %>%
        dplyr::mutate(p_signif = dplyr::case_when(
          Pvalue > 0.05 ~ "", Pvalue > 0.01 & Pvalue <= 0.05 ~ "*",
          Pvalue < 0.01 & Pvalue >= 0.001 ~ "**", Pvalue < 0.001 ~ "***"
        ))
      spec_cor_self_r_p <- cbind(spec_cor_self_r %>% dplyr::select(1, 2, 4, 5, 3),
                                 spec_cor_self_p %>% dplyr::select(3, 6)) %>%
        dplyr::filter(ID != Type) %>%
        dplyr::mutate(tmp = ifelse(ID > Type, paste0(ID, Type), paste0(Type, ID))) %>%
        dplyr::distinct(tmp, .keep_all = TRUE) %>%
        dplyr::select(-tmp) %>%
        dplyr::select(ID, Type, Correlation, p_signif) %>%
        purrr::set_names(c("from", "to", "weight", "sig"))
    } else {
      # spec_relation = FALSE or single-column spec (no within-block correlation)
      spec_cor_self_r_p <- data.frame(
        from = character(0), to = character(0), weight = numeric(0), sig = character(0)
      )
    }
    if (base::nrow(spec_cor_self_r_p) > 0L) {
      spec_graph_obj <- build_graph_from_df(
        df = spec_cor_self_r_p,
        node_annotation = NULL,
        directed = FALSE,
        module.method = "Fast_greedy"
      )
    } else {
      node_names <- colnames(spec_relation_df)
      spec_graph_obj <- igraph::graph_from_data_frame(
        d = data.frame(from = character(0), to = character(0), weight = numeric(0)),
        vertices = data.frame(name = node_names),
        directed = FALSE
      ) %>% tidygraph::as_tbl_graph()
    }
    lay_args <- list(graph_obj = spec_graph_obj, r = radius, node_add = NULL, orientation = spec_orientation)
    lay_args <- lay_args[names(lay_args) %in% names(formals(lay_func))]
    ly_j <- do.call(lay_func, lay_args)
    node_names <- spec_graph_obj %>%
      tidygraph::activate(nodes) %>%
      tidygraph::as_tibble() %>%
      dplyr::pull(name)
    layout_list[[j]] <- data.frame(ID = node_names, x = ly_j$x, y = ly_j$y, spec_block = spec_block_names[j])
  }

  # compute anchors for multi-network placement
  .compute_anchors <- function(n_grp, group_layout, anchor_dist, nrow, ncol) {
    if (group_layout == "circle") {
      angles <- pi / 2 - 2 * pi * (0:(n_grp - 1)) / n_grp
      anchors <- lapply(angles, function(a) c(anchor_dist * cos(a), anchor_dist * sin(a)))
    } else if (group_layout == "row") {
      nr <- if (!is.null(nrow)) as.integer(nrow) else 1L
      nc <- if (!is.null(ncol)) as.integer(ncol) else n_grp
      if (is.null(nrow) && !is.null(ncol)) nr <- max(1L, as.integer(ceiling(n_grp / nc)))
      if (!is.null(nrow) && is.null(ncol)) nc <- max(1L, as.integer(ceiling(n_grp / nr)))
      nr <- max(1L, nr)
      nc <- max(1L, nc)
      anchors <- lapply(seq_len(n_grp), function(i) {
        ii <- i - 1L
        r <- ii %/% nc
        c <- ii %% nc
        c((c - (nc - 1) / 2) * anchor_dist, -((r - (nr - 1) / 2) * anchor_dist))
      })
    } else if (group_layout == "column") {
      nr <- if (!is.null(nrow)) as.integer(nrow) else n_grp
      nc <- if (!is.null(ncol)) as.integer(ncol) else 1L
      if (is.null(nrow) && !is.null(ncol)) nr <- max(1L, as.integer(ceiling(n_grp / nc)))
      if (!is.null(nrow) && is.null(ncol)) nc <- max(1L, as.integer(ceiling(n_grp / nr)))
      nr <- max(1L, nr)
      nc <- max(1L, nc)
      anchors <- lapply(seq_len(n_grp), function(i) {
        ii <- i - 1L
        r <- ii %% nr
        c <- ii %/% nr
        c((c - (nc - 1) / 2) * anchor_dist, -((r - (nr - 1) / 2) * anchor_dist))
      })
    } else if (group_layout == "snake") {
      nr <- if (!is.null(nrow)) as.integer(nrow) else 1L
      nc <- if (!is.null(ncol)) as.integer(ncol) else n_grp
      if (is.null(nrow) && !is.null(ncol)) nr <- max(1L, as.integer(ceiling(n_grp / nc)))
      if (!is.null(nrow) && is.null(ncol)) nc <- max(1L, as.integer(ceiling(n_grp / nr)))
      nr <- max(1L, nr)
      nc <- max(1L, nc)
      anchors <- lapply(seq_len(n_grp), function(i) {
        ii <- i - 1L
        r <- ii %/% nc
        c <- if (r %% 2L == 0L) ii %% nc else (nc - 1L) - (ii %% nc)
        c((c - (nc - 1) / 2) * anchor_dist, -((r - (nr - 1) / 2) * anchor_dist))
      })
    } else if (group_layout %in% c("square", "diamond", "triangle", "triangle_down")) {
      base_angle <- switch(group_layout,
        square = 3 * pi / 4, diamond = pi / 2,
        triangle = pi / 2, triangle_down = -pi / 2
      )
      angles <- base_angle - 2 * pi * (0:(n_grp - 1)) / n_grp
      anchors <- lapply(angles, function(a) c(anchor_dist * cos(a), anchor_dist * sin(a)))
    } else {
      angles <- pi / 2 - 2 * pi * (0:(n_grp - 1)) / n_grp
      anchors <- lapply(angles, function(a) c(anchor_dist * cos(a), anchor_dist * sin(a)))
    }
    do.call(rbind, anchors)
  }

  if (n_spec == 1L) {
    cor_spec_env <- layout_list[[1L]] %>% dplyr::select(ID, x, y)
  } else {
    anchors_df <- .compute_anchors(n_spec, group_layout, anchor_dist, nrow, ncol)
    n_nodes <- vapply(layout_list, function(x) base::nrow(x), integer(1))
    n_min <- min(n_nodes)
    cor_spec_env_parts <- list()
    for (j in seq_len(n_spec)) {
      ly_df <- layout_list[[j]]
      ax <- anchors_df[j, 1]
      ay <- anchors_df[j, 2]
      if (isTRUE(scale_networks)) {
        xmin <- min(ly_df$x)
        xmax <- max(ly_df$x)
        ymin <- min(ly_df$y)
        ymax <- max(ly_df$y)
        xmid <- (xmax + xmin) / 2
        ymid <- (ymax + ymin) / 2
        scale_v <- max(xmax - xmin, ymax - ymin, 1e-8)
        ly_df <- ly_df %>%
          dplyr::mutate(
            x = (x - xmid) / scale_v * (2 * radius) + ax,
            y = (y - ymid) / scale_v * (2 * radius) + ay
          )
      } else {
        # r = minimum radius; larger networks scale by node count (n_j / n_min)
        scale_j <- n_nodes[j] / n_min
        ly_df <- ly_df %>%
          dplyr::mutate(x = x * scale_j + ax, y = y * scale_j + ay)
      }
      cor_spec_env_parts[[j]] <- ly_df %>% dplyr::select(ID, x, y)
    }
    cor_spec_env <- do.call(rbind, cor_spec_env_parts)
  }

  k_vec
  k_gap
  length_dist

  .diag_xy <- function(id_idx, type_idx, ori, k_gap, length_dist, side_anchor, heatmap_step) {
    x_anchor <- if (ori %in% c("top_right", "bottom_right")) side_anchor[["right"]] else side_anchor[["left"]]
    y_anchor <- if (ori %in% c("top_right", "top_left")) side_anchor[["top"]] else side_anchor[["bottom"]]

    x_out <- if (ori %in% c("top_right", "bottom_right")) {
      x_anchor + heatmap_step * (k_gap[[ori]] + id_idx - 1)
    } else {
      -x_anchor - heatmap_step * (length_dist - id_idx)
    }

    y_out <- if (ori %in% c("top_right", "top_left")) {
      y_anchor + heatmap_step * (k_gap[[ori]] + type_idx - 2)
    } else {
      -y_anchor - heatmap_step * (length_dist - type_idx - 1)
    }

    list(x = x_out, y = y_out)
  }

  # Heatmap step and anchor are fixed (independent of r). When r increases,
  # only the network scales; heatmaps stay at the same position and size,
  # since distance has not changed.
  r_ref <- 6
  heatmap_step <- max(1, 0.5 * r_ref / max(length_dist, 1)) * HeatmapScale
  diag_default <- min(max(abs(cor_spec_env$x), na.rm = TRUE),
                      max(abs(cor_spec_env$y), na.rm = TRUE)) / sqrt(2)
  .quad_reach <- function(df, ori, default_val) {
    val <- switch(
      ori,
      top_right = {
        idx <- df$x >= 0 & df$y >= 0
        if (!any(idx)) NA_real_ else max(pmin(df$x[idx], df$y[idx]), na.rm = TRUE)
      },
      bottom_right = {
        idx <- df$x >= 0 & df$y <= 0
        if (!any(idx)) NA_real_ else max(pmin(df$x[idx], -df$y[idx]), na.rm = TRUE)
      },
      top_left = {
        idx <- df$x <= 0 & df$y >= 0
        if (!any(idx)) NA_real_ else max(pmin(-df$x[idx], df$y[idx]), na.rm = TRUE)
      },
      bottom_left = {
        idx <- df$x <= 0 & df$y <= 0
        if (!any(idx)) NA_real_ else max(pmin(-df$x[idx], -df$y[idx]), na.rm = TRUE)
      }
    )
    if (!is.finite(val)) default_val else val
  }
  # Anchor stays at fixed position (r_ref scale) regardless of r; distance unchanged
  current_extent <- max(max(abs(cor_spec_env$x), na.rm = TRUE), max(abs(cor_spec_env$y), na.rm = TRUE), 1e-8)
  quad_reach_raw <- stats::setNames(
    vapply(orientation, function(ori) .quad_reach(cor_spec_env, ori, diag_default), numeric(1)),
    orientation
  )
  quad_anchor <- (quad_reach_raw / current_extent) * r_ref + distance
  fallback_anchor <- (diag_default / current_extent) * r_ref + distance
  side_anchor <- c(
    right = max(c(quad_anchor[intersect(c("top_right", "bottom_right"), orientation)], fallback_anchor), na.rm = TRUE),
    left = max(c(quad_anchor[intersect(c("top_left", "bottom_left"), orientation)], fallback_anchor), na.rm = TRUE),
    top = max(c(quad_anchor[intersect(c("top_right", "top_left"), orientation)], fallback_anchor), na.rm = TRUE),
    bottom = max(c(quad_anchor[intersect(c("bottom_right", "bottom_left"), orientation)], fallback_anchor), na.rm = TRUE)
  )

  # get targets informations

  .make_targets <- function(df, ori, k_gap, length_dist, side_anchor, heatmap_step){
    df_diag <- df %>%
      dplyr::mutate(
        ID = as.character(ID),
        Type = as.character(Type)
      ) %>%
      dplyr::filter(ID == Type)

    xy_diag <- .diag_xy(
      id_idx = df_diag$ID2,
      type_idx = df_diag$Type2,
      ori = ori,
      k_gap = k_gap,
      length_dist = length_dist,
      side_anchor = side_anchor,
      heatmap_step = heatmap_step
    )

    df_diag %>%
      dplyr::transmute(ID, x_to = xy_diag$x, y_to = xy_diag$y)
  }

  xy_targets <- purrr::imap_dfr(
    .x = env_cor_self_list[orientation],
    .f = .make_targets,
    k_gap = k_gap,
    length_dist = length_dist,
    side_anchor = side_anchor,
    heatmap_step = heatmap_step
  )

  cor_spec_env_location <- cor_spec_env_list_out %>%
    dplyr::mutate(ID = as.character(ID), Type = as.character(Type)) %>%
    dplyr::left_join(cor_spec_env, by = "ID") %>%
    dplyr::left_join(xy_targets, by = c("Type" = "ID")) %>%
    dplyr::mutate(
      line_type = dplyr::if_else(.data$Pvalue <= 0.05, "solid", "dashed")
    )

  # Filter only at plotting stage to avoid dropping central nodes
  link_df <- cor_spec_env_location
  if (isTRUE(drop_nonsig)) {
    link_df <- link_df %>% dplyr::filter(.data$Pvalue <= 0.05)
  }

  .offset_env <- function(df, ori, k_gap, length_dist, side_anchor, heatmap_step,
                          HeatmapLabelOrient = 0,
                          y_top_all = NULL, y_bottom_all = NULL){
    stopifnot(ori %in% c("top_right","bottom_right","top_left","bottom_left"))
    df <- df %>% dplyr::mutate(ID = as.character(ID), Type = as.character(Type))
    x_anchor <- if (ori %in% c("top_right", "bottom_right")) side_anchor[["right"]] else side_anchor[["left"]]
    y_anchor <- if (ori %in% c("top_right", "top_left")) side_anchor[["top"]] else side_anchor[["bottom"]]


    x_tile <- if (ori %in% c("top_right","bottom_right")) {
      x_anchor + heatmap_step * (k_gap[[ori]] + df$ID2 - 1)
    } else {
      -x_anchor - heatmap_step * (length_dist - df$ID2)
    }
    y_tile <- if (ori %in% c("top_right","top_left")) {
      y_anchor + heatmap_step * (k_gap[[ori]] + df$Type2 - 1)
    } else {
      -y_anchor - heatmap_step * (length_dist - df$Type2)
    }

    tile <- df %>% dplyr::mutate(x_tile = x_tile, y_tile = y_tile, orientation = ori)


    diag_df <- df %>% dplyr::filter(ID == Type)
    diag_xy <- .diag_xy(
      id_idx = diag_df$ID2,
      type_idx = diag_df$Type2,
      ori = ori,
      k_gap = k_gap,
      length_dist = length_dist,
      side_anchor = side_anchor,
      heatmap_step = heatmap_step
    )
    x_diag <- diag_xy$x
    y_diag <- diag_xy$y
    diag    <- diag_df %>% dplyr::transmute(ID, x_diag, y_diag, orientation = ori)




    if (HeatmapLabelOrient == 0 || is.null(y_top_all) || is.null(y_bottom_all)) {
      y_id_lab <- if (ori %in% c("top_right","top_left")) {
        y_anchor + heatmap_step * length_dist
      } else {
        -y_anchor - heatmap_step * length_dist
      }
    } else {
      if (ori %in% c("top_right","top_left")) {

        y_id_lab <- y_top_all + heatmap_step
      } else {

        y_id_lab <- y_bottom_all - heatmap_step
      }
    }
    x_type_lab <- if (ori %in% c("top_right","bottom_right")) {
      x_anchor + heatmap_step * length_dist
    } else {
      -x_anchor - heatmap_step * length_dist
    }
    hjust_type <- if (ori %in% c("top_right","bottom_right")) "left" else "right"

    id_lab <- df %>%
      dplyr::distinct(ID, .keep_all = TRUE) %>%
      dplyr::transmute(ID,
                       x_id = if (ori %in% c("top_right","bottom_right")) {
                         x_anchor + heatmap_step * (k_gap[[ori]] + ID2 - 1)
                       } else {
                         -x_anchor - heatmap_step * (length_dist - ID2)
                       },
                       y_id = y_id_lab, orientation = ori)

    type_lab <- df %>%
      dplyr::distinct(Type, .keep_all = TRUE) %>%
      dplyr::transmute(Type,
                       x_type = x_type_lab,
                       y_type = if (ori %in% c("top_right","top_left")) {
                         y_anchor + heatmap_step * (k_gap[[ori]] + Type2 - 1)
                       } else {
                         -y_anchor - heatmap_step * (length_dist - Type2)
                       },
                       hjust_type = hjust_type, orientation = ori)

    list(tile = tile, diag = diag, id_lab = id_lab, type_lab = type_lab)
  }

  .add_quadrant_layers <- function(p,
                                   pack,
                                   idx,
                                   scale_name = "Env",
                                  HeatmapLabelSize = 5,
                                  HeatmapSigSize = 5,
                                  HeatmapLabelOrient = 0,
                                  HeatmapTileColor = NA,
                                  HeatmapTileSize = 0,
                                   low_pal  = c("#4d9221", "#8073ac", "#4393c3", "#66bd63"),
                                   high_pal = c("#c51b7d", "#e08214", "#d6604d", "#f46d43")){
    tile     <- pack$tile
    diag     <- pack$diag
    id_lab   <- pack$id_lab
    type_lab <- pack$type_lab


    ori_lab <- unique(id_lab$orientation)[1]
    hjust_id <- dplyr::case_when(
      ori_lab %in% c("top_right", "top_left")       ~ 0,
      ori_lab %in% c("bottom_right", "bottom_left") ~ 1,
      TRUE ~ 0.5
    )
    vjust_id <- dplyr::case_when(
      ori_lab %in% c("top_right", "top_left")       ~ -0.25,
      ori_lab %in% c("bottom_right", "bottom_left") ~  0.25,
      TRUE ~ 0.5
    )

    p +
      ggplot2::geom_tile(
        data = tile,
        ggplot2::aes(x = x_tile, y = y_tile, fill = Correlation),
        colour = HeatmapTileColor,
        linewidth = HeatmapTileSize
      ) +
      ggplot2::geom_text(data = tile, ggplot2::aes(x = x_tile, y = y_tile, label = p_signif), size = HeatmapSigSize) +
      ggplot2::geom_text(data = id_lab, ggplot2::aes(x = x_id,   y = y_id,   label = ID),
                size = HeatmapLabelSize,
                vjust = vjust_id,
                hjust = hjust_id,
                angle = HeatmapLabelOrient) +
      ggplot2::geom_text(data = type_lab, ggplot2::aes(x = x_type, y = y_type, label = Type),
                hjust = type_lab$hjust_type[1],
                size = HeatmapLabelSize) +
      ggplot2::geom_point(data = diag, ggplot2::aes(x = x_diag, y = y_diag),
                 shape = 21, fill = HeatmapPointFill, size = HeatmapPointSize) +
      ggplot2::scale_fill_gradient2(
        low = low_pal[idx], mid = "#ffffff", high = high_pal[idx],
        midpoint = 0, name = paste0(scale_name, " ", idx),
        guide = ggplot2::guide_colorbar(order = idx)
      )
  }

  # Resolve heatmap palettes (per-quadrant low/high)
  .resolve_heatmap_pal <- function(heatmap_colorbar, n_quad,
                                   low_default, high_default) {
    low <- rep(low_default, length.out = n_quad)
    high <- rep(high_default, length.out = n_quad)

    if (is.null(heatmap_colorbar)) {
      return(list(low = low, high = high))
    }

    if (is.list(heatmap_colorbar) &&
        length(heatmap_colorbar) == 2 &&
        all(c("low", "high") %in% names(heatmap_colorbar))) {
      low_in <- heatmap_colorbar$low
      high_in <- heatmap_colorbar$high
      if (!is.character(low_in) || !is.character(high_in)) {
        stop("`heatmap_colorbar$low` and `heatmap_colorbar$high` must be character color vectors.")
      }
      low <- rep(low_in, length.out = n_quad)
      high <- rep(high_in, length.out = n_quad)
      return(list(low = low, high = high))
    }

    if (is.list(heatmap_colorbar) && length(heatmap_colorbar) == n_quad) {
      for (i in seq_len(n_quad)) {
        el <- heatmap_colorbar[[i]]
        if (is.list(el) && all(c("low", "high") %in% names(el))) {
          low[i] <- as.character(el$low)[1]
          high[i] <- as.character(el$high)[1]
        } else if (is.atomic(el) && length(el) >= 2) {
          low[i] <- as.character(el[[1]])[1]
          high[i] <- as.character(el[[2]])[1]
        } else {
          stop("`heatmap_colorbar[[i]]` must be `c(low, high)` or `list(low=..., high=...)`.")
        }
      }
      return(list(low = low, high = high))
    }

    stop("`heatmap_colorbar` must be NULL, a list(low=..., high=...), or a per-quadrant list.")
  }


  .compute_y_range <- function(df, ori, k_gap, length_dist, side_anchor, heatmap_step){
    df <- df %>% dplyr::mutate(ID = as.character(ID), Type = as.character(Type))
    y_anchor <- if (ori %in% c("top_right", "top_left")) side_anchor[["top"]] else side_anchor[["bottom"]]
    y_tile <- if (ori %in% c("top_right","top_left")) {
      y_anchor + heatmap_step * (k_gap[[ori]] + df$Type2 - 1)
    } else {
      -y_anchor - heatmap_step * (length_dist - df$Type2)
    }
    data.frame(
      orientation = ori,
      ymin = min(y_tile, na.rm = TRUE),
      ymax = max(y_tile, na.rm = TRUE)
    )
  }

  y_ranges <- purrr::imap_dfr(
    env_cor_self_list[orientation],
    ~ .compute_y_range(.x, .y, k_gap, length_dist, side_anchor, heatmap_step)
  )

  y_top_all <- y_ranges %>%
    dplyr::filter(.data$orientation %in% c("top_right","top_left")) %>%
    dplyr::pull(.data$ymax) %>%
    max(na.rm = TRUE)

  y_bottom_all <- y_ranges %>%
    dplyr::filter(.data$orientation %in% c("bottom_right","bottom_left")) %>%
    dplyr::pull(.data$ymin) %>%
    min(na.rm = TRUE)


  packs <- purrr::imap(
    env_cor_self_list[orientation],
    ~ .offset_env(.x, .y, k_gap, length_dist, side_anchor, heatmap_step,
                  HeatmapLabelOrient = HeatmapLabelOrient,
                  y_top_all = y_top_all,
                  y_bottom_all = y_bottom_all)
  )


  diag_all <- purrr::map_dfr(packs, "diag")

  p0 <- ggplot2::ggplot()

  pal_default_low  <- c("#4d9221", "#8073ac", "#4393c3", "#66bd63")
  pal_default_high <- c("#c51b7d", "#e08214", "#d6604d", "#f46d43")
  pal <- .resolve_heatmap_pal(HeatmapColorBar, length(packs), pal_default_low, pal_default_high)

  for (i in seq_along(packs)) {
    if (i > 1) p0 <- p0 + ggnewscale::new_scale_fill()
    p0 <- .add_quadrant_layers(
      p0, packs[[i]], idx = i, scale_name = "Env",
      HeatmapLabelSize = HeatmapLabelSize,
      HeatmapSigSize = HeatmapSigSize,
      HeatmapLabelOrient = HeatmapLabelOrient,
      HeatmapTileColor = HeatmapTileColor,
      HeatmapTileSize = HeatmapTileSize,
      low_pal = pal$low,
      high_pal = pal$high
    )
  }

  p0

  p1 <- p0 +
    ggnewscale::new_scale_color() +
    ggplot2::geom_segment(
      data = link_df,
      ggplot2::aes(x = x, y = y, xend = x_to, yend = y_to,
          color = Correlation,
          linetype = line_type,
          linewidth = -log10(Pvalue)),
      alpha = SigLineAlpha
    ) +
    ggplot2::scale_color_gradient(low = SigLineColor[1], high = SigLineColor[2]) +
    ggplot2::scale_linewidth_continuous(range = SigLineWidth) +
    ggplot2::scale_linetype_identity() +

    ggplot2::geom_point(
      data = diag_all,
      ggplot2::aes(x = x_diag, y = y_diag),
      shape = 21, fill = HeatmapPointFill, size = HeatmapPointSize
    ) +
    ggplot2::geom_point(
      data = cor_spec_env,
      ggplot2::aes(x = x, y = y), shape = 21, fill = CorePointFill, size = CorePointSize
    ) +
    ggplot2::geom_text(
      data = cor_spec_env,
      ggplot2::aes(x = x, y = y, label = ID), size = 5
    ) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::theme_void() +
    ggplot2::theme(
      plot.margin = ggplot2::margin(10, 10, 10, 10),
      aspect.ratio = 1,
      legend.position = "top"
    )

  p1


  p2 <- p0 +
    ggnewscale::new_scale_color() +
    ggplot2::geom_segment(
      data = link_df,
      ggplot2::aes(x = x, y = y, xend = x_to, yend = y_to,
          color = Correlation,
          linetype = line_type,
          linewidth = -log10(Pvalue)),
      alpha = SigLineAlpha
    ) +
    ggplot2::scale_color_gradient(low = SigLineColor[1], high = SigLineColor[2]) +
    ggplot2::scale_linewidth_continuous(range = SigLineWidth) +
    ggplot2::scale_linetype_identity() +
    ggplot2::geom_point(
      data = diag_all,
      ggplot2::aes(x = x_diag, y = y_diag),
      shape = 21, fill = HeatmapPointFill, size = HeatmapPointSize
    ) +
    ggplot2::geom_point(
      data = cor_spec_env,
      ggplot2::aes(x = x, y = y), shape = 21, fill = CorePointFill, size = CorePointSize
    ) +
    ggplot2::geom_text(
      data = cor_spec_env,
      ggplot2::aes(x = x, y = y, label = ID),
      size = 5
    ) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::theme_void() +
    ggplot2::theme(
      plot.margin = ggplot2::margin(10, 10, 10, 10),
      aspect.ratio = 1,
      legend.position = "top"
    )

  p2 <- p0 +
    ggnewscale::new_scale_fill() +
    ggplot2::geom_curve(data = link_df,
              mapping = ggplot2::aes(x = x, y = y, xend = x_to, yend = y_to,
                            color = Correlation,
                            linetype = line_type,
                            linewidth = -log10(Pvalue)),
               alpha = SigLineAlpha,
               curvature = 0.25
    ) +
    ggplot2::scale_color_gradient(low = SigLineColor[1], high = SigLineColor[2]) +
    ggplot2::scale_linewidth_continuous(range = SigLineWidth) +
    ggplot2::scale_linetype_identity() +
    ggplot2::geom_point(
      data = diag_all,
      ggplot2::aes(x = x_diag, y = y_diag),
      shape = 21, fill = HeatmapPointFill, size = HeatmapPointSize
    ) +
    ggplot2::geom_point(data = cor_spec_env,
               mapping = ggplot2::aes(x = x, y = y, fill = ID),
               shape = 21,
               fill = CorePointFill,
               size = CorePointSize) +
    ggplot2::geom_text(data = cor_spec_env,
              mapping = ggplot2::aes(x =x, y = y, label = ID),
              size = 5) +
    # geom_line(data = cor_spec_env_location %>% dplyr::distinct(ID, .keep_all = T) %>% dplyr::select(ID, x, y),
    #           mapping = ggplot2::aes(x = x, y = y, group = 1),
    #           linetype = 1,
    #           linewidth = 1.5,
    #           color = "#41b6c4") +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::theme_void() +
    ggplot2::theme(
      plot.margin = ggplot2::margin(1, 1, 1, 1, "cm"),
      aspect.ratio = 1,
      legend.position = "top"
    )

  p2

  return(list(p1, p2, cor_spec_env_list_out))

}


