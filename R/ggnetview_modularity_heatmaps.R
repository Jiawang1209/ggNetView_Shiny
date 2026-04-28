#' Compute module eigengene (PC1) from OTU abundance matrix
#'
#' For each module, performs PCA on the module members' abundance matrix
#' (samples x OTUs) and returns the first principal component as the
#' module eigengene. This represents the "overall activity/state" of
#' each module across samples.
#'
#' @param otu_mat Numeric matrix. Rows = OTUs/ASVs, columns = samples.
#'   Must have rownames (OTU IDs) and colnames (sample IDs).
#' @param graph_obj A tbl_graph from build_graph_from_mat or build_graph_from_df.
#'   Must have node attribute \code{name} and a module column.
#' @param module_col Character. Name of the module column in graph_obj nodes.
#'   One of \code{"Modularity"}, \code{"modularity3"}, \code{"modularity2"}.
#' @param exclude_others Logical. If TRUE, exclude "Others" module from output.
#' @param scale_pca Logical. If TRUE, scale variables before PCA (recommended).
#'
#' @return A data frame with samples as rows and module eigengenes as columns.
#'   Column names are module names.
#' @keywords internal
get_module_eigengene <- function(otu_mat,
                                 graph_obj,
                                 module_col = "Modularity",
                                 exclude_others = TRUE,
                                 scale_pca = TRUE) {
  node_df <- graph_obj %>%
    tidygraph::activate(nodes) %>%
    tidygraph::as_tibble()

  if (!"name" %in% colnames(node_df)) {
    stop("`graph_obj` nodes must have a `name` column.", call. = FALSE)
  }

  mod_candidates <- c("Modularity", "modularity3", "modularity2")
  if (!module_col %in% colnames(node_df)) {
    hit <- mod_candidates[mod_candidates %in% colnames(node_df)]
    module_col <- if (length(hit) > 0) hit[1] else stop("No module column found.", call. = FALSE)
  }

  if (is.null(rownames(otu_mat)) || is.null(colnames(otu_mat))) {
    stop("`otu_mat` must have rownames (OTU IDs) and colnames (sample IDs).", call. = FALSE)
  }

  otu_mat <- as.matrix(otu_mat)
  node_names <- as.character(node_df$name)
  modules <- as.character(node_df[[module_col]])

  if (exclude_others) {
    keep <- modules != "Others"
    node_names <- node_names[keep]
    modules <- modules[keep]
  }

  mod_levels <- unique(modules)
  mod_levels <- mod_levels[mod_levels != "Others"]
  if (length(mod_levels) == 0) {
    stop("No non-Others modules found in graph_obj.", call. = FALSE)
  }

  eigengene_list <- list()
  for (mod in mod_levels) {
    otu_in_mod <- node_names[modules == mod]
    otu_in_mat <- intersect(otu_in_mod, rownames(otu_mat))
    if (length(otu_in_mat) < 2L) next

    mat_mod <- t(otu_mat[otu_in_mat, , drop = FALSE])
    if (any(!is.finite(mat_mod))) {
      mat_mod[!is.finite(mat_mod)] <- 0
    }
    pca_out <- stats::prcomp(mat_mod, center = TRUE, scale. = scale_pca)
    pc1 <- pca_out$x[, 1L, drop = TRUE]
    eigengene_list[[mod]] <- pc1
  }

  if (length(eigengene_list) == 0) {
    stop("Could not compute eigengene for any module (need >= 2 OTUs per module).", call. = FALSE)
  }

  out <- as.data.frame(eigengene_list)
  colnames(out) <- names(eigengene_list)
  rownames(out) <- colnames(otu_mat)
  out
}


#' Compute module relative abundance index
#'
#' For each module, computes the sum or mean of member OTU abundances
#' per sample. Simpler than eigengene but more "abundance-focused".
#'
#' @param otu_mat Numeric matrix. Rows = OTUs, columns = samples.
#' @param graph_obj A tbl_graph with \code{name} and module column.
#' @param module_col Character. Module column name.
#' @param type Character. \code{"sum"} (total abundance) or \code{"mean"}
#'   (average abundance per OTU).
#' @param exclude_others Logical. Exclude "Others" module.
#'
#' @return A data frame with samples as rows and modules as columns.
#' @keywords internal
get_module_abundance <- function(otu_mat,
                                graph_obj,
                                module_col = "Modularity",
                                type = c("sum", "mean"),
                                exclude_others = TRUE) {
  type <- match.arg(type)
  node_df <- graph_obj %>%
    tidygraph::activate(nodes) %>%
    tidygraph::as_tibble()

  mod_candidates <- c("Modularity", "modularity3", "modularity2")
  if (!module_col %in% colnames(node_df)) {
    hit <- mod_candidates[mod_candidates %in% colnames(node_df)]
    module_col <- if (length(hit) > 0) hit[1] else stop("No module column found.", call. = FALSE)
  }

  otu_mat <- as.matrix(otu_mat)
  node_names <- as.character(node_df$name)
  modules <- as.character(node_df[[module_col]])

  if (exclude_others) {
    keep <- modules != "Others"
    node_names <- node_names[keep]
    modules <- modules[keep]
  }

  mod_levels <- unique(modules)
  mod_levels <- mod_levels[mod_levels != "Others"]

  ab_list <- list()
  for (mod in mod_levels) {
    otu_in_mod <- node_names[modules == mod]
    otu_in_mat <- intersect(otu_in_mod, rownames(otu_mat))
    if (length(otu_in_mat) == 0L) next

    mat_mod <- otu_mat[otu_in_mat, , drop = FALSE]
    if (type == "sum") {
      ab_list[[mod]] <- colSums(mat_mod)
    } else {
      ab_list[[mod]] <- colMeans(mat_mod)
    }
  }

  out <- as.data.frame(ab_list)
  colnames(out) <- names(ab_list)
  rownames(out) <- colnames(otu_mat)
  out
}


#' Visualize network with module-environment heatmaps
#'
#' Combines ggNetView network visualization with four environmental correlation
#' heatmaps. Module-environment relationships are computed using module
#' eigengenes (recommended) or module relative abundance, then correlated
#' with environmental factors. Links connect module centroids to heatmap
#' diagonals.
#'
#' @param graph_obj A graph object from \code{build_graph_from_mat} or
#'   \code{build_graph_from_df}.
#' @param env Data frame or matrix of environmental variables (samples as rows).
#' @param otu_mat Numeric matrix. Rows = OTUs/ASVs, columns = samples.
#'   Used to compute module eigengenes or module abundance. Must align with
#'   \code{graph_obj} node names (rownames) and \code{env} sample IDs.
#' @param env_select Named list. Column indices or names for each env block.
#'   Each block corresponds to one heatmap quadrant. E.g.
#'   \code{list(Env01 = 1:5, Env02 = 6:10, Env03 = 11:15, Env04 = 16:20)}.
#' @param module_index Character. How to represent each module for env correlation:
#'   \code{"eigengene"} (default, PC1 of module OTUs) or \code{"abundance"}
#'   (sum/mean of module OTU abundances).
#' @param abundance_type Character. When \code{module_index = "abundance"},
#'   use \code{"sum"} or \code{"mean"}.
#' @param relation_method \code{"correlation"} or \code{"mantel"}.
#' @param cor.method Correlation method: \code{"pearson"}, \code{"kendall"}, \code{"spearman"}.
#' @param cor.use Handling of missing values in correlation.
#' @param mantel.method2 Correlation for Mantel test.
#' @param drop_nonsig Logical. Drop non-significant links from plot.
#' @param layout Character. Layout for ggNetView (e.g. \code{"gephi"}, \code{"square"}).
#' @param layout.module Character. Module ordering strategy passed through to
#'   the underlying ggNetView call. One of \code{"random"} (default),
#'   \code{"adjacent"} or \code{"order"}.
#' @param orientation Character vector. Heatmap quadrants:
#'   \code{"top_right"}, \code{"bottom_right"}, \code{"top_left"}, \code{"bottom_left"}.
#' @param distance Numeric. Gap between the scaled network boundary and the
#'   environmental heatmaps.
#' @param r Numeric. Effective radius for scaling the central network.
#' @param HeatmapScale Numeric (default = 1). Global scale factor for the overall
#'   heatmap size. Values > 1 enlarge the whole heatmap layout.
#' @param HeatmapLabelSize Numeric (default = 5). Text size for the heatmap
#'   row/column labels.
#' @param HeatmapSigSize Numeric (default = 5). Text size for the significance
#'   marks rendered on the heatmap tiles.
#' @param HeatmapColorBar A list of length-2 character vectors giving
#'   (low, high) hex colours, one per heatmap quadrant. \code{NULL} (default)
#'   uses the package colour palette.
#' @param HeatmapLabelOrient Numeric (default = 0). Rotation in degrees applied
#'   to the heatmap labels.
#' @param HeatmapPointSize Numeric (default = 5). Point size for the central
#'   module anchor used to attach the heatmap.
#' @param HeatmapPointFill Character (default = \code{"#de77ae"}). Fill colour
#'   for the central module anchor point.
#' @param HeatmapTileColor Border colour for the heatmap tiles. Default
#'   \code{NA} (no border).
#' @param HeatmapTileSize Numeric (default = 0). Border size for the heatmap
#'   tiles.
#' @param SigLineAlpha Numeric (default = 0.5). Transparency for module-heatmap
#'   link lines. Must be between 0 and 1.
#' @param SigLineWidth Numeric vector of length 2 (default = c(0.5, 2)).
#'   Min and max line width for module-heatmap links. Line width is mapped from
#'   \code{-log10(p-value)}: smaller p (more significant) -> thicker line.
#'   E.g. p=0.05->1.3, p=0.01->2, p=0.001->3; values are scaled to this range.
#' @param SigLineColor Character vector of length 2. Colors for link gradient
#'   (low and high correlation).
#' @param ... Additional arguments passed to layout and network rendering,
#'   including: \code{layout}, \code{layout.module}, \code{shrink},
#'   \code{inner_shrink} (intra-module compactness, only used when
#'   \code{layout = "WGCNA"}; see \code{\link{ggNetView}}), \code{jitter},
#'   \code{add_outer}, \code{add_group_outer}, \code{label} (logical or character:
#'   module labels in ggNetView style), \code{labelsize}, \code{labelsegmentsize},
#'   \code{labelsegmentalpha}, \code{fill}, \code{color}, \code{pointsize}.
#'
#' @return A list: \code{[[1]]} ggplot with straight links, \code{[[2]]} ggplot
#'   with curved links, \code{[[3]]} data frame of module-env correlation stats.
#' @export
ggnetview_modularity_heatmaps <- function(
    graph_obj,
    env,
    otu_mat,
    env_select = NULL,
    module_index = c("eigengene", "abundance"),
    abundance_type = c("sum", "mean"),
    relation_method = c("correlation", "mantel"),
    cor.method = c("pearson", "kendall", "spearman"),
    cor.use = c("everything", "all", "complete", "pairwise", "na"),
    mantel.method2 = c("pearson", "kendall", "spearman"),
    drop_nonsig = FALSE,
    layout = "gephi",
    layout.module = c("random", "adjacent", "order"),
    orientation = c("top_right", "bottom_right", "top_left", "bottom_left"),
    distance = 3,
    r = 6,
    HeatmapScale = 1,
    SigLineAlpha = 0.5,
    HeatmapLabelSize = 5,
    HeatmapSigSize = 5,
    HeatmapColorBar = NULL,
    HeatmapLabelOrient = 0,
    SigLineWidth = c(0.5, 2),
    SigLineColor = c("#fdbb84", "#d7301f"),
    HeatmapPointSize = 5,
    HeatmapPointFill = "#de77ae",
    HeatmapTileColor = NA,
    HeatmapTileSize = 0,
    ...) {

  module_index <- match.arg(module_index)
  abundance_type <- match.arg(abundance_type)
  relation_method <- match.arg(relation_method)
  cor.method <- match.arg(cor.method)
  cor.use <- match.arg(cor.use)
  mantel.method2 <- match.arg(mantel.method2)
  orientation <- match.arg(orientation, several.ok = TRUE)

  distance <- as.numeric(distance)
  if (length(distance) != 1 || is.na(distance) || distance < 0) {
    stop("`distance` must be a single non-negative numeric value.", call. = FALSE)
  }
  r <- as.numeric(r)
  if (length(r) != 1 || is.na(r) || r <= 0) {
    stop("`r` must be a single positive numeric value.", call. = FALSE)
  }
  HeatmapScale <- as.numeric(HeatmapScale)
  SigLineAlpha <- as.numeric(SigLineAlpha)
  if (length(HeatmapScale) != 1 || !is.finite(HeatmapScale) || HeatmapScale <= 0) {
    stop("`HeatmapScale` must be a single positive numeric value.", call. = FALSE)
  }
  if (length(SigLineAlpha) != 1 || !is.finite(SigLineAlpha) || SigLineAlpha < 0 || SigLineAlpha > 1) {
    stop("`SigLineAlpha` must be a single numeric value between 0 and 1.", call. = FALSE)
  }

  if (is.null(env_select)) {
    stop("`env_select` must be provided (named list of env column indices/names).", call. = FALSE)
  }

  env <- as.data.frame(env)
  if (is.data.frame(otu_mat)) otu_mat <- as.matrix(otu_mat)

  if (length(env_select) != length(orientation)) {
    stop("`env_select` must have the same length as `orientation` (one env block per heatmap quadrant).", call. = FALSE)
  }
  env_list <- purrr::map(env_select, ~ env[, .x, drop = FALSE])
  env_block_names <- names(env_list)
  if (is.null(env_block_names)) env_block_names <- paste0("Env", seq_along(env_list))
  names(env_list) <- orientation

  k_vec <- purrr::map_int(env_list, ncol)
  length_dist <- max(k_vec)
  k_gap <- length_dist - k_vec
  names(k_gap) <- orientation

  sample_ids <- rownames(env)
  if (is.null(sample_ids)) sample_ids <- as.character(seq_len(nrow(env)))

  otu_samples <- colnames(otu_mat)
  common_samples <- intersect(sample_ids, otu_samples)
  if (length(common_samples) < 3L) {
    stop("`env` and `otu_mat` must share at least 3 sample IDs.", call. = FALSE)
  }

  env <- env[common_samples, , drop = FALSE]
  otu_mat <- otu_mat[, common_samples, drop = FALSE]

  if (module_index == "eigengene") {
    spec_df <- get_module_eigengene(otu_mat, graph_obj, exclude_others = TRUE)
  } else {
    spec_df <- get_module_abundance(otu_mat, graph_obj, type = abundance_type, exclude_others = TRUE)
  }

  spec_df <- spec_df[common_samples, , drop = FALSE]
  spec_list <- list(Modules = spec_df)
  spec_block_names <- "Modules"

  env_list <- purrr::map(env_select, ~ env[common_samples, .x, drop = FALSE])
  names(env_list) <- env_block_names

  pairs_to_compute <- expand.grid(
    env_block = env_block_names,
    spec_block = spec_block_names,
    stringsAsFactors = FALSE
  )
  pairs_to_compute <- as.matrix(pairs_to_compute[, c("env_block", "spec_block")])

  if (relation_method == "correlation") {
    cor_parts <- list()
    for (col in seq_len(nrow(pairs_to_compute))) {
      env_blk <- pairs_to_compute[col, 1]
      spec_blk <- pairs_to_compute[col, 2]
      j <- which(spec_block_names == spec_blk)
      p <- which(env_block_names == env_blk)
      if (length(j) != 1 || length(p) != 1) next
      cor_out <- psych::corr.test(spec_list[[j]], env_list[[p]], use = cor.use, method = cor.method)
      cor_r <- cor_out$r %>%
        as.data.frame() %>%
        tibble::rownames_to_column(var = "ID") %>%
        tidyr::pivot_longer(cols = -ID, names_to = "Type", values_to = "Correlation")
      cor_p <- cor_out$p %>%
        as.data.frame() %>%
        tibble::rownames_to_column(var = "ID") %>%
        tidyr::pivot_longer(cols = -ID, names_to = "Type", values_to = "Pvalue") %>%
        dplyr::mutate(p_signif = dplyr::case_when(
          Pvalue > 0.05 ~ "",
          Pvalue > 0.01 & Pvalue <= 0.05 ~ "*",
          Pvalue < 0.01 & Pvalue >= 0.001 ~ "**",
          Pvalue < 0.001 ~ "***",
          TRUE ~ ""
        ))
      cor_parts[[length(cor_parts) + 1L]] <- cbind(
        cor_r,
        cor_p %>% dplyr::select(Pvalue, p_signif)
      ) %>% dplyr::mutate(spec_block = spec_blk, env_block = env_blk)
    }
    cor_spec_env_list_out <- if (length(cor_parts) > 0) {
      do.call(rbind, cor_parts) %>% dplyr::mutate(method = "correlation")
    } else {
      tibble::tibble(ID = character(), Type = character(), Correlation = numeric(),
                    Pvalue = numeric(), p_signif = character(),
                    spec_block = character(), env_block = character(), method = "correlation")
    }
  } else {
    cor_parts <- list()
    for (col in seq_len(nrow(pairs_to_compute))) {
      env_blk <- pairs_to_compute[col, 1]
      spec_blk <- pairs_to_compute[col, 2]
      j <- which(spec_block_names == spec_blk)
      p <- which(env_block_names == env_blk)
      if (length(j) != 1 || length(p) != 1) next
      mout <- mantel_pairwise(
        spec_df = spec_list[[j]],
        env_df = env_list[[p]],
        method = mantel.method2,
        permutations = 999L,
        na_omit = TRUE
      ) %>%
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
      cor_parts[[length(cor_parts) + 1L]] <- mout
    }
    cor_spec_env_list_out <- if (length(cor_parts) > 0) {
      do.call(rbind, cor_parts) %>% dplyr::mutate(method = "mantel")
    } else {
      tibble::tibble(ID = character(), Type = character(), Correlation = numeric(),
                    Pvalue = numeric(), p_signif = character(),
                    spec_block = character(), env_block = character(), method = "mantel")
    }
  }

  env_cor_self_list <- list()
  for (i in seq_along(orientation)) {
    ori <- orientation[i]
    cor_out_self <- psych::corr.test(env_list[[i]], use = cor.use, method = cor.method)
    cor_self_r <- cor_out_self$r %>% as.data.frame()
    cor_self_p <- cor_out_self$p %>% as.data.frame()

    if (ori %in% c("top_right", "bottom_right")) {
      cor_self_r[upper.tri(cor_self_r)] <- NA
      cor_self_p[upper.tri(cor_self_p)] <- NA
    } else {
      cor_self_r[lower.tri(cor_self_r)] <- NA
      cor_self_p[lower.tri(cor_self_p)] <- NA
    }

    cor_self_r <- cor_self_r %>%
      tibble::rownames_to_column(var = "ID") %>%
      tidyr::pivot_longer(cols = -ID, names_to = "Type", values_to = "Correlation") %>%
      dplyr::mutate(
        ID = factor(ID, levels = unique(ID), ordered = TRUE),
        Type = factor(Type, levels = if (ori %in% c("top_right", "bottom_left")) rev(unique(Type)) else unique(Type), ordered = TRUE),
        ID2 = as.numeric(ID),
        Type2 = as.numeric(Type)
      ) %>%
      stats::na.omit()

    cor_self_p <- cor_self_p %>%
      tibble::rownames_to_column(var = "ID") %>%
      tidyr::pivot_longer(cols = -ID, names_to = "Type", values_to = "Pvalue") %>%
      dplyr::mutate(
        ID = factor(ID, levels = unique(ID), ordered = TRUE),
        Type = factor(Type, levels = if (ori %in% c("top_right", "bottom_left")) rev(unique(Type)) else unique(Type), ordered = TRUE),
        ID2 = as.numeric(ID),
        Type2 = as.numeric(Type)
      ) %>%
      stats::na.omit() %>%
      dplyr::mutate(p_signif = dplyr::case_when(
        Pvalue > 0.05 ~ "",
        Pvalue > 0.01 & Pvalue <= 0.05 ~ "*",
        Pvalue < 0.01 & Pvalue >= 0.001 ~ "**",
        Pvalue < 0.001 ~ "***",
        TRUE ~ ""
      ))

    env_cor_self_list[[i]] <- cbind(
      cor_self_r %>% dplyr::select(ID, Type, ID2, Type2, Correlation),
      cor_self_p %>% dplyr::select(Pvalue, p_signif)
    )
  }
  names(env_cor_self_list) <- orientation

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

  .make_targets <- function(df, ori, k_gap, length_dist, side_anchor, heatmap_step) {
    df_diag <- df %>%
      dplyr::mutate(ID = as.character(ID), Type = as.character(Type)) %>%
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

  layout.module <- match.arg(layout.module)
  ggnetview_args <- list(...)

  gv_call_args <- list(
    graph_obj = graph_obj,
    layout = layout,
    layout.module = layout.module,
    r = 1,
    scale_radius = r,
    return_layout = TRUE,
    node_add = if (!is.null(ggnetview_args$node_add)) ggnetview_args$node_add else 7,
    scale = if (!is.null(ggnetview_args$scale)) ggnetview_args$scale else TRUE,
    anchor_dist = if (!is.null(ggnetview_args$anchor_dist)) ggnetview_args$anchor_dist else 6,
    orientation = if (!is.null(ggnetview_args$orientation)) ggnetview_args$orientation else "up",
    angle = if (!is.null(ggnetview_args$angle)) ggnetview_args$angle else 0,
    center = if (!is.null(ggnetview_args$center)) ggnetview_args$center else TRUE,
    shrink = if (!is.null(ggnetview_args$shrink)) ggnetview_args$shrink else 1,
    inner_shrink = if (!is.null(ggnetview_args$inner_shrink)) ggnetview_args$inner_shrink else 1,
    jitter = if (!is.null(ggnetview_args$jitter)) ggnetview_args$jitter else FALSE,
    jitter_sd = if (!is.null(ggnetview_args$jitter_sd)) ggnetview_args$jitter_sd else 0.1,
    k_nn = if (!is.null(ggnetview_args$k_nn)) ggnetview_args$k_nn else 12,
    push_others_delta = if (!is.null(ggnetview_args$push_others_delta)) ggnetview_args$push_others_delta else 0,
    add_outer = if (!is.null(ggnetview_args$add_outer)) ggnetview_args$add_outer else FALSE,
    fill = ggnetview_args$fill,
    color = ggnetview_args$color,
    pointsize = ggnetview_args$pointsize,
    seed = if (!is.null(ggnetview_args$seed)) ggnetview_args$seed else 1115
  )
  if (layout %in% c("consensus_module_equal_gephi", "consensus_module_gephi")) {
    gv_call_args$nrow <- ggnetview_args$nrow
    gv_call_args$ncol <- ggnetview_args$ncol
  }
  gv_formals <- names(formals(ggNetView))
  gv_call_args <- gv_call_args[names(gv_call_args) %in% gv_formals]

  gv_out <- do.call(ggNetView, gv_call_args)
  if (!is.list(gv_out) || !"layout_data" %in% names(gv_out)) {
    stop("ggNetView did not return layout_data. Ensure layout produces modular structure.", call. = FALSE)
  }

  layout_data <- gv_out$layout_data
  graph_ly_scaled <- layout_data$graph_ly_final
  ggplot_data <- layout_data$ggplot_data
  module_centroids <- layout_data$module_centroids %>%
    dplyr::filter(ID %in% colnames(spec_df))

  if (nrow(module_centroids) == 0) {
    stop("No module centroids could be computed. No overlap between modules and spec columns.", call. = FALSE)
  }

  net_bounds <- list(
    xmin = min(graph_ly_scaled$x, na.rm = TRUE),
    xmax = max(graph_ly_scaled$x, na.rm = TRUE),
    ymin = min(graph_ly_scaled$y, na.rm = TRUE),
    ymax = max(graph_ly_scaled$y, na.rm = TRUE)
  )

  # Heatmap step and anchor are fixed (independent of r). When r increases,
  # only the network scales; heatmaps stay at the same position and size,
  # since distance has not changed.
  r_ref <- 6
  heatmap_step <- max(1, 0.5 * r_ref / max(length_dist, 1)) * HeatmapScale
  diag_default <- min(max(abs(graph_ly_scaled$x), na.rm = TRUE),
                      max(abs(graph_ly_scaled$y), na.rm = TRUE)) / sqrt(2)
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
  quad_reach_raw <- stats::setNames(
    vapply(orientation, function(ori) .quad_reach(graph_ly_scaled, ori, diag_default), numeric(1)),
    orientation
  )
  quad_anchor <- (quad_reach_raw / r) * r_ref + distance
  fallback_anchor <- (diag_default / r) * r_ref + distance
  side_anchor <- c(
    right = max(c(quad_anchor[intersect(c("top_right", "bottom_right"), orientation)], fallback_anchor), na.rm = TRUE),
    left = max(c(quad_anchor[intersect(c("top_left", "bottom_left"), orientation)], fallback_anchor), na.rm = TRUE),
    top = max(c(quad_anchor[intersect(c("top_right", "top_left"), orientation)], fallback_anchor), na.rm = TRUE),
    bottom = max(c(quad_anchor[intersect(c("bottom_right", "bottom_left"), orientation)], fallback_anchor), na.rm = TRUE)
  )

  xy_targets <- purrr::imap_dfr(
    env_cor_self_list[orientation],
    .make_targets,
    k_gap = k_gap,
    length_dist = length_dist,
    side_anchor = side_anchor,
    heatmap_step = heatmap_step
  )

  cor_spec_env_location <- cor_spec_env_list_out %>%
    dplyr::mutate(ID = as.character(ID), Type = as.character(Type)) %>%
    dplyr::left_join(module_centroids, by = "ID") %>%
    dplyr::left_join(xy_targets, by = c("Type" = "ID")) %>%
    dplyr::mutate(line_type = dplyr::if_else(.data$Pvalue <= 0.05, "solid", "dashed"))

  link_df <- cor_spec_env_location %>%
    dplyr::mutate(
      # -log10(p): smaller p -> larger value -> thicker line
      # p=0.05->1.3, p=0.01->2, p=0.001->3
      sig_strength = -log10(.data$Pvalue)
    )
  if (isTRUE(drop_nonsig)) {
    link_df <- link_df %>% dplyr::filter(.data$Pvalue <= 0.05)
  }

  .offset_env <- function(df, ori, k_gap, length_dist, side_anchor, heatmap_step,
                          HeatmapLabelOrient = 0, y_top_all = NULL,
                          y_bottom_all = NULL) {
    df <- df %>% dplyr::mutate(ID = as.character(ID), Type = as.character(Type))
    x_anchor <- if (ori %in% c("top_right", "bottom_right")) side_anchor[["right"]] else side_anchor[["left"]]
    y_anchor <- if (ori %in% c("top_right", "top_left")) side_anchor[["top"]] else side_anchor[["bottom"]]

    x_tile <- if (ori %in% c("top_right", "bottom_right")) {
      x_anchor + heatmap_step * (k_gap[[ori]] + df$ID2 - 1)
    } else {
      -x_anchor - heatmap_step * (length_dist - df$ID2)
    }
    y_tile <- if (ori %in% c("top_right", "top_left")) {
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
    diag <- diag_df %>% dplyr::transmute(ID, x_diag, y_diag, orientation = ori)
    if (HeatmapLabelOrient == 0 || is.null(y_top_all) || is.null(y_bottom_all)) {
      y_id_lab <- if (ori %in% c("top_right", "top_left")) {
        y_anchor + heatmap_step * length_dist
      } else {
        -y_anchor - heatmap_step * length_dist
      }
    } else {
      y_id_lab <- if (ori %in% c("top_right", "top_left")) {
        y_top_all + heatmap_step
      } else {
        y_bottom_all - heatmap_step
      }
    }
    x_type_lab <- if (ori %in% c("top_right", "bottom_right")) {
      x_anchor + heatmap_step * length_dist
    } else {
      -x_anchor - heatmap_step * length_dist
    }
    hjust_type <- if (ori %in% c("top_right", "bottom_right")) "left" else "right"
    id_lab <- df %>%
      dplyr::distinct(ID, .keep_all = TRUE) %>%
      dplyr::transmute(
        ID,
        x_id = if (ori %in% c("top_right", "bottom_right")) {
          x_anchor + heatmap_step * (k_gap[[ori]] + ID2 - 1)
        } else {
          -x_anchor - heatmap_step * (length_dist - ID2)
        },
        y_id = y_id_lab,
        orientation = ori
      )
    type_lab <- df %>%
      dplyr::distinct(Type, .keep_all = TRUE) %>%
      dplyr::transmute(
        Type,
        x_type = x_type_lab,
        y_type = if (ori %in% c("top_right", "top_left")) {
          y_anchor + heatmap_step * (k_gap[[ori]] + Type2 - 1)
        } else {
          -y_anchor - heatmap_step * (length_dist - Type2)
        },
        hjust_type = hjust_type,
        orientation = ori
      )
    list(tile = tile, diag = diag, id_lab = id_lab, type_lab = type_lab)
  }

  .compute_y_range <- function(df, ori, k_gap, length_dist, side_anchor, heatmap_step) {
    df <- df %>% dplyr::mutate(ID = as.character(ID), Type = as.character(Type))
    y_anchor <- if (ori %in% c("top_right", "top_left")) side_anchor[["top"]] else side_anchor[["bottom"]]
    y_tile <- if (ori %in% c("top_right", "top_left")) {
      y_anchor + heatmap_step * (k_gap[[ori]] + df$Type2 - 1)
    } else {
      -y_anchor - heatmap_step * (length_dist - df$Type2)
    }
    data.frame(orientation = ori, ymin = min(y_tile, na.rm = TRUE), ymax = max(y_tile, na.rm = TRUE))
  }

  y_ranges <- purrr::imap_dfr(
    env_cor_self_list[orientation],
    ~ .compute_y_range(.x, .y, k_gap, length_dist, side_anchor, heatmap_step)
  )
  y_top_all <- max(y_ranges$ymax[y_ranges$orientation %in% c("top_right", "top_left")], na.rm = TRUE)
  y_bottom_all <- min(y_ranges$ymin[y_ranges$orientation %in% c("bottom_right", "bottom_left")], na.rm = TRUE)

  packs <- purrr::imap(
    env_cor_self_list[orientation],
    ~ .offset_env(.x, .y, k_gap, length_dist, side_anchor, heatmap_step,
                  HeatmapLabelOrient = HeatmapLabelOrient,
                  y_top_all = y_top_all,
                  y_bottom_all = y_bottom_all)
  )

  diag_all <- purrr::map_dfr(packs, "diag")

  pal_default_low <- c("#4d9221", "#8073ac", "#4393c3", "#66bd63")
  pal_default_high <- c("#c51b7d", "#e08214", "#d6604d", "#f46d43")
  n_quad <- length(packs)
  if (!is.null(HeatmapColorBar) && is.list(HeatmapColorBar) &&
      length(HeatmapColorBar) == 2 && all(c("low", "high") %in% names(HeatmapColorBar))) {
    pal_low <- rep(HeatmapColorBar$low, length.out = n_quad)
    pal_high <- rep(HeatmapColorBar$high, length.out = n_quad)
  } else {
    pal_low <- rep(pal_default_low, length.out = n_quad)
    pal_high <- rep(pal_default_high, length.out = n_quad)
  }

  .add_quadrant_layers <- function(p, pack, idx, low_pal, high_pal) {
    tile <- pack$tile
    diag <- pack$diag
    id_lab <- pack$id_lab
    type_lab <- pack$type_lab
    ori_lab <- unique(id_lab$orientation)[1]
    hjust_id <- if (ori_lab %in% c("top_right", "top_left")) 0 else 1
    vjust_id <- if (ori_lab %in% c("top_right", "top_left")) -0.25 else 0.25
    p +
      ggplot2::geom_tile(data = tile, ggplot2::aes(x = x_tile, y = y_tile, fill = Correlation),
                         colour = HeatmapTileColor, linewidth = HeatmapTileSize) +
      ggplot2::geom_text(data = tile, ggplot2::aes(x = x_tile, y = y_tile, label = p_signif), size = HeatmapSigSize) +
      ggplot2::geom_text(data = id_lab, ggplot2::aes(x = x_id, y = y_id, label = ID),
                        size = HeatmapLabelSize, vjust = vjust_id, hjust = hjust_id, angle = HeatmapLabelOrient) +
      ggplot2::geom_text(data = type_lab, ggplot2::aes(x = x_type, y = y_type, label = Type),
                        hjust = type_lab$hjust_type[1], size = HeatmapLabelSize) +
      ggplot2::geom_point(data = diag, ggplot2::aes(x = x_diag, y = y_diag),
                         shape = 21, fill = HeatmapPointFill, size = HeatmapPointSize) +
      ggplot2::scale_fill_gradient2(
        low = low_pal[idx], mid = "#ffffff", high = high_pal[idx],
        midpoint = 0, name = paste0("Env ", idx),
        guide = ggplot2::guide_colorbar(order = idx, direction = "horizontal")
      )
  }

  p_heatmaps <- ggplot2::ggplot()
  for (i in seq_along(packs)) {
    if (i > 1) p_heatmaps <- p_heatmaps + ggnewscale::new_scale_fill()
    p_heatmaps <- .add_quadrant_layers(p_heatmaps, packs[[i]], idx = i, pal_low, pal_high)
  }

  node_plot_df <- ggplot_data[[1]]
  edge_plot_df <- ggplot_data[[2]]
  fill_scale_net <- if (!is.null(ggnetview_args$fill)) {
    ggplot2::scale_fill_manual(values = ggnetview_args$fill, guide = "none")
  } else {
    scale_fill_ggnetview(unique(node_plot_df$Modularity), guide = "none")
  }

  add_outer <- isTRUE(ggnetview_args$add_outer)
  add_group_outer <- isTRUE(ggnetview_args$add_group_outer)
  label_arg <- if ("label" %in% names(ggnetview_args)) ggnetview_args$label else TRUE
  show_module_label <- isTRUE(label_arg) || (is.character(label_arg) && length(label_arg) == 1 && nchar(trimws(label_arg)) > 0)
  module_label_prefix <- if (is.character(label_arg) && length(label_arg) == 1) trimws(label_arg) else "Modularity"
  if (identical(module_label_prefix, "")) module_label_prefix <- "Modularity"
  labelsize <- if (!is.null(ggnetview_args$labelsize)) as.numeric(ggnetview_args$labelsize) else 10
  labelsegmentsize <- if (!is.null(ggnetview_args$labelsegmentsize)) as.numeric(ggnetview_args$labelsegmentsize) else 1
  labelsegmentalpha <- if (!is.null(ggnetview_args$labelsegmentalpha)) as.numeric(ggnetview_args$labelsegmentalpha) else 1
  mod_col_lab <- if ("modularity3" %in% colnames(graph_ly_scaled)) "modularity3" else "Modularity"
  lab_df <- NULL
  if (show_module_label) {
    xr_lab <- range(graph_ly_scaled$x, na.rm = TRUE)
    yr_lab <- range(graph_ly_scaled$y, na.rm = TRUE)
    x_mid_lab <- stats::median(graph_ly_scaled$x)
    dx_lab <- diff(xr_lab) * 0.12
    lab_df <- graph_ly_scaled %>%
      dplyr::distinct(.data[[mod_col_lab]], .keep_all = TRUE) %>%
      dplyr::filter(.data[[mod_col_lab]] != "Others") %>%
      dplyr::mutate(side = ifelse(x < x_mid_lab, "left", "right")) %>%
      dplyr::group_by(side) %>%
      dplyr::arrange(y, .by_group = TRUE) %>%
      dplyr::mutate(
        y_rank = dplyr::row_number(),
        y_target = scales::rescale(y_rank, to = yr_lab),
        x_anchor = dplyr::if_else(side == "left", xr_lab[1] - dx_lab, xr_lab[2] + dx_lab),
        nudge_x = x_anchor - x,
        nudge_y = y_target - y,
        hjust = dplyr::if_else(side == "left", 1, 0),
        .label_text = paste0(module_label_prefix, " ", .data[[mod_col_lab]])
      ) %>%
      dplyr::ungroup()
  }
  module_label_fun <- function(x) paste0(module_label_prefix, " ", x)
  q_outer <- if (!is.null(ggnetview_args$q_outer)) as.numeric(ggnetview_args$q_outer) else 0.88
  expand_outer <- if (!is.null(ggnetview_args$expand_outer)) as.numeric(ggnetview_args$expand_outer) else 1.02
  outerwidth <- if (!is.null(ggnetview_args$outerwidth)) as.numeric(ggnetview_args$outerwidth) else 1.25
  outerlinetype <- if (!is.null(ggnetview_args$outerlinetype)) ggnetview_args$outerlinetype else 2
  outeralpha <- if (!is.null(ggnetview_args$outeralpha)) as.numeric(ggnetview_args$outeralpha) else 0.5

  p0 <- p_heatmaps +
    ggnewscale::new_scale_fill() +
    ggnewscale::new_scale_color()
  p0 <- p0 +
    ggplot2::geom_segment(
      data = edge_plot_df,
      ggplot2::aes(x = from_x, y = from_y, xend = to_x, yend = to_y),
      alpha = 0.25,
      colour = "grey70"
    )

  if (add_group_outer && nrow(node_plot_df) > 0) {
    group_circle_df <- node_plot_df %>% dplyr::mutate(.group_outer = 1L)
    circle_n_grp <- max(40, min(300, as.integer(round(8 * sqrt(nrow(group_circle_df))))))
    fill_grp <- if (!is.null(ggnetview_args$add_group_outer_fill) && length(ggnetview_args$add_group_outer_fill) > 0)
      ggnetview_args$add_group_outer_fill[1L] else NA
    alpha_grp <- if (is.na(fill_grp)) 1 else
      if (!is.null(ggnetview_args$add_group_outer_fill_alpha)) ggnetview_args$add_group_outer_fill_alpha else 0.2
    p0 <- p0 +
      ggforce::geom_mark_circle(
        data = group_circle_df,
        mapping = ggplot2::aes(x = x, y = y, group = .group_outer),
        fill = fill_grp,
        alpha = alpha_grp,
        color = if (!is.null(ggnetview_args$add_group_outer_color)) ggnetview_args$add_group_outer_color else "grey50",
        linetype = if (!is.null(ggnetview_args$add_group_outer_linetype)) ggnetview_args$add_group_outer_linetype else 1,
        linewidth = if (!is.null(ggnetview_args$add_group_outer_linewidth)) ggnetview_args$add_group_outer_linewidth else 0.5,
        n = circle_n_grp,
        expand = grid::unit(if (!is.null(ggnetview_args$add_group_outer_expand)) ggnetview_args$add_group_outer_expand else 2, "mm")
      )
  }

  if (add_outer) {
    maskTable <- generateMask_ggnetview(
      dims = graph_ly_scaled[, c("x", "y")],
      clusters = graph_ly_scaled$Modularity,
      q = q_outer,
      expand = expand_outer
    )
    if (nrow(maskTable) > 0L) {
      maskTable <- maskTable %>%
        dplyr::mutate(cluster = factor(cluster, levels = levels(graph_ly_scaled$Modularity), ordered = TRUE))
      mask_classes <- levels(maskTable$cluster)
      fill_scale_mask <- if (!is.null(ggnetview_args$fill)) {
        ggplot2::scale_fill_manual(values = ggnetview_args$fill, guide = "none")
      } else {
        scale_fill_ggnetview(mask_classes, guide = "none")
      }
      color_scale_mask <- if (!is.null(ggnetview_args$color)) {
        ggplot2::scale_color_manual(values = ggnetview_args$color, guide = "none")
      } else {
        scale_color_ggnetview(mask_classes, guide = "none")
      }
      p0 <- p0 +
        ggnewscale::new_scale_fill() +
        ggnewscale::new_scale_color() +
        ggplot2::geom_polygon(
          data = maskTable %>% dplyr::filter(cluster != "Others"),
          mapping = ggplot2::aes(x = x, y = y, group = cluster, fill = cluster, color = cluster),
          linewidth = outerwidth,
          linetype = outerlinetype,
          alpha = outeralpha,
          show.legend = FALSE
        ) +
        fill_scale_mask +
        color_scale_mask +
        ggnewscale::new_scale_fill() +
        ggnewscale::new_scale_color()
    }
  }

  p0 <- p0 +
    ggplot2::geom_point(
      data = node_plot_df,
      ggplot2::aes(x = x, y = y, fill = Modularity),
      shape = 21,
      size = 3,
      alpha = 0.9
    ) +
    fill_scale_net

  p1 <- p0 +
    ggnewscale::new_scale_color() +
    ggplot2::geom_segment(
      data = link_df,
      ggplot2::aes(x = x, y = y, xend = x_to, yend = y_to,
                   color = Correlation, linetype = line_type, linewidth = sig_strength),
      alpha = SigLineAlpha
    ) +
    ggplot2::scale_color_gradient(
      low = SigLineColor[1], high = SigLineColor[2],
      guide = ggplot2::guide_colorbar(direction = "horizontal")
    ) +
    ggplot2::scale_linewidth_continuous(
      range = SigLineWidth, name = "-log10(Pvalue)",
      guide = ggplot2::guide_legend(nrow = 1)  # horizontal row at top
    ) +
    ggplot2::scale_linetype_identity() +
    ggplot2::geom_point(data = diag_all, ggplot2::aes(x = x_diag, y = y_diag),
                        shape = 21, fill = HeatmapPointFill, size = HeatmapPointSize) +
    {       if (show_module_label && !is.null(lab_df) && nrow(lab_df) > 0) {
        lab_classes <- sort(unique(lab_df$Modularity))
        color_scale_lab <- if (!is.null(ggnetview_args$color)) ggplot2::scale_color_manual(values = ggnetview_args$color, labels = module_label_fun) else scale_color_ggnetview(lab_classes, labels = module_label_fun)
        list(
          ggnewscale::new_scale_color(),
          ggrepel::geom_text_repel(
            data = lab_df,
            mapping = ggplot2::aes(x = x, y = y, label = .label_text, color = Modularity),
            size = labelsize,
            nudge_x = lab_df$nudge_x,
            nudge_y = lab_df$nudge_y,
            hjust = lab_df$hjust,
            min.segment.length = 0,
            segment.size = labelsegmentsize,
            segment.alpha = labelsegmentalpha,
            max.overlaps = Inf,
            box.padding = 0.15,
            point.padding = 0.15,
            force = 0.05,
            show.legend = FALSE
          ),
          color_scale_lab
        )
      } else list() } +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::theme_void() +
    ggplot2::theme(
      plot.margin = ggplot2::margin(10, 10, 10, 10),
      aspect.ratio = 1,
      legend.position = "top",
      legend.box = "horizontal",
      legend.box.just = "left"
    )

  p2 <- p0 +
    ggnewscale::new_scale_color() +
    ggplot2::geom_curve(
      data = link_df,
      ggplot2::aes(x = x, y = y, xend = x_to, yend = y_to,
                   color = Correlation, linetype = line_type, linewidth = sig_strength),
      alpha = SigLineAlpha,
      curvature = 0.25
    ) +
    ggplot2::scale_color_gradient(
      low = SigLineColor[1], high = SigLineColor[2],
      guide = ggplot2::guide_colorbar(direction = "horizontal")
    ) +
    ggplot2::scale_linewidth_continuous(
      range = SigLineWidth, name = "-log10(Pvalue)",
      guide = ggplot2::guide_legend(nrow = 1)  # horizontal row at top
    ) +
    ggplot2::scale_linetype_identity() +
    ggplot2::geom_point(data = diag_all, ggplot2::aes(x = x_diag, y = y_diag),
                        shape = 21, fill = HeatmapPointFill, size = HeatmapPointSize) +
    {       if (show_module_label && !is.null(lab_df) && nrow(lab_df) > 0) {
        lab_classes <- sort(unique(lab_df$Modularity))
        color_scale_lab <- if (!is.null(ggnetview_args$color)) ggplot2::scale_color_manual(values = ggnetview_args$color, labels = module_label_fun) else scale_color_ggnetview(lab_classes, labels = module_label_fun)
        list(
          ggnewscale::new_scale_color(),
          ggrepel::geom_text_repel(
            data = lab_df,
            mapping = ggplot2::aes(x = x, y = y, label = .label_text, color = Modularity),
            size = labelsize,
            nudge_x = lab_df$nudge_x,
            nudge_y = lab_df$nudge_y,
            hjust = lab_df$hjust,
            min.segment.length = 0,
            segment.size = labelsegmentsize,
            segment.alpha = labelsegmentalpha,
            max.overlaps = Inf,
            box.padding = 0.15,
            point.padding = 0.15,
            force = 0.05,
            show.legend = FALSE
          ),
          color_scale_lab
        )
      } else list() } +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::theme_void() +
    ggplot2::theme(
      plot.margin = ggplot2::margin(10, 10, 10, 10),
      aspect.ratio = 1,
      legend.position = "top",
      legend.box = "horizontal",
      legend.box.just = "left"
    )

  list(p1, p2, cor_spec_env_list_out)
}
