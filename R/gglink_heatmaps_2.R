#' Visualize multi-orientation environmental-species correlation heatmaps (adaptive sizing)
#'
#' An improved version of \code{\link{gglink_heatmaps}} that sizes each heatmap
#' quadrant independently according to its own number of variables.
#' All tiles share the same size; larger env blocks simply extend further.
#' The central species network is always kept at the centre of the canvas.
#'
#' @inheritParams gglink_heatmaps
#'
#' @returns A list of length 3:
#' - [[1]]: ggplot object with straight link segments.
#' - [[2]]: ggplot object with curved link segments.
#' - [[3]]: data.frame of full species-environment correlation statistics
#'          (unfiltered, not affected by \code{drop_nonsig}), with columns
#'          \code{ID}, \code{Type}, \code{Correlation}, \code{Pvalue}, \code{spec_block},
#'          \code{env_block}, and \code{method} (e.g. \code{"correlation"} or \code{"mantel"}).
#' @export
#'
#' @examples
#' \dontrun{
#' # Adaptive-sized variant of `gglink_heatmaps()`.
#' p <- gglink_heatmaps_2(
#'   env  = env,
#'   spec = spec
#' )
#' }
gglink_heatmaps_2 <- function(
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

  ####----split data----####
  env_list <- purrr::map(env_select, ~ env[, .x, drop = FALSE])
  spec_list <- purrr::map(spec_select, ~ spec[, .x, drop = FALSE])

  k_vec  <- purrr::map_int(env_list, function(x) ncol(x))
  k_per_ori <- stats::setNames(k_vec, orientation)

  # Two-tier alignment:
  #  - Small quadrants align their outer edges to each other (k_gap > 0).
  #  - Large quadrant(s) start at the anchor and extend freely (k_gap = 0).
  # length_dist = max among the non-maximum k values (reference for small ones).
  # If all k are equal, length_dist = that common value (standard behaviour).
  k_max <- max(k_vec)
  k_small <- k_vec[k_vec < k_max]
  length_dist <- if (length(k_small) > 0L) max(k_small) else k_max
  k_gap <- stats::setNames(pmax(length_dist - k_vec, 0L), orientation)
  # Per-quadrant effective span used for the negative-direction formula.
  # For small quads: effective_len = length_dist (aligned outer edge).
  # For large quads: effective_len = k_i     (extends further out).
  effective_len <- stats::setNames(pmax(k_vec, length_dist), orientation)

  distance <- as.numeric(distance)
  if (is.na(distance) || length(distance) != 1) {
    stop("`distance` must be a single numeric value.")
  }
  if (distance < 0) {
    stop("`distance` must be non-negative.")
  }

  ####----env self-correlation per quadrant----####
  env_cor_self_list <- list()

  for (i in seq_along(orientation)) {
    if (orientation[i] == "top_right") {
      cor_out_self <- psych::corr.test(env_list[[i]], use = cor.use, method = cor.method)

      cor_self_r <- cor_out_self$r %>% as.data.frame()
      cor_self_r[upper.tri(cor_self_r)] <- NA

      cor_self_p <- cor_out_self$p %>% as.data.frame()
      cor_self_p[upper.tri(cor_self_p)] <- NA

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
          Pvalue <= 0.01 & Pvalue >= 0.001 ~ "**",
          Pvalue < 0.001 ~ "***"
        ))

      cor_self_r_p <- cbind(cor_self_r %>% dplyr::select(1,2,4,5,3),
                            cor_self_p %>% dplyr::select(3,6))

      env_cor_self_list[[i]] <- cor_self_r_p
    }

    if (orientation[i] == "bottom_right") {
      cor_out_self <- psych::corr.test(env_list[[i]], use = cor.use, method = cor.method)

      cor_self_r <- cor_out_self$r %>% as.data.frame()
      cor_self_r[upper.tri(cor_self_r)] <- NA

      cor_self_p <- cor_out_self$p %>% as.data.frame()
      cor_self_p[upper.tri(cor_self_p)] <- NA

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
          Pvalue <= 0.01 & Pvalue >= 0.001 ~ "**",
          Pvalue < 0.001 ~ "***"
        ))

      cor_self_r_p <- cbind(cor_self_r %>% dplyr::select(1,2,4,5,3),
                            cor_self_p %>% dplyr::select(3,6)
      )

      env_cor_self_list[[i]] <- cor_self_r_p
    }

    if (orientation[i] == "top_left") {
      cor_out_self <- psych::corr.test(env_list[[i]], use = cor.use, method = cor.method)

      cor_self_r <- cor_out_self$r %>% as.data.frame()
      cor_self_r[lower.tri(cor_self_r)] <- NA

      cor_self_p <- cor_out_self$p %>% as.data.frame()
      cor_self_p[lower.tri(cor_self_p)] <- NA

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
          Pvalue <= 0.01 & Pvalue >= 0.001 ~ "**",
          Pvalue < 0.001 ~ "***"
        ))

      cor_self_r_p <- cbind(cor_self_r %>% dplyr::select(1,2,4,5,3),
                            cor_self_p %>% dplyr::select(3,6)
      )

      env_cor_self_list[[i]] <- cor_self_r_p
    }

    if (orientation[i] == "bottom_left") {
      cor_out_self <- psych::corr.test(env_list[[i]], use = cor.use, method = cor.method)

      cor_self_r <- cor_out_self$r %>% as.data.frame()
      cor_self_r[lower.tri(cor_self_r)] <- NA

      cor_self_p <- cor_out_self$p %>% as.data.frame()
      cor_self_p[lower.tri(cor_self_p)] <- NA

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
          Pvalue <= 0.01 & Pvalue >= 0.001 ~ "**",
          Pvalue < 0.001 ~ "***"
        ))

      cor_self_r_p <- cbind(cor_self_r %>% dplyr::select(1,2,4,5,3),
                            cor_self_p %>% dplyr::select(3,6)
      )

      env_cor_self_list[[i]] <- cor_self_r_p
    }
  }

  names(env_cor_self_list) <- orientation

  ####----spec-env relationship----####

  spec_block_names <- names(spec_list)
  if (is.null(spec_block_names)) spec_block_names <- paste0("Spec", seq_along(spec_list))

  env_block_names <- names(env_list)
  if (is.null(env_block_names)) env_block_names <- paste0("Env", seq_along(env_list))

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

  if (!isTRUE(comparisons)) {
    pairs_to_compute <- matrix(character(0), nrow = 2, ncol = 0)
  } else if (!is.null(comparisons_groups) && length(comparisons_groups) > 0) {
    pairs_to_compute <- do.call(cbind, lapply(comparisons_groups, function(x) matrix(x, nrow = 2)))
    pairs_to_compute <- unique(as.data.frame(t(pairs_to_compute)))
    pairs_to_compute <- as.matrix(pairs_to_compute)
    if (nrow(pairs_to_compute) > 0) {
      pairs_to_compute <- t(pairs_to_compute)
    } else {
      pairs_to_compute <- matrix(character(0), nrow = 2, ncol = 0)
    }
  } else {
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
            Pvalue <= 0.01 & Pvalue >= 0.001 ~ "**",
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
            Pvalue <= 0.01 & Pvalue >= 0.001 ~ "**",
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
          Pvalue <= 0.01 & Pvalue >= 0.001 ~ "**", Pvalue < 0.001 ~ "***"
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
        scale_j <- n_nodes[j] / n_min
        ly_df <- ly_df %>%
          dplyr::mutate(x = x * scale_j + ax, y = y * scale_j + ay)
      }
      cor_spec_env_parts[[j]] <- ly_df %>% dplyr::select(ID, x, y)
    }
    cor_spec_env <- do.call(rbind, cor_spec_env_parts)
  }

  ####----per-quadrant layout (uniform tile, two-tier alignment)----####

  # Fixed tile size for all quadrants.
  # Small quadrants: k_gap > 0, outer edges aligned among themselves.
  # Large quadrant:  k_gap = 0, extends freely beyond the small alignment line.
  # effective_len drives the negative-direction formula so large quads don't
  # intrude into the network area.
  r_ref <- 6
  heatmap_step <- 1 * HeatmapScale

  .diag_xy_2 <- function(id_idx, type_idx, ori, k_gap, effective_len,
                          side_anchor, heatmap_step) {
    x_anchor <- if (ori %in% c("top_right", "bottom_right")) side_anchor[["right"]] else side_anchor[["left"]]
    y_anchor <- if (ori %in% c("top_right", "top_left")) side_anchor[["top"]] else side_anchor[["bottom"]]

    x_out <- if (ori %in% c("top_right", "bottom_right")) {
      x_anchor + heatmap_step * (k_gap[[ori]] + id_idx - 1)
    } else {
      -x_anchor - heatmap_step * (effective_len[[ori]] - id_idx)
    }

    y_out <- if (ori %in% c("top_right", "top_left")) {
      y_anchor + heatmap_step * (k_gap[[ori]] + type_idx - 2)
    } else {
      -y_anchor - heatmap_step * (effective_len[[ori]] - type_idx - 1)
    }

    list(x = x_out, y = y_out)
  }

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

  .make_targets_2 <- function(df, ori, k_gap, effective_len,
                               side_anchor, heatmap_step){
    df_diag <- df %>%
      dplyr::mutate(ID = as.character(ID), Type = as.character(Type)) %>%
      dplyr::filter(ID == Type)

    xy_diag <- .diag_xy_2(
      id_idx = df_diag$ID2,
      type_idx = df_diag$Type2,
      ori = ori,
      k_gap = k_gap,
      effective_len = effective_len,
      side_anchor = side_anchor,
      heatmap_step = heatmap_step
    )

    df_diag %>%
      dplyr::transmute(ID, x_to = xy_diag$x, y_to = xy_diag$y)
  }

  xy_targets <- purrr::imap_dfr(
    .x = env_cor_self_list[orientation],
    .f = .make_targets_2,
    k_gap = k_gap,
    effective_len = effective_len,
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

  link_df <- cor_spec_env_location
  if (isTRUE(drop_nonsig)) {
    link_df <- link_df %>% dplyr::filter(.data$Pvalue <= 0.05)
  }
  link_df <- link_df %>%
    dplyr::mutate(
      link_width_value = -log10(pmax(.data$Pvalue, .Machine$double.xmin))
    )

  .offset_env_2 <- function(df, ori, k_per_ori, k_gap, effective_len,
                             side_anchor, heatmap_step,
                             HeatmapLabelOrient = 0,
                             y_top_all = NULL, y_bottom_all = NULL){
    stopifnot(ori %in% c("top_right","bottom_right","top_left","bottom_left"))
    k_i   <- k_per_ori[[ori]]
    gap_i <- k_gap[[ori]]
    eff_i <- effective_len[[ori]]
    df <- df %>% dplyr::mutate(ID = as.character(ID), Type = as.character(Type))
    x_anchor <- if (ori %in% c("top_right", "bottom_right")) side_anchor[["right"]] else side_anchor[["left"]]
    y_anchor <- if (ori %in% c("top_right", "top_left")) side_anchor[["top"]] else side_anchor[["bottom"]]

    # Positive direction: gap shifts small quadrants to their alignment boundary.
    # Negative direction: effective_len sets the outer limit correctly for both
    # small (effective_len = length_dist) and large (effective_len = k_i) quads.
    x_tile <- if (ori %in% c("top_right","bottom_right")) {
      x_anchor + heatmap_step * (gap_i + df$ID2 - 1)
    } else {
      -x_anchor - heatmap_step * (eff_i - df$ID2)
    }
    y_tile <- if (ori %in% c("top_right","top_left")) {
      y_anchor + heatmap_step * (gap_i + df$Type2 - 1)
    } else {
      -y_anchor - heatmap_step * (eff_i - df$Type2)
    }

    tile <- df %>% dplyr::mutate(x_tile = x_tile, y_tile = y_tile, orientation = ori)

    diag_df <- df %>% dplyr::filter(ID == Type)
    diag_xy <- .diag_xy_2(
      id_idx = diag_df$ID2,
      type_idx = diag_df$Type2,
      ori = ori,
      k_gap = k_gap,
      effective_len = effective_len,
      side_anchor = side_anchor,
      heatmap_step = heatmap_step
    )
    x_diag <- diag_xy$x
    y_diag <- diag_xy$y
    diag    <- diag_df %>% dplyr::transmute(ID, x_diag, y_diag, orientation = ori)

    if (HeatmapLabelOrient == 0 || is.null(y_top_all) || is.null(y_bottom_all)) {
      y_id_lab <- if (ori %in% c("top_right","top_left")) {
        y_anchor + heatmap_step * (gap_i + k_i)
      } else {
        -y_anchor - heatmap_step * (gap_i + k_i)
      }
    } else {
      if (ori %in% c("top_right","top_left")) {
        y_id_lab <- y_top_all + heatmap_step
      } else {
        y_id_lab <- y_bottom_all - heatmap_step
      }
    }
    x_type_lab <- if (ori %in% c("top_right","bottom_right")) {
      x_anchor + heatmap_step * (gap_i + k_i)
    } else {
      -x_anchor - heatmap_step * (gap_i + k_i)
    }
    hjust_type <- if (ori %in% c("top_right","bottom_right")) "left" else "right"

    id_lab <- df %>%
      dplyr::distinct(ID, .keep_all = TRUE) %>%
      dplyr::transmute(ID,
                       x_id = if (ori %in% c("top_right","bottom_right")) {
                         x_anchor + heatmap_step * (gap_i + ID2 - 1)
                       } else {
                         -x_anchor - heatmap_step * (eff_i - ID2)
                       },
                       y_id = y_id_lab, orientation = ori)

    type_lab <- df %>%
      dplyr::distinct(Type, .keep_all = TRUE) %>%
      dplyr::transmute(Type,
                       x_type = x_type_lab,
                       y_type = if (ori %in% c("top_right","top_left")) {
                         y_anchor + heatmap_step * (gap_i + Type2 - 1)
                       } else {
                         -y_anchor - heatmap_step * (eff_i - Type2)
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

  .compute_y_range_2 <- function(df, ori, k_per_ori, k_gap, effective_len,
                                  side_anchor, heatmap_step){
    k_i   <- k_per_ori[[ori]]
    gap_i <- k_gap[[ori]]
    eff_i <- effective_len[[ori]]
    df <- df %>% dplyr::mutate(ID = as.character(ID), Type = as.character(Type))
    y_anchor <- if (ori %in% c("top_right", "top_left")) side_anchor[["top"]] else side_anchor[["bottom"]]
    y_tile <- if (ori %in% c("top_right","top_left")) {
      y_anchor + heatmap_step * (gap_i + df$Type2 - 1)
    } else {
      -y_anchor - heatmap_step * (eff_i - df$Type2)
    }
    data.frame(
      orientation = ori,
      ymin = min(y_tile, na.rm = TRUE),
      ymax = max(y_tile, na.rm = TRUE)
    )
  }

  y_ranges <- purrr::imap_dfr(
    env_cor_self_list[orientation],
    ~ .compute_y_range_2(.x, .y, k_per_ori, k_gap, effective_len,
                         side_anchor, heatmap_step)
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
    ~ .offset_env_2(.x, .y, k_per_ori, k_gap, effective_len,
                    side_anchor, heatmap_step,
                    HeatmapLabelOrient = HeatmapLabelOrient,
                    y_top_all = y_top_all,
                    y_bottom_all = y_bottom_all)
  )

  diag_all <- purrr::map_dfr(packs, "diag")

  # Compute symmetric plot limits so the central network stays centred
  all_x <- c(
    unlist(lapply(packs, function(p) c(p$tile$x_tile, p$id_lab$x_id, p$type_lab$x_type))),
    cor_spec_env$x
  )
  all_y <- c(
    unlist(lapply(packs, function(p) c(p$tile$y_tile, p$id_lab$y_id, p$type_lab$y_type))),
    cor_spec_env$y
  )
  half_range <- max(abs(c(all_x, all_y)), na.rm = TRUE) * 1.08

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

  # Shared coord with forced symmetric centering
  centered_coord <- ggplot2::coord_cartesian(
    xlim = c(-half_range, half_range),
    ylim = c(-half_range, half_range),
    clip = "off"
  )

  p1 <- p0 +
    ggnewscale::new_scale_color() +
    ggplot2::geom_segment(
      data = link_df,
      ggplot2::aes(x = x, y = y, xend = x_to, yend = y_to,
          color = Correlation,
          linetype = line_type,
          linewidth = link_width_value),
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
    centered_coord +
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
                            linewidth = link_width_value),
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
    centered_coord +
    ggplot2::theme_void() +
    ggplot2::theme(
      plot.margin = ggplot2::margin(1, 1, 1, 1, "cm"),
      aspect.ratio = 1,
      legend.position = "top"
    )

  return(list(p1, p2, cor_spec_env_list_out))

}
