#' Visualize multi-orientation environmental–species correlation heatmaps
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
#' @param spec_layout Character
#' Character string specifying the spatial arrangement of species nodes. Options include `"ringle"` (radial) and other supported layouts.
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
#' @param shape Intrger
#' Integer or numeric specifying the shape of species nodes in the plot (passed to `geom_point()`).
#' @param distance Numeric
#' the offset distance between central nodes and the environmental heatmaps.
#' @param orientation Character
#' Character vector defining which heatmap quadrants to display. Can include any combination of `"top_right"`, `"bottom_right"`, `"top_left"`, and `"bottom_left"`.
#' @param r Numeric
#' radius of the central species layout (in plot units).
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
#'  c("#2166ac", "#b2182b"),  # 第1个象限 low/high
#'  c("#1b7837", "#762a83"),  # 第2个
#'  c("#4393c3", "#d6604d"),  # 第3个
#'  c("#92c5de", "#f4a582")   # 第4个
#'  )`
#' @param HeatmapLabelOrient Numeric (default = 0)
#' Rotation angle (in degrees) for heatmap axis labels (ID/Type).
#' Use this to avoid overlap of the top/bottom labels; e.g. 45 or 90.
#'
#' @param SigLineWidth Numeric vector of length 2 (default = c(0.5, 2))
#' Controls the minimum and maximum line width for species–environment links,
#' scaled by significance (p-value). Smaller p leads to thicker lines.
#' @param SigLineColor Character vector (length 2, default = c("#fdbb84", "#d7301f"))
#' Colors used for the species–environment link color gradient, corresponding to
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
#'
#' @param fontsize Numeric (default = 5)
#' (Deprecated) Use `HeatmapLabelSize` instead.
#'
#' @returns A list of length 3:
#' - [[1]]: ggplot object with straight link segments.
#' - [[2]]: ggplot object with curved link segments.
#' - [[3]]: data.frame of full species–environment correlation statistics
#'          (unfiltered, not affected by `drop_nonsig`).
#' @export
#'
#' @examples NULL
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
    fontsize = 5,
    orientation = c("top_right", "bottom_right", "top_left","bottom_left"),
    r = 6
){

  # argument test
  relation_method <- match.arg(relation_method)
  cor.method      <- match.arg(cor.method)
  cor.use         <- match.arg(cor.use)
  mantel.method   <- match.arg(mantel.method)
  mantel.method2  <- match.arg(mantel.method2)
  mantel.alternative <- match.arg(mantel.alternative)
  orientation     <- match.arg(orientation, several.ok = TRUE)



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
  # if env_select = NULL & spec_select = NULL
  # 说明是最简单的方式 1个点，1个矩阵

  # 如果env_select 含有多个，则出现不同位置的热图

  # 如果 spec_select 出现多个，则出现不同位置的点，这里建议使用环状布局

  ####----split data----####
  # 1 个点， 然后4个环境数据
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

  # 这里需要统计一下
  k_vec  <- purrr::map_int(env_list, ncol)
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

  length_dist <- max(k_vec) + 0.5 * radius + distance

  # 真实不够的，我们需要使用gap去补充
  k_gap <- length_dist - k_vec

  # 然后分别对env_list的元素，自身内部做相关性分析
  # purrr::map(env_list, function(x){
  #   cor_out_self <- psych::corr.test(x)
  # })

  ####----环境因子自身的相关性----####
  env_cor_self_list <- list()

  # 分别进行分析
  for (i in seq_along(orientation)) {
    # top_right
    if (orientation[i] == "top_right") {
      # 这个是右上角的
      # 单独一个做相关性
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
      # 单独一个做相关性
      cor_out_self <- psych::corr.test(env_list[[i]], use = cor.use, method = cor.method)

      # 右下角的
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
      # 单独一个做相关性
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
      # 单独一个做相关性
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


  ####----核心物种与环境因子之间的关系----####

  if (relation_method == "correlation") {
    cor_spec_env_list <- list()

    for (p in seq_along(env_list)) {
      for (j in seq_along(spec_list)) {
        # correlation
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
                                      cor_env_list_tmp_p %>% dplyr::select(3,4))

        cor_spec_env_list[[p]] <- cor_env_list_tmp_r_p

      }

    }

    cor_spec_env_list_out <- do.call(rbind, cor_spec_env_list)

  }

  names(cor_spec_env_list) <- orientation

  cor_spec_env_list_out

  # core location layout
  # create graph obj
  # first to calculate spec correlation
  if (isTRUE(spec_relation)) {
    # compute relationship
    spec_relation_df <- spec_list[[1]]

    # single correlation analysis
    spec_cor_out_self <- psych::corr.test(spec_relation_df, use = cor.use, method = cor.method)

    # correlation
    spec_cor_self_r <- spec_cor_out_self$r %>% as.data.frame()
    spec_cor_self_r[lower.tri(spec_cor_self_r)] <- NA

    # pvalue
    spec_cor_self_p <- spec_cor_out_self$p %>% as.data.frame()
    spec_cor_self_p[lower.tri(spec_cor_self_p)] <- NA

    # combine
    spec_cor_self_r <- spec_cor_self_r %>%
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

    spec_cor_self_p <- spec_cor_self_p %>%
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

    spec_cor_self_r_p <- cbind(spec_cor_self_r %>% dplyr::select(1,2,4,5,3),
                               spec_cor_self_p %>% dplyr::select(3,6)
                               ) %>%
      dplyr::filter(ID != Type) %>%
      dplyr::mutate(tmp = ifelse(ID > Type, stringr::str_c(ID, Type), stringr::str_c(Type, ID))) %>%
      dplyr::distinct(tmp, .keep_all = T) %>%
      dplyr::select(-tmp) %>%
      dplyr::select(ID, Type, Correlation, p_signif) %>%
      purrr::set_names(c("from", "to", "weight", "sig"))
  }

  # create graph obj
  spec_graph_obj <- build_graph_from_df(
    df = spec_cor_self_r_p,
    node_annotation = NULL,
    directed = F,
    module.method = "Fast_greedy"
  )

  # spec_graph_obj <- igraph::graph_from_data_frame(
  #   d = spec_cor_self_r_p,
  #   vertices = NULL,
  #   directed = F
  # )
  # g <- spec_graph_obj
  #
  # tidygraph::as_tbl_graph(spec_graph_obj)

  # get location
  # find layout function
  # spec_layout = "diamond"
  # spec_layout = "gephi"
  # spec_layout = "square"
  # spec_layout = "rectangle_outline"

  func_name <- paste0("create_layout_", spec_layout)

  # find layout functions from ggNetView package
  lay_func <- utils::getFromNamespace(func_name, "ggNetView")


  ly1 = lay_func(graph_obj = spec_graph_obj,
                 r = radius,
                 node_add = NULL,
                 orientation = spec_orientation)

  ggplot(data = ly1) + geom_point(aes(x = x , y = y))

  # # 查看一下里面有多少个变量, 然后将其均等分
  # n_points <- cor_spec_env_list_out$ID %>% unique() %>% length()
  #
  # # 计算每一个点的角度
  # angles <- seq(0, 2*pi, length.out = n_points + 1)[-(n_points+1)]
  # center_x <- 0
  # center_y <- 0
  #
  # # 计算坐标
  # x <- center_x + radius * cos(angles)
  # y <- center_y + radius * sin(angles)

  # 中间点的物理位置
  cor_spec_env <- data.frame(
    # ID = cor_spec_env_list_out$ID %>% unique() %>% sort(),
    ID = spec_graph_obj %>%
      tidygraph::activate(nodes) %>%
      tidygraph::as_tibble() %>%
      tidygraph::pull(name),
    x = ly1$x,
    y = ly1$y
  )

  k_vec
  k_gap
  length_dist

  # get targets informations
  # 针对每一个 orientation 设置 x_to y_to
  .make_targets <- function(df, ori, k_gap, length_dist){
    df %>%
      dplyr::mutate(
        ID = as.character(ID),
        Type = as.character(Type)
      ) %>%
      dplyr::filter(ID == Type) %>%
      dplyr::transmute(ID,
                       x_to = dplyr::case_when(
                         ori == "top_right" ~ ID2 + k_gap[ori],
                         ori == "bottom_right" ~ ID2 + k_gap[ori],
                         ori == "top_left" ~ ID2 - length_dist,
                         ori == "bottom_left" ~ID2 - length_dist
                       ),
                       y_to = dplyr::case_when(
                         ori == "top_right" ~ Type2+k_gap[ori]-1,
                         ori == "bottom_right" ~ Type2-length_dist+1,
                         ori == "top_left" ~ Type2+k_gap[ori]-1,
                         ori == "bottom_left" ~Type2-length_dist+1
                       ))
  }

  xy_targets <- purrr::imap_dfr(
    .x = env_cor_self_list[orientation],
    .f = .make_targets,
    k_gap = k_gap,
    length_dist = length_dist
  )

  xy_targets

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

  .offset_env <- function(df, ori, k_gap, length_dist, HeatmapLabelOrient = 0,
                          y_top_all = NULL, y_bottom_all = NULL){
    stopifnot(ori %in% c("top_right","bottom_right","top_left","bottom_left"))
    df <- df %>% dplyr::mutate(ID = as.character(ID), Type = as.character(Type))

    # tile 坐标（四象限通用规则）
    x_tile <- if (ori %in% c("top_right","bottom_right")) df$ID2 + k_gap[[ori]] else df$ID2 - length_dist
    y_tile <- if (ori %in% c("top_right","top_left"))      df$Type2 + k_gap[[ori]] else df$Type2 - length_dist

    tile <- df %>% dplyr::mutate(x_tile = x_tile, y_tile = y_tile, orientation = ori)

    # 对角点（你原来用于标注主对角）
    diag_df <- df %>% dplyr::filter(ID == Type)
    x_diag  <- if (ori %in% c("top_right","bottom_right")) diag_df$ID2 + k_gap[[ori]] else diag_df$ID2 - length_dist
    y_diag  <- if (ori %in% c("top_right","top_left"))     diag_df$Type2 + k_gap[[ori]] - 1 else diag_df$Type2 - length_dist + 1
    diag    <- diag_df %>% dplyr::transmute(ID, x_diag, y_diag, orientation = ori)

    # 轴标签位置
    # HeatmapLabelOrient == 0: 保持原来的相对位置
    # HeatmapLabelOrient != 0: 使用全局的顶部/底部边界，再外移 1 个单位，保证上下各象限对齐
    if (HeatmapLabelOrient == 0 || is.null(y_top_all) || is.null(y_bottom_all)) {
      y_id_lab <- if (ori %in% c("top_right","top_left")) length_dist + 1 else 0 - length_dist
    } else {
      if (ori %in% c("top_right","top_left")) {
        # 顶部两个象限：在全局热图最上边界基础上再向上挪动 1 个单位
        y_id_lab <- y_top_all + 1
      } else {
        # 底部两个象限：在全局热图最下边界基础上再向下挪动 1 个单位
        y_id_lab <- y_bottom_all - 1
      }
    }
    x_type_lab <- if (ori %in% c("top_right","bottom_right")) length_dist + 1 else 0 - length_dist
    hjust_type <- if (ori %in% c("top_right","bottom_right")) "left" else "right"

    id_lab <- df %>%
      dplyr::distinct(ID, .keep_all = TRUE) %>%
      dplyr::transmute(ID,
                       x_id = if (ori %in% c("top_right","bottom_right")) ID2 + k_gap[[ori]] else ID2 - length_dist,
                       y_id = y_id_lab, orientation = ori)

    type_lab <- df %>%
      dplyr::distinct(Type, .keep_all = TRUE) %>%
      dplyr::transmute(Type,
                       x_type = x_type_lab,
                       y_type = if (ori %in% c("top_right","top_left")) Type2 + k_gap[[ori]] else Type2 - length_dist,
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

    # 根据象限为水平 ID 标签设置对齐方式
    ori_lab <- unique(id_lab$orientation)[1]
    hjust_id <- dplyr::case_when(
      ori_lab %in% c("top_right", "top_left")       ~ 0,  # 左上/右上：左对齐
      ori_lab %in% c("bottom_right", "bottom_left") ~ 1,  # 左下/右下：右对齐
      TRUE ~ 0.5
    )
    vjust_id <- dplyr::case_when(
      ori_lab %in% c("top_right", "top_left")       ~ -0.25, # 左上/右上：向上偏移
      ori_lab %in% c("bottom_right", "bottom_left") ~  0.25, # 左下/右下：向下偏移
      TRUE ~ 0.5
    )

    p +
      ggplot2::geom_tile(
        data = tile,
        aes(x = x_tile, y = y_tile, fill = Correlation),
        colour = HeatmapTileColor,
        linewidth = HeatmapTileSize
      ) +
      ggplot2::geom_text(data = tile, aes(x = x_tile, y = y_tile, label = p_signif), size = HeatmapSigSize) +
      ggplot2::geom_text(data = id_lab, aes(x = x_id,   y = y_id,   label = ID),
                size = HeatmapLabelSize,
                vjust = vjust_id,
                hjust = hjust_id,
                angle = HeatmapLabelOrient) +
      ggplot2::geom_text(data = type_lab, aes(x = x_type, y = y_type, label = Type),
                hjust = type_lab$hjust_type[1],
                size = HeatmapLabelSize) +
      ggplot2::geom_point(data = diag, aes(x = x_diag, y = y_diag),
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

  # 先计算所有象限中 tile 的全局顶部/底部边界，便于对齐标签
  .compute_y_range <- function(df, ori, k_gap, length_dist){
    df <- df %>% dplyr::mutate(ID = as.character(ID), Type = as.character(Type))
    x_tile <- if (ori %in% c("top_right","bottom_right")) df$ID2 + k_gap[[ori]] else df$ID2 - length_dist
    y_tile <- if (ori %in% c("top_right","top_left"))      df$Type2 + k_gap[[ori]] else df$Type2 - length_dist
    data.frame(
      orientation = ori,
      ymin = min(y_tile, na.rm = TRUE),
      ymax = max(y_tile, na.rm = TRUE)
    )
  }

  y_ranges <- purrr::imap_dfr(
    env_cor_self_list[orientation],
    ~ .compute_y_range(.x, .y, k_gap, length_dist)
  )

  y_top_all <- y_ranges %>%
    dplyr::filter(.data$orientation %in% c("top_right","top_left")) %>%
    dplyr::pull(.data$ymax) %>%
    max(na.rm = TRUE)

  y_bottom_all <- y_ranges %>%
    dplyr::filter(.data$orientation %in% c("bottom_right","bottom_left")) %>%
    dplyr::pull(.data$ymin) %>%
    min(na.rm = TRUE)

  # 先为每个方位算好偏移后的数据包
  packs <- purrr::imap(
    env_cor_self_list[orientation],
    ~ .offset_env(.x, .y, k_gap, length_dist,
                  HeatmapLabelOrient = HeatmapLabelOrient,
                  y_top_all = y_top_all,
                  y_bottom_all = y_bottom_all)
  )

  # 汇总所有象限中热图对角线上的点，用于在连线之后再次绘制，避免被遮挡
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
  # 统一叠加连线 & 中圈节点
  p1 <- p0 +
    ggnewscale::new_scale_color() +
    ggplot2::geom_segment(
      data = link_df,
      aes(x = x, y = y, xend = x_to, yend = y_to,
          color = Correlation,
          linetype = line_type,
          linewidth = -log10(Pvalue)),
      alpha = 0.5
    ) +
    ggplot2::scale_color_gradient(low = SigLineColor[1], high = SigLineColor[2]) +
    ggplot2::scale_linewidth_continuous(range = SigLineWidth) +
    ggplot2::scale_linetype_identity() +
    # 再次覆盖绘制热图对角点，保证在线段上方
    ggplot2::geom_point(
      data = diag_all,
      aes(x = x_diag, y = y_diag),
      shape = 21, fill = HeatmapPointFill, size = HeatmapPointSize
    ) +
    ggplot2::geom_point(
      data = cor_spec_env,
      aes(x = x, y = y), shape = 21, fill = CorePointFill, size = CorePointSize
    ) +
    ggplot2::geom_text(
      data = cor_spec_env,
      aes(x = x, y = y, label = ID), size = 5
    ) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::theme_void() +
    ggplot2::theme(
      plot.margin = margin(10,10,10,10),
      aspect.ratio = 1,
      legend.position = "top"
    )

  p1


  p2 <- p0 +
    ggnewscale::new_scale_color() +
    ggplot2::geom_segment(
      data = link_df,
      aes(x = x, y = y, xend = x_to, yend = y_to,
          color = Correlation,
          linetype = line_type,
          linewidth = -log10(Pvalue)),
      alpha = 0.5
    ) +
    ggplot2::scale_color_gradient(low = SigLineColor[1], high = SigLineColor[2]) +
    ggplot2::scale_linewidth_continuous(range = SigLineWidth) +
    ggplot2::scale_linetype_identity() +
    ggplot2::geom_point(
      data = diag_all,
      aes(x = x_diag, y = y_diag),
      shape = 21, fill = HeatmapPointFill, size = HeatmapPointSize
    ) +
    ggplot2::geom_point(
      data = cor_spec_env,
      aes(x = x, y = y), shape = 21, fill = CorePointFill, size = CorePointSize
    ) +
    ggplot2::geom_text(
      data = cor_spec_env,
      aes(x = x, y = y, label = ID),
      size = 5
    ) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::theme_void() +
    ggplot2::theme(
      plot.margin = margin(10,10,10,10),
      aspect.ratio = 1,
      legend.position = "top"
    )

  p2 <- p0 +
    ggnewscale::new_scale_fill() +
    ggplot2::geom_curve(data = link_df,
              mapping = aes(x = x, y = y, xend = x_to, yend = y_to,
                            color = Correlation,
                            linetype = line_type,
                            linewidth = -log10(Pvalue)),
               alpha = 0.5,
               curvature = 0.25
    ) +
    ggplot2::scale_color_gradient(low = SigLineColor[1], high = SigLineColor[2]) +
    ggplot2::scale_linewidth_continuous(range = SigLineWidth) +
    ggplot2::scale_linetype_identity() +
    ggplot2::geom_point(
      data = diag_all,
      aes(x = x_diag, y = y_diag),
      shape = 21, fill = HeatmapPointFill, size = HeatmapPointSize
    ) +
    ggplot2::geom_point(data = cor_spec_env,
               mapping = aes(x = x, y = y, fill = ID),
               shape = 21,
               fill = CorePointFill,
               size = CorePointSize) +
    ggplot2::geom_text(data = cor_spec_env,
              mapping = aes(x =x, y = y, label = ID),
              size = 5) +
    # geom_line(data = cor_spec_env_location %>% dplyr::distinct(ID, .keep_all = T) %>% dplyr::select(ID, x, y),
    #           mapping = aes(x = x, y = y, group = 1),
    #           linetype = 1,
    #           linewidth = 1.5,
    #           color = "#41b6c4") +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::theme_void() +
    ggplot2::theme(
      plot.margin = margin(1,1,1,1,"cm"),
      aspect.ratio = 1,
      legend.position = "top"
    )

  p2

  return(list(p1, p2, cor_spec_env_list_out))

}


