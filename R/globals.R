## ggNetView: namespace imports and global variable declarations
## This file consolidates all base/imported function imports and the
## tidy-eval / data-masking column names that R CMD check flags as
## "no visible binding for global variable". It is intentionally
## documentation-only (no exported objects).

#' ggNetView internal: namespace imports
#'
#' Internal scaffold to declare base R and third-party function imports.
#' Not intended to be called directly.
#'
#' @name ggNetView-internal
#' @keywords internal
#' @importFrom ggplot2 aes after_stat coord_cartesian coord_fixed geom_curve
#' @importFrom ggplot2 geom_point geom_segment geom_text geom_tile ggplot
#' @importFrom ggplot2 guide_colorbar margin scale_color_gradient
#' @importFrom ggplot2 scale_fill_gradient2 theme theme_bw theme_void
#' @importFrom ggnewscale new_scale_fill
#' @importFrom dplyr all_of case_when desc everything n where
#' @importFrom tibble as_tibble
#' @importFrom stringr str_c str_remove
#' @importFrom methods as
#' @importFrom stats cov cov2cor density median na.exclude p.adjust phyper
#' @importFrom stats sd setNames
#' @importFrom graphics curve hist
#' @importFrom utils head
NULL

utils::globalVariables(c(
  ## tidy-eval placeholders
  ".", ".data",
  ## internal helpers (leading dot)
  ".curve_bin", ".curve_id", ".edge_dist", ".group_outer",
  ".label_text", ".mag_deg", ".mag_group", ".node_index__",
  ## generic data-frame columns
  "annotation", "ASV", "ASV_to", "auto_score",
  "Authority_score",
  "Betweenness",
  "Closeness", "Coreness",
  "Eigenvector",
  "Harmonic", "Hub_score",
  "Influence", "IVI",
  "PageRank",
  "cluster",
  "corr_direction", "correlation", "Correlation", "Count",
  "degree", "Degree",
  "dx", "edges", "end2",
  "Experiment",
  "from", "from_group", "from_id", "from_x", "from_y",
  "group", "Group", "GroupA", "GroupA_x_center", "GroupA_y_center",
  "GroupB", "GroupB_x_center", "GroupB_y_center",
  "id", "ID", "ID2",
  "ks_p", "lab_df", "leaf", "line_type",
  "mean_relative_abundance", "mod_target",
  "modularity", "Modularity", "modularity2", "modularity3", "modularity4",
  "Module",
  "name", "name_from", "name_to", "new_modularity",
  "node", "node_size", "node1.node", "nodes",
  "old_modularity", "ONTOLOGY",
  "p_end1", "p_signif", "p_start1", "p_value", "pad",
  "pvalue", "Pvalue",
  "r_neg_mean", "r_pos_mean", "relative_abundance",
  "Sample", "scale_v", "side", "sig_strength", "size", "spacing",
  "spec_block",
  "sse_exp", "start2",
  "t_total", "threshold", "tmp",
  "to", "to_group", "to_id", "to_x", "to_y",
  "type", "Type", "Type2",
  "V1", "value", "Value",
  "weight",
  "x", "x_A", "x_anchor", "x_B", "x_diag", "x_id", "x_lab",
  "x_mid", "x_tile", "x_to", "x_type",
  "xend", "xmax", "xmid", "xmin", "xmind", "xr",
  "y", "y_A", "y_B", "y_diag", "y_id", "y_lab", "y_rank",
  "y_target", "y_tile", "y_to", "y_type",
  "yend", "ymax", "ymid", "ymin", "ymind", "yr",
  ## label-geometry / mask helpers used in ggNetView() and link plots
  "mx", "my", "elbow_x", "elbow_y", "y_anchor",
  "theta_actual", "theta_target", "polygon_id",
  ## dataset names referenced as bare symbols in defaults / examples
  "Envdf_4st", "Spedf"
))
