#' Visualize multi-orientation environmental-species correlation heatmaps2
#'
#' @param Environment character or data.frame
#' File path or data frame of environment data.
#' @param Experiment character or data.frame
#' File path or data frame of experiment data.
#' @param edge character or data.frame
#' File path or data frame of edge data.
#' @param node character or data.frame
#' File path or data frame of node data.
#' @param sample_col Character (default = "Sample")
#' Column name used as sample ID when input is a data frame or file.
#' @param delim Character (default = ",")
#' Delimiter for reading input files.
#' @param hub_n Integer (default = NULL)
#' Number of hub nodes used in layout; if NULL, uses all nodes.
#' @param r numeric (default = 6)
#'
#' @returns a ggplot2 object
#' @export
#'
#' @examples
#' \dontrun{
#' p <- gglink_heatmap_triple(
#'   Environment = env_df,
#'   Experiment  = exp_df,
#'   edge        = edge_df,
#'   node        = node_df
#' )
#' }
gglink_heatmap_triple <- function(
    Environment,
    Experiment,
    edge,
    node,
    sample_col = "Sample",
    delim = ",",
    hub_n = NULL,
    r = 6
){

  .read_table <- function(x) {
    if (is.character(x)) {
      readr::read_delim(file = x, delim = delim)
    } else if (is.data.frame(x)) {
      x
    } else {
      stop("Inputs must be file paths or data frames.")
    }
  }

  # Environment Data
  Environment <- .read_table(Environment) %>%
    tibble::as_tibble()
  if (!sample_col %in% colnames(Environment)) {
    stop("`sample_col` not found in Environment.")
  }
  Environment <- Environment %>%
    tibble::column_to_rownames(var = sample_col)

  # Experiment Data
  Experiment <- .read_table(Experiment) %>%
    tibble::as_tibble()
  if (!sample_col %in% colnames(Experiment)) {
    stop("`sample_col` not found in Experiment.")
  }
  Experiment <- Experiment %>%
    tibble::column_to_rownames(var = sample_col)

  # edge Data
  edge <- .read_table(edge) %>%
    tibble::as_tibble()
  if (!all(c("from", "to") %in% colnames(edge))) {
    stop("`edge` must contain columns: from, to.")
  }

  # node Data
  node <- .read_table(node) %>%
    tibble::as_tibble()
  if (!"node" %in% colnames(node)) {
    stop("`node` must contain column: node.")
  }


  # Correlation
  stat_out <- cor_test2(Environment, Experiment)

  graph_obj <- tidygraph::tbl_graph(nodes = node, edges = edge)

  # layout
  layout_manual <- create_layout2(graph_obj,
                                 stat_out = stat_out,
                                 hub_names = NULL,
                                 hub_n = hub_n,
                                 r = r)

  hm_df <- stat_out[[1]]
  id_lab <- hm_df %>%
    dplyr::distinct(ID, ID2, .keep_all = TRUE) %>%
    dplyr::mutate(
      x_lab = ID2,
      y_lab = max(Type2, na.rm = TRUE) + 1
    )
  type_lab <- hm_df %>%
    dplyr::distinct(Type, Type2, .keep_all = TRUE) %>%
    dplyr::mutate(
      x_lab = max(ID2, na.rm = TRUE) + 1,
      y_lab = Type2
    )

  p <- ggraph::ggraph(layout_manual)  +
    ggraph::geom_edge_link(aes(color = weight, width = weight)) +
    ggraph::scale_edge_color_gradientn(colors = c("#74add1","#abd9e9","#ffffbf","#fdae61","#f46d43"),
                               guide = ggraph::guide_edge_colorbar(direction = "horizontal",
                                                           title.position = "top")) +
    ggraph::scale_edge_width(range = c(0.1, 1),
                     guide = ggplot2::guide_legend(nrow = 2,
                                          direction = "horizontal",
                                          title.position = "top"))  +
    ggnewscale::new_scale_fill() +
    ggplot2::geom_tile(data = hm_df,
              aes(x = ID2, y = Type2), fill = "white", color = "#000000", linewidth = 0.5, inherit.aes = F) +
    ggplot2::geom_text(data = id_lab,
              aes(x = x_lab, y = y_lab, label = ID), inherit.aes = FALSE) +
    ggplot2::geom_text(data = type_lab,
              aes(x = x_lab, y = y_lab, label = Type), hjust = "left") +
    ggplot2::geom_point(data = hm_df,
               aes(x = ID2, y = Type2, fill = Value, size = abs(Value)), shape = 21, color = "black") +
    ggplot2::geom_text(data = stat_out[[2]],
              aes(x = ID2, y = Type2, label = p_value),
              size = 7.5) +
    ggplot2::scale_fill_gradient(low = "#edf8b1", high = "#1d91c0", name = "Env Cor",
                        guide = ggplot2::guide_colorbar(direction = "horizontal",
                                               title.position = "top")) +
    ggplot2::scale_x_discrete(position = "top") +
    ggplot2::scale_y_discrete(position = "right") +
    ggplot2::scale_size(range = c(6,16),
               guide = ggplot2::guide_legend(direction = "horizontal",
                                    title.position = "top"),
               name = "Env Cor Size") +
    ggplot2::xlab('') +
    ggplot2::ylab('')  +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::geom_segment(data = stat_out[[3]] ,
                 mapping = aes(x = p_start1, y = p_end1, xend = start2, yend = end2,
                               linetype = p_value,
                               color = Value,
                               linewidth = abs(Value))) +
    ggplot2::scale_color_gradient(low = "#e0f3db", high = "#4eb3d3", name = "Correlation",
                         guide = ggplot2::guide_colorbar(direction = "horizontal",
                                                title.position = "top")) +
    ggplot2::scale_linewidth(range = c(1, 2.5),
                    name = "Cor",
                    guide = ggplot2::guide_legend(direction = "horizontal",
                                         nrow = 2,
                                         title.position = "top")) +
    ggplot2::scale_linetype(name = "PValue",
                   guide = ggplot2::guide_legend(direction = "horizontal",
                                        nrow = 2,
                                        title.position = "top")) +
    ggplot2::geom_point(data = stat_out[[3]],
               mapping = aes(x = p_start1, y = p_end1), fill = "#9e9ac8", size = 5, shape = 21) +
   ggnewscale::new_scale_fill() +
    ggraph::geom_node_point(data = layout_manual, aes(fill = annotation, shape = annotation), size = 13.5) +
    ggplot2::scale_fill_manual(values = c("#66c2a5", "#fc8d62", "#8da0cb", "#e78ac3", "#a6d854", "#ffd92f"),
                      guide = ggplot2::guide_legend(title.position = "top",
                                           nrow = 2)) +
    ggplot2::scale_shape_manual(values = c(21,21:25),
                       guide = ggplot2::guide_legend(title.position = "top",
                                            nrow = 2)) +
    # `n_points` is the number of non-hub nodes (placed on the outer circle);
    # `create_layout2()` attaches it as an attribute so we don't hard-code the split.
    ggraph::geom_node_text(
      data = layout_manual %>%
        tidygraph::slice(-seq_len(.n_points_attr(layout_manual))),
      aes(label = node)
    ) +
    ggraph::geom_node_text(
      data = layout_manual %>%
        tidygraph::slice(seq_len(.n_points_attr(layout_manual))),
      aes(x =  x + 0.25,
          y =  y,
          label = node,
      ),
      color = "#000000",
      size = 3,
      hjust = 'outward'
    ) +
    ggplot2::guides(shape = ggplot2::guide_legend(nrow = 2)) +
    ggplot2::coord_equal(clip = "off") +
    ggraph::theme_graph() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, family = "bold"),
      plot.margin = ggplot2::margin(1,1,1,1,"cm"),
      legend.position = "bottom",
      legend.title = ggplot2::element_text(hjust = 0.5),
      legend.ticks = ggplot2::element_line(color = "#000000"),
      legend.frame = ggplot2::element_rect(color = "#000000")
    )

  return(p)
}
