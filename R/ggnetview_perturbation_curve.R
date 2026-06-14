#' Plot a network perturbation (attack) curve
#'
#' Renders the perturbation curve produced by
#' [get_network_perturbation()] -- a chosen topology metric against the
#' fraction of nodes removed, coloured by strategy. For the random
#' strategy a mean +/- standard-error ribbon is drawn. The styling matches
#' the other `ggnetview_*` plots (`theme_classic`, square aspect, black
#' axes).
#'
#' @param curve A data frame as returned in the `curve` element of
#'   [get_network_perturbation()], or that element bound from several runs
#'   (e.g. `rbind(random$curve, targeted$curve)`) to overlay strategies.
#' @param metric Character (default `"LCC_fraction"`). Which metric to
#'   plot; must be present in `curve$metric`.
#'
#' @returns A ggplot object.
#'
#' @seealso [get_network_perturbation()].
#'
#' @export
#'
#' @examples
#' \donttest{
#' data(ppi_example)
#' obj <- build_graph_from_df(
#'   df              = ppi_example$ppi,
#'   node_annotation = ppi_example$annotation
#' )
#' rnd <- get_network_perturbation(obj, strategy = "random", bootstrap = 20)
#' tgt <- get_network_perturbation(obj, strategy = "targeted",
#'                                 centrality = "degree")
#' ggnetview_perturbation_curve(rbind(rnd$curve, tgt$curve))
#' }
ggnetview_perturbation_curve <- function(curve, metric = "LCC_fraction") {

  if (!is.data.frame(curve) || !all(c("strategy", "fraction", "metric", "value") %in% names(curve))) {
    stop("`curve` must be the `curve` data frame from get_network_perturbation().",
         call. = FALSE)
  }
  if (!metric %in% curve$metric) {
    stop(sprintf("Metric `%s` not found. Available: %s", metric,
                 paste(unique(curve$metric), collapse = ", ")), call. = FALSE)
  }

  d <- curve[curve$metric == metric, , drop = FALSE]
  has_se <- "value_se" %in% names(d) && any(is.finite(d$value_se) & d$value_se > 0)

  p <- ggplot2::ggplot(
    d, ggplot2::aes(x = .data$fraction, y = .data$value,
                    colour = .data$strategy, fill = .data$strategy)
  )

  if (has_se) {
    p <- p + ggplot2::geom_ribbon(
      ggplot2::aes(ymin = .data$value - .data$value_se,
                   ymax = .data$value + .data$value_se),
      colour = NA, alpha = 0.2
    )
  }

  p +
    ggplot2::geom_line(linewidth = 0.9) +
    ggplot2::geom_point(size = 2) +
    ggplot2::labs(
      x = "Fraction of nodes removed",
      y = metric,
      colour = "Strategy", fill = "Strategy"
    ) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      aspect.ratio = 1,
      panel.grid = ggplot2::element_blank(),
      axis.line  = ggplot2::element_line(colour = "black"),
      axis.ticks = ggplot2::element_line(colour = "black"),
      axis.text  = ggplot2::element_text(colour = "black", size = 11),
      axis.title = ggplot2::element_text(colour = "black", size = 13)
    )
}
