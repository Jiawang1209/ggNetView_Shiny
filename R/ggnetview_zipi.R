#' Compute Zi-Pi (within-module connectivity and participation coefficient)
#'
#' Calculates the within-module degree z-score (Zi) and among-module connectivity
#' (participation coefficient, Pi) for each node in a modular network.
#' These metrics classify nodes into roles such as module hubs, connectors,
#' and peripherals.
#'
#' @param nodes_bulk Data frame or tibble.
#'   Node table with modularity and degree information.
#'   Node IDs must be in \code{rownames} or in a \code{name} column (compatible
#'   with \code{tidygraph::as_tibble} output).
#' @param z_bulk_mat Numeric matrix.
#'   Adjacency or correlation matrix; rows and columns must correspond to nodes.
#'   Non-zero entries are treated as edges. \code{NA}/\code{Inf} are replaced with 0.
#' @param modularity_col Character.
#'   Column name in \code{nodes_bulk} containing module labels.
#' @param degree_col Character.
#'   Column name in \code{nodes_bulk} containing node degree (number of edges).
#'
#' @param zi_threshold Numeric (default = 2.5).
#'   Threshold for within-module connectivity (Zi) in role classification.
#' @param pi_threshold Numeric (default = 0.62).
#'   Threshold for among-module connectivity (Pi) in role classification.
#' @param na.rm Logical (default = \code{FALSE}).
#'   If \code{TRUE}, remove rows with NA in Zi or Pi from the output.
#'   If \code{FALSE}, keep all rows; NA in Zi/Pi results in \code{type = NA}.
#'
#' @returns A list with two elements:
#'   \itemize{
#'     \item \code{data}: Data frame merging \code{nodes_bulk} with
#'       \code{within_module_connectivities}, \code{among_module_connectivities},
#'       and \code{type} (node role).
#'     \item \code{plot}: ggplot object of the Zi-Pi scatter plot with quadrant
#'       labels and background shading.
#'   }
#'
#' @details
#' \strong{Zi (within-module connectivity):} Reflects how strongly a node is
#' connected within its own module. Higher values indicate the node has more
#' connections within the module and may play a core role inside it.
#'
#' \strong{Pi (among-module connectivity):} Measures how much a node connects
#' to other modules. Higher values indicate the node acts as a bridge between
#' modules, facilitating information, material or energy flow across the network.
#'
#' \strong{Node roles (by default thresholds Zi=2.5, Pi=0.62):}
#' \itemize{
#'   \item \code{Module hubs}: High Zi, low Pi. Core members within their module,
#'     important for module stability and function, but weakly connected to
#'     other modules.
#'   \item \code{Connectors}: Low Zi, high Pi. Not prominent within their module,
#'     but strongly connect across modules, acting as bridges.
#'   \item \code{Network hubs}: High Zi, high Pi. Core nodes both within and
#'     across modules, critical for overall network structure and stability.
#'   \item \code{Peripherals}: Low Zi, low Pi. Peripheral or satellite nodes
#'     with few connections within and across modules.
#' }
#'
#' @references
#'   Guimera R, Amaral LAN (2005). "Functional cartography of complex metabolic
#'   networks." \emph{Nature} 433(7028):895-900.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' g <- build_graph_from_mat(otu_rare_relative, method = "WGCNA",
#'   transfrom.method = "none", cor.method = "pearson", proc = "Bonferroni",
#'   module.method = "Fast_greedy")
#' nodes_bulk <- get_graph_nodes(g)
#' adj_mat <- get_graph_adjacency(g)
#' res <- ggnetview_zipi(nodes_bulk, adj_mat, "Modularity", "Degree")


#' }
ggnetview_zipi <- function(nodes_bulk, z_bulk_mat, modularity_col, degree_col,
                          zi_threshold = 2.5, pi_threshold = 0.62, na.rm = FALSE) {
  if (!is.data.frame(nodes_bulk)) {
    stop("`nodes_bulk` must be a data frame or tibble.", call. = FALSE)
  }
  if (!is.matrix(z_bulk_mat) && !is.data.frame(z_bulk_mat)) {
    stop("`z_bulk_mat` must be a matrix or data frame.", call. = FALSE)
  }
  z_bulk_mat <- as.matrix(z_bulk_mat)
  if (is.null(rownames(z_bulk_mat)) || is.null(colnames(z_bulk_mat))) {
    stop("`z_bulk_mat` must have rownames and colnames (node IDs).", call. = FALSE)
  }


  rn <- rownames(nodes_bulk)
  if (is.null(rn) || length(rn) == 0L ||
      identical(rn, as.character(seq_len(nrow(nodes_bulk))))) {
    if ("name" %in% names(nodes_bulk)) {
      ids <- as.character(nodes_bulk[["name"]])
    } else {
      stop("`nodes_bulk` must have rownames as node IDs, or contain a `name` column.")
    }
  } else {
    ids <- rn
  }
  if (length(ids) == 0L || any(is.na(ids)) || any(ids == "")) {
    stop("Node IDs in `nodes_bulk` (rownames or `name` column) must not be empty or NA.")
  }


  if (!all(ids %in% rownames(z_bulk_mat))) {
    stop("`rownames(nodes_bulk)` must be a subset of `rownames(z_bulk_mat)` and aligned.")
  }

  z_bulk_mat <- z_bulk_mat[ids, ids, drop = FALSE]


  if (any(!is.finite(z_bulk_mat))) {
    z_bulk_mat[!is.finite(z_bulk_mat)] <- 0
  }
  A <- (abs(z_bulk_mat) > 0) * 1L
  diag(A) <- 1L

  if (!modularity_col %in% names(nodes_bulk)) {
    stop(sprintf("Column `%s` is missing from `nodes_bulk`.", modularity_col))
  }
  if (!degree_col %in% names(nodes_bulk)) {
    stop(sprintf("Column `%s` is missing from `nodes_bulk`.", degree_col))
  }
  mod  <- nodes_bulk[[modularity_col]]
  deg  <- nodes_bulk[[degree_col]]


  if (any(is.na(mod))) stop("Module column contains NA values.")
  if (any(is.na(deg))) stop("Degree column contains NA values.")



  split_idx <- split(seq_along(ids), f = factor(mod, levels = unique(mod)))
  z_vec <- numeric(length(ids)); names(z_vec) <- ids

  for (lev in names(split_idx)) {
    idx <- split_idx[[lev]]
    if (length(idx) <= 1) {
      z_vec[idx] <- 0
      next
    }
    Aii <- A[idx, idx, drop = FALSE]
    k_in <- rowSums(Aii) - 1L
    sd_k <- stats::sd(k_in)
    if (sd_k == 0) z_vec[idx] <- 0 else z_vec[idx] <- (k_in - mean(k_in)) / sd_k
  }



  modules <- names(split_idx)
  kis_mat <- sapply(modules, function(lev) {
    idx <- split_idx[[lev]]
    rowSums(A[, idx, drop = FALSE])
  })
  if (!is.matrix(kis_mat)) kis_mat <- as.matrix(kis_mat)

  for (j in seq_along(modules)) {
    idx <- split_idx[[modules[j]]]
    kis_mat[idx, j] <- kis_mat[idx, j] - 1L
  }
  kis_mat[kis_mat < 0] <- 0

  sum_kis2 <- rowSums(kis_mat^2)
  k_tot    <- as.numeric(deg)
  P <- numeric(length(k_tot))
  P[k_tot == 0] <- 0
  nz <- (k_tot > 0)
  P[nz] <- 1 - (sum_kis2[nz] / (k_tot[nz]^2))
  names(P) <- ids


  id_col <- "name"
  if (!id_col %in% names(nodes_bulk)) {
    nodes_bulk[[id_col]] <- ids
  }
  out <- data.frame(
    name = ids,
    within_module_connectivities = z_vec[ids],
    among_module_connectivities  = P[ids],
    row.names = NULL,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  zi_pi <- dplyr::left_join(nodes_bulk, out, by = id_col)


  zi_pi$type <- NA_character_
  zi_ok <- !is.na(zi_pi$within_module_connectivities)
  pi_ok <- !is.na(zi_pi$among_module_connectivities)
  valid <- zi_ok & pi_ok

  zi_pi$type[valid & zi_pi$within_module_connectivities < zi_threshold & zi_pi$among_module_connectivities < pi_threshold] <- "Peripherals"
  zi_pi$type[valid & zi_pi$within_module_connectivities < zi_threshold & zi_pi$among_module_connectivities >= pi_threshold] <- "Connectors"
  zi_pi$type[valid & zi_pi$within_module_connectivities >= zi_threshold & zi_pi$among_module_connectivities < pi_threshold] <- "Module hubs"
  zi_pi$type[valid & zi_pi$within_module_connectivities >= zi_threshold & zi_pi$among_module_connectivities >= pi_threshold] <- "Network hubs"

  if (isTRUE(na.rm)) {
    zi_pi <- zi_pi[valid, , drop = FALSE]
  }


  plot_data <- zi_pi[valid, , drop = FALSE]
  if (nrow(plot_data) > 0L) {
    x_vals <- plot_data$among_module_connectivities
    y_vals <- plot_data$within_module_connectivities

    x_lim <- range(c(x_vals, pi_threshold), na.rm = TRUE)
    y_lim <- range(c(y_vals, zi_threshold), na.rm = TRUE)
    x_range_pre <- diff(x_lim)
    x_lim[2L] <- max(x_lim[2L], pi_threshold + 0.15 * x_range_pre)

    type_colors <- c(
      "Peripherals"   = "#377eb8",
      "Connectors"    = "#4daf4a",
      "Module hubs"   = "#e41a1c",
      "Network hubs"  = "#ff7f00"
    )
    zi_pi$type <- factor(zi_pi$type, levels = names(type_colors))


    p0 <- ggplot2::ggplot(
      data = zi_pi,
      ggplot2::aes(
        x = .data$among_module_connectivities,
        y = .data$within_module_connectivities
      )
    ) +
      ggplot2::scale_x_continuous(limits = x_lim, expand = c(0.001, 0.1)) +
      ggplot2::scale_y_continuous(limits = y_lim, expand = c(0.1, 0.1))


    built0 <- ggplot2::ggplot_build(p0)
    lay0 <- built0$layout
    x_range <- lay0$panel_scales_x[[1L]]$limits
    y_range <- lay0$panel_scales_y[[1L]]$limits
    lab_size <- 5.5
    x_range_diff <- diff(x_range)
    y_range_diff <- diff(y_range)

    left_half <- pi_threshold - x_range[1L]
    x_left <- pi_threshold - 0.015 * left_half

    right_half <- x_range[2L] - pi_threshold
    x_right <- x_range[2L] - 0.02 * x_range_diff
    x_right <- max(x_right, pi_threshold + 0.1 * right_half)
    y_top   <- y_range[2L] - 0.02 * y_range_diff
    y_bot   <- y_range[1L] + 0.02 * y_range_diff


    p_zipi <- p0 +
      ggplot2::aes(color = .data$type) +
      ggplot2::annotate("rect", fill = "#b3cde3", xmin = -Inf, xmax = pi_threshold,
                       ymin = -Inf, ymax = zi_threshold, alpha = 0.25) +
      ggplot2::annotate("rect", fill = "#fbb4ae", xmin = -Inf, xmax = pi_threshold,
                       ymin = zi_threshold, ymax = Inf, alpha = 0.25) +
      ggplot2::annotate("rect", fill = "#ccebc5", xmin = pi_threshold, xmax = Inf,
                       ymin = -Inf, ymax = zi_threshold, alpha = 0.25) +
      ggplot2::annotate("rect", fill = "#fed9a6", xmin = pi_threshold, xmax = Inf,
                       ymin = zi_threshold, ymax = Inf, alpha = 0.25) +
      ggplot2::annotate("text", label = "Module hubs", x = x_left, y = y_top,
                       size = lab_size, hjust = 0, vjust = 0.5) +
      ggplot2::annotate("text", label = "Peripherals", x = x_left, y = y_bot,
                       size = lab_size, hjust = 0, vjust = 0.5) +
      ggplot2::annotate("text", label = "Network hubs", x = x_right, y = y_top,
                       size = lab_size, hjust = 1, vjust = 0.5) +
      ggplot2::annotate("text", label = "Connectors", x = x_right, y = y_bot,
                       size = lab_size, hjust = 1, vjust = 0.5) +
      ggplot2::geom_vline(xintercept = pi_threshold, linetype = 1) +
      ggplot2::geom_hline(yintercept = zi_threshold, linetype = 1) +
      ggplot2::geom_point(alpha = 0.8, na.rm = TRUE, size = 3) +
      ggplot2::scale_color_manual(
        values = type_colors,
        na.value = "grey50",
        name = "Node role",
        drop = FALSE,
        na.translate = TRUE
      ) +
      ggplot2::labs(
        x = "Among-module connectivities (Pi)",
        y = "Within-module connectivities (Zi)"
      ) +
      ggplot2::theme_classic() +
      ggplot2::theme(
        aspect.ratio = 1,
        panel.grid = ggplot2::element_blank(),
        axis.line = ggplot2::element_line(colour = "black"),
        axis.ticks = ggplot2::element_line(colour = "black"),
        axis.text = ggplot2::element_text(colour = "black", size = 11),
        axis.title = ggplot2::element_text(colour = "black", size = 13)
      ) +
      ggplot2::coord_cartesian(clip = "off")
  } else {
    p_zipi <- NULL
  }

  list(data = zi_pi, plot = p_zipi)
}
