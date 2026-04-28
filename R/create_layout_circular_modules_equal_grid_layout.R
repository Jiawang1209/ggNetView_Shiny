#' @noRd
create_layout_circular_modules_equal_grid_layout <- function(
    graph_obj,
    r = 1,
    anchor_dist = 10,
    scale = TRUE,
    orientation = c("up", "down", "left", "right"),
    angle = 0
) {
  orientation <- match.arg(orientation)
  base_angle <- switch(
    orientation,
    up = 0, right = -pi / 2, down = pi, left = pi / 2
  )
  theta_shift <- base_angle + angle

  node_df <- graph_obj %>%
    tidygraph::activate(nodes) %>%
    tidygraph::as_tibble() %>%
    dplyr::mutate(.node_index__ = dplyr::row_number())

  mod_levels <- node_df$Modularity %>% droplevels() %>% levels() %>% as.character()
  module_list <- node_df %>% dplyr::group_split(Modularity, .keep = TRUE)
  n_vec <- purrr::map_int(module_list, nrow)
  n_mod <- length(n_vec)
  if (n_mod < 1) stop("Circular modules layout requires at least 1 module (from column `Modularity`).")

  angles <- pi / 2 - 2 * pi * (0:(n_mod - 1)) / n_mod
  anchors <- lapply(angles, function(a) c(anchor_dist * cos(a), anchor_dist * sin(a)))

  ig <- tidygraph::as.igraph(graph_obj)
  R_target <- r * sqrt(max(n_vec))

  ly_list <- vector("list", n_mod)
  for (i in seq_len(n_mod)) {
    cx <- anchors[[i]][1]
    cy <- anchors[[i]][2]
    node_names <- module_list[[i]]$name
    vid <- match(node_names, igraph::V(ig)$name)
    subg <- igraph::induced_subgraph(ig, vid)
    coords_raw <- igraph::layout_on_grid(subg) %>%
      as.data.frame() %>%
      purrr::set_names(c("x", "y"))
    if (nrow(coords_raw) == 0) {
      coords_i <- data.frame(x = cx, y = cy)
    } else {
      cx_raw <- mean(coords_raw$x)
      cy_raw <- mean(coords_raw$y)
      coords_centered <- coords_raw %>%
        dplyr::mutate(x = x - cx_raw, y = y - cy_raw)
      extent <- max(
        max(coords_centered$x) - min(coords_centered$x),
        max(coords_centered$y) - min(coords_centered$y),
        1e-10
      )
      scale_f <- R_target / extent
      coords_i <- coords_centered %>%
        dplyr::mutate(x = x * scale_f + cx, y = y * scale_f + cy)
    }
    nodes_i <- module_list[[i]] %>% dplyr::select(.node_index__)
    if (nrow(coords_i) != nrow(nodes_i)) stop("Internal error: coords length != node count.")
    ly_i <- dplyr::bind_cols(nodes_i, coords_i)
    ly_i$group <- mod_levels[i]
    ly_list[[i]] <- ly_i
  }

  ly <- dplyr::bind_rows(ly_list)
  if (theta_shift != 0) {
    Rm <- matrix(c(cos(theta_shift), -sin(theta_shift), sin(theta_shift), cos(theta_shift)), nrow = 2)
    xy <- as.matrix(ly[, c("x", "y")])
    ly[, c("x", "y")] <- t(Rm %*% t(xy))
  }
  if (isTRUE(scale)) {
    rescale01 <- function(v) {
      rng <- range(v, na.rm = TRUE)
      if (diff(rng) == 0) return(rep(0, length(v)))
      (v - rng[1]) / diff(rng)
    }
    ly$x <- rescale01(ly$x) * 2 - 1
    ly$y <- rescale01(ly$y) * 2 - 1
  }
  ly <- ly %>% dplyr::arrange(.node_index__) %>% dplyr::select(x, y, group)
  rownames(ly) <- NULL
  ly
}
