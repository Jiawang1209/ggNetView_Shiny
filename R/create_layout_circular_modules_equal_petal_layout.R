#' @noRd
create_layout_circular_modules_equal_petal_layout <- function(
    graph_obj,
    r = 1,
    node_add = 7,
    anchor_dist = 10,
    scale = TRUE,
    petals = 6,
    amp = 0.35,
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

  circle_layout <- function(n, na) {
    counts <- 1; total <- 1; i <- 2
    while (total < n) {
      add <- na * (i - 1)
      if (total + add <= n) { counts <- c(counts, add); total <- total + add }
      else { counts <- c(counts, n - total); total <- n }
      i <- i + 1
    }
    counts
  }

  n_circle_vec <- purrr::map_int(n_vec, ~ length(circle_layout(.x, node_add)))
  n_circle_max <- max(n_circle_vec)
  R_max <- (n_circle_max - 1) * r

  n_vec_node <- purrr::map(n_vec, ~ data.frame(
    number_circle = seq_along(circle_layout(.x, node_add)),
    number_node = circle_layout(.x, node_add)
  ))

  petal_from_anchor <- function(cx, cy, info_df, r_step) {
    ly <- data.frame(x = cx, y = cy)
    if (nrow(info_df) >= 2) {
      for (index in 2:nrow(info_df)) {
        m <- info_df$number_node[index]
        R_base <- (index - 1) * r_step
        theta <- 2 * pi * (0:(m - 1)) / m
        radius <- R_base * (1 + amp * cos(petals * theta))
        x <- cx + radius * cos(theta)
        y <- cy + radius * sin(theta)
        ly <- dplyr::bind_rows(ly, data.frame(x = x, y = y))
      }
    }
    ly
  }

  ly_list <- vector("list", n_mod)
  for (i in seq_len(n_mod)) {
    cx <- anchors[[i]][1]; cy <- anchors[[i]][2]
    info_df <- n_vec_node[[i]]
    n_circle_i <- nrow(info_df)
    r_step_i <- if (n_circle_i <= 1) 0 else R_max / (n_circle_i - 1)
    coords_i <- petal_from_anchor(cx, cy, info_df, r_step = r_step_i)
    nodes_i <- module_list[[i]] %>% dplyr::select(.node_index__)
    if (nrow(coords_i) != nrow(nodes_i)) stop("Internal error: coords length != node count.")
    ly_i <- dplyr::bind_cols(nodes_i, coords_i); ly_i$group <- mod_levels[i]
    ly_list[[i]] <- ly_i
  }

  ly <- dplyr::bind_rows(ly_list)
  if (theta_shift != 0) {
    Rm <- matrix(c(cos(theta_shift), -sin(theta_shift), sin(theta_shift), cos(theta_shift)), nrow = 2)
    xy <- as.matrix(ly[, c("x", "y")]); ly[, c("x", "y")] <- t(Rm %*% t(xy))
  }
  if (isTRUE(scale)) {
    rescale01 <- function(v) {
      rng <- range(v, na.rm = TRUE)
      if (diff(rng) == 0) return(rep(0, length(v)))
      (v - rng[1]) / diff(rng)
    }
    ly$x <- rescale01(ly$x) * 2 - 1; ly$y <- rescale01(ly$y) * 2 - 1
  }
  ly <- ly %>% dplyr::arrange(.node_index__) %>% dplyr::select(x, y, group)
  rownames(ly) <- NULL
  ly
}
