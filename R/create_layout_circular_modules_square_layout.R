#' @noRd
create_layout_circular_modules_square_layout <- function(
    graph_obj,
    r = 1,
    node_add = 7,
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
    tidygraph::as_tibble()

  mod_levels <- node_df$Modularity %>% droplevels() %>% levels() %>% as.character()
  module_list <- node_df %>% dplyr::group_split(Modularity)
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

  square_param_to_xy <- function(u, a) {
    u <- u %% 1
    s <- 0.25
    x <- numeric(length(u)); y <- numeric(length(u))
    idx0 <- u < s; t0 <- u[idx0] / s; x[idx0] <- -a + 2 * a * t0; y[idx0] <- a
    idx1 <- (u >= s) & (u < 2 * s); t1 <- (u[idx1] - s) / s; x[idx1] <- a; y[idx1] <- a - 2 * a * t1
    idx2 <- (u >= 2 * s) & (u < 3 * s); t2 <- (u[idx2] - 2 * s) / s; x[idx2] <- a - 2 * a * t2; y[idx2] <- -a
    idx3 <- u >= 3 * s; t3 <- (u[idx3] - 3 * s) / s; x[idx3] <- -a; y[idx3] <- -a + 2 * a * t3
    data.frame(x = x, y = y)
  }

  n_vec_node <- purrr::map(n_vec, ~ data.frame(
    number_circle = seq_along(circle_layout(.x, node_add)),
    number_node = circle_layout(.x, node_add)
  ))

  square_from_anchor <- function(cx, cy, info_df, r_step) {
    ly <- data.frame(x = cx, y = cy)
    offset_accum <- 0
    if (nrow(info_df) >= 2) {
      for (index in 2:nrow(info_df)) {
        m <- info_df$number_node[index]
        half_len <- (index - 1) * r_step
        offset_accum <- (offset_accum + 0.5 / m) %% 1
        u <- ((0:(m - 1)) / m + offset_accum) %% 1
        coords <- square_param_to_xy(u, a = half_len)
        coords$x <- coords$x + cx; coords$y <- coords$y + cy
        ly <- dplyr::bind_rows(ly, coords)
      }
    }
    ly
  }

  ly_list <- vector("list", n_mod)
  for (i in seq_len(n_mod)) {
    cx <- anchors[[i]][1]; cy <- anchors[[i]][2]
    ly_i <- square_from_anchor(cx, cy, n_vec_node[[i]], r_step = r)
    ly_i$group <- mod_levels[i]
    ly_list[[i]] <- ly_i
  }

  ly <- dplyr::bind_rows(ly_list)
  if (theta_shift != 0) {
    Rm <- matrix(c(cos(theta_shift), -sin(theta_shift), sin(theta_shift), cos(theta_shift)), nrow = 2)
    xy <- as.matrix(ly[, c("x", "y")]); ly[, c("x", "y")] <- t(Rm %*% t(xy))
  }
  rownames(ly) <- NULL
  ly
}
