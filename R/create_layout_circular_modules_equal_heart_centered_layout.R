#' @noRd
create_layout_circular_modules_equal_heart_centered_layout <- function(
    graph_obj,
    r = 0.15,
    node_add = 8,
    anchor_dist = 10,
    scale = TRUE,
    y_squash = 1.0,
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

  heart_raw <- function(t) {
    x <- 16 * (sin(t))^3
    y <- 13 * cos(t) - 5 * cos(2*t) - 2 * cos(3*t) - cos(4*t)
    cbind(x, y)
  }

  heart_centered_dense <- function(L, squash = 1.0, dense = 1024) {
    tt <- seq(0, 2*pi, length.out = dense + 1)[-(dense + 1)]
    xy <- heart_raw(tt)
    xy[,2] <- xy[,2] * squash
    xy <- xy * L
    cx <- mean(xy[,1]); cy <- mean(xy[,2])
    xy <- cbind(xy[,1] - cx, xy[,2] - cy)
    xy
  }

  sample_on_heart <- function(L, m, M_full, squash = 1.0) {
    if (m <= 0) return(matrix(numeric(0), ncol = 2))
    if (L == 0) return(matrix(c(0,0), ncol = 2))
    poly <- heart_centered_dense(L, squash = squash, dense = 2048)
    seg <- sqrt(rowSums((poly[c(2:nrow(poly),1), ] - poly)^2))
    s <- c(0, cumsum(seg))
    P <- s[length(s)]
    s_mid_all <- ((0:(M_full - 1)) + 0.5) / M_full * P
    s_target <- s_mid_all[seq_len(m)]
    idx <- findInterval(s_target, s, all.inside = TRUE)
    s0 <- s[idx]; s1 <- s[idx + 1]
    p0 <- poly[idx, , drop = FALSE]
    p1 <- poly[(idx %% nrow(poly)) + 1, , drop = FALSE]
    w <- (s_target - s0) / pmax(s1 - s0, .Machine$double.eps)
    xy <- p0 + (p1 - p0) * w
    xy
  }

  ring_sizes_func <- function(n, na) {
    ring_sizes <- c(1); total <- 1; k <- 2
    while (total < n) {
      add <- na * (k - 1)
      take <- min(add, n - total)
      ring_sizes <- c(ring_sizes, take)
      total <- total + take
      k <- k + 1
    }
    ring_sizes
  }

  n_ring_vec <- purrr::map_int(n_vec, ~ length(ring_sizes_func(.x, node_add)))
  n_ring_max <- max(n_ring_vec)
  R_max <- (n_ring_max - 1) * r

  ly_list <- vector("list", n_mod)
  for (i in seq_len(n_mod)) {
    cx <- anchors[[i]][1]; cy <- anchors[[i]][2]
    n_i <- n_vec[i]
    ring_sizes <- ring_sizes_func(n_i, node_add)
    n_ring_i <- length(ring_sizes)
    r_step_i <- if (n_ring_i <= 1) 0 else R_max / (n_ring_i - 1)

    out <- list()
    out[[1]] <- data.frame(x = cx, y = cy)
    if (n_ring_i >= 2) {
      for (ring in 2:n_ring_i) {
        m <- ring_sizes[ring]
        L <- (ring - 1) * r_step_i
        if (m <= 0) next
        M_full <- node_add * (ring - 1)
        xy <- sample_on_heart(L, m, M_full, squash = y_squash)
        out[[ring]] <- data.frame(x = xy[,1] + cx, y = xy[,2] + cy)
      }
    }
    coords_i <- dplyr::bind_rows(out)
    nodes_i <- module_list[[i]] %>% dplyr::select(.node_index__)
    if (nrow(coords_i) != nrow(nodes_i)) stop("Internal error: coords length != node count.")
    ly_i <- dplyr::bind_cols(nodes_i, coords_i)
    ly_i$group <- mod_levels[i]
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
