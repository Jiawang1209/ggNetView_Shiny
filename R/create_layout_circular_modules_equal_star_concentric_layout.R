#' @noRd
create_layout_circular_modules_equal_star_concentric_layout <- function(
    graph_obj,
    r = 0.1,
    node_add = 7,
    anchor_dist = 10,
    scale = TRUE,
    inner_ratio = 0.45,
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

  star_vertices <- function(Ro, Ri) {
    ang0 <- pi / 2
    angs <- ang0 + (0:9) * (pi / 5)
    rad <- ifelse((0:9) %% 2 == 0, Ro, Ri)
    x <- rad * cos(angs)
    y <- rad * sin(angs)
    cbind(x, y)
  }

  map_s_to_star <- function(s, Ro, Ri) {
    V <- star_vertices(Ro, Ri)
    V2 <- V[c(2:10, 1), , drop = FALSE]
    seglen <- sqrt(rowSums((V2 - V)^2))
    P <- sum(seglen)
    if (P == 0) return(cbind(0, 0))
    s <- s %% P
    cum <- c(0, cumsum(seglen))
    idx <- findInterval(s, cum, rightmost.closed = FALSE)
    idx[idx < 1] <- 1
    idx[idx > 10] <- 10
    s0 <- cum[idx]; s1 <- cum[idx + 1]
    w <- (s - s0) / pmax(s1 - s0, .Machine$double.eps)
    P0 <- V[idx, , drop = FALSE]
    P1 <- V2[idx, , drop = FALSE]
    xy <- P0 + (P1 - P0) * w
    xy
  }

  n_circle_vec <- purrr::map_int(n_vec, ~ length(circle_layout(.x, node_add)))
  n_circle_max <- max(n_circle_vec)
  R_max <- (n_circle_max - 1) * r

  n_vec_node <- purrr::map(n_vec, ~ data.frame(
    number_circle = seq_along(circle_layout(.x, node_add)),
    number_node = circle_layout(.x, node_add)
  ))

  star_concentric_from_anchor <- function(cx, cy, info_df, r_step) {
    ly <- data.frame(x = cx, y = cy)
    if (nrow(info_df) >= 2) {
      for (row in 2:nrow(info_df)) {
        m <- info_df$number_node[row]
        Ro <- (row - 1) * r_step
        Ri <- Ro * inner_ratio
        if (m <= 0 || Ro <= 0) next
        Vtmp <- star_vertices(Ro, Ri)
        seg <- sqrt(rowSums((Vtmp[c(2:10, 1), ] - Vtmp)^2))
        P <- sum(seg)
        s_mid <- ((0:(m - 1)) + 0.5) / m * P
        xy <- map_s_to_star(s_mid, Ro, Ri)
        ly <- dplyr::bind_rows(ly, data.frame(x = xy[, 1] + cx, y = xy[, 2] + cy))
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
    coords_i <- star_concentric_from_anchor(cx, cy, info_df, r_step = r_step_i)
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
