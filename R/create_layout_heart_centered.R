#' @noRd
create_layout_heart_centered <- function(
    graph_obj,
    r = 0.15,
    node_add = 8L,
    orientation = c("up","down","left","right"),
    scale = T,
    anchor_dist = 10,
    angle = 0,
    y_squash = 1.0
){



  orientation <- match.arg(orientation)
  base_angle <- switch(orientation,
                       up = 0, right = -pi/2, down = pi, left = pi/2)
  theta_shift <- base_angle + angle


  node_df <- graph_obj %>%
    tidygraph::activate(nodes) %>%
    tibble::as_tibble()
  n <- nrow(node_df)
  if (n <= 0) return(data.frame(x=numeric(), y=numeric(), ring=integer(), idx=integer()))


  ring_sizes <- c(1L)
  total <- 1L; k <- 2L
  while (total < n) {
    add  <- node_add * (k - 1L)
    take <- min(add, n - total)
    ring_sizes <- c(ring_sizes, take)
    total <- total + take
    k <- k + 1L
  }


  heart_raw <- function(t){
    x <- 16 * (sin(t))^3
    y <- 13 * cos(t) - 5 * cos(2*t) - 2 * cos(3*t) - cos(4*t)
    cbind(x, y)
  }




  heart_centered_dense <- function(L, squash = 1.0, dense = 1024L){
    tt <- seq(0, 2*pi, length.out = dense + 1L)[- (dense + 1L)]
    xy <- heart_raw(tt)
    xy[,2] <- xy[,2] * squash
    xy <- xy * L
    cx <- mean(xy[,1]); cy <- mean(xy[,2])
    xy <- cbind(xy[,1] - cx, xy[,2] - cy)
    xy
  }



  sample_on_heart <- function(L, m, M_full, squash = 1.0){
    if (m <= 0) return(matrix(numeric(0), ncol = 2))
    if (L == 0)  return(matrix(c(0,0), ncol = 2))

    poly <- heart_centered_dense(L, squash = squash, dense = 2048L)

    seg  <- sqrt(rowSums((poly[c(2:nrow(poly),1), ] - poly)^2))
    s    <- c(0, cumsum(seg))
    P    <- s[length(s)]

    s_mid_all <- ((0:(M_full - 1L)) + 0.5) / M_full * P
    s_target  <- s_mid_all[seq_len(m)]


    idx <- findInterval(s_target, s, all.inside = TRUE)

    s0  <- s[idx]; s1 <- s[idx + 1L]
    p0  <- poly[idx, , drop = FALSE]
    p1  <- poly[(idx %% nrow(poly)) + 1L, , drop = FALSE]
    w   <- (s_target - s0) / pmax(s1 - s0, .Machine$double.eps)
    xy  <- p0 + (p1 - p0) * w
    xy
  }

  out <- list()

  out[[1]] <- data.frame(x = 0, y = 0, ring = 1L, idx = 1L)


  if (length(ring_sizes) >= 2) {
    for (ring in 2:length(ring_sizes)) {
      m <- ring_sizes[ring]
      L <- (ring - 1) * r
      if (m <= 0) next
      M_full <- node_add * (ring - 1L)

      xy <- sample_on_heart(L, m, M_full, squash = y_squash)


      if (theta_shift != 0) {
        Rm <- matrix(c(cos(theta_shift), -sin(theta_shift),
                       sin(theta_shift),  cos(theta_shift)), nrow = 2)
        xy <- t(Rm %*% t(xy))
      }

      out[[ring]] <- data.frame(x = xy[,1], y = xy[,2],
                                ring = ring, idx = seq_len(m))
    }
  }

  layout_df <- dplyr::bind_rows(out) %>%
    dplyr::select(x, y)


  # eps <- 1e-12
  # key <- paste(round(layout_df$x/eps), round(layout_df$y/eps))
  # stopifnot(!any(duplicated(key)))

  layout_df
}
