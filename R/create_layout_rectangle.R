#' @noRd
create_layout_rectangle <- function(
    graph_obj,
    node_add = 7,
    r = 0.1,
    ratio = 1.5,
    scale = T,
    anchor_dist = 10,
    orientation = c("up","down","left","right"),
    angle = 0
){


  orientation <- match.arg(orientation)
  base_angle <- switch(orientation,
                       up = 0, right = -pi/2, down = pi, left = pi/2)
  theta_shift <- base_angle + angle



  node_df <- graph_obj %>%
    tidygraph::activate(nodes) %>%
    tibble::as_tibble()
  n <- nrow(node_df)


  ring_counts <- (function(n, node_add){
    counts <- 1
    total  <- 1
    i <- 2
    while (total < n) {
      add <- node_add * (i - 1)
      if (total + add <= n) {
        counts <- c(counts, add)
        total  <- total + add
      } else {
        counts <- c(counts, n - total)
        total  <- n
      }
      i <- i + 1
    }
    counts
  })(n, node_add)

  layout_df_info <- data.frame(
    number_circle = seq_along(ring_counts),
    number = ring_counts
  )



  rectangle_param_to_xy <- function(u, a, b){
    u <- u %% 1

    fa <- a / (a + b)
    fb <- b / (a + b)

    s0 <- fa
    s1 <- fa + fb
    s2 <- fa + fb + fa


    x <- numeric(length(u))
    y <- numeric(length(u))


    idx0 <- u < s0
    if (any(idx0)) {
      t0 <- u[idx0] / fa
      x[idx0] <- -a + 2 * a * t0
      y[idx0] <-  b
    }


    idx1 <- (u >= s0) & (u < s1)
    if (any(idx1)) {
      t1 <- (u[idx1] - s0) / fb
      x[idx1] <-  a
      y[idx1] <-  b - 2 * b * t1
    }


    idx2 <- (u >= s1) & (u < s2)
    if (any(idx2)) {
      t2 <- (u[idx2] - s1) / fa
      x[idx2] <-  a - 2 * a * t2
      y[idx2] <- -b
    }


    idx3 <- (u >= s2)
    if (any(idx3)) {
      t3 <- (u[idx3] - s2) / fb
      x[idx3] <- -a
      y[idx3] <- -b + 2 * b * t3
    }

    data.frame(x = x, y = y)
  }


  ly <- data.frame(x = 0, y = 0)
  offset_accum <- 0

  for (index in seq_len(nrow(layout_df_info))[-1L]) {
    m <- layout_df_info$number[index]

    half_height <- (index - 1) * r
    half_width  <- ratio * half_height


    offset_accum <- (offset_accum + 0.5 / m) %% 1
    u <- ((0:(m - 1)) / m + offset_accum) %% 1

    coords <- rectangle_param_to_xy(u, a = half_width, b = half_height)
    ly <- dplyr::bind_rows(ly, coords)
  }



  if (theta_shift != 0) {
    Rm <- matrix(c(cos(theta_shift), -sin(theta_shift),
                   sin(theta_shift),  cos(theta_shift)), nrow = 2)
    xy <- as.matrix(ly[, c("x","y")])
    ly[, c("x","y")] <- t(Rm %*% t(xy))
  }

  ly
}
