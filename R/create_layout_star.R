
#' @noRd
create_layout_star <- function(
    graph_obj,
    node_add = 7,
    r = 0.1,
    inner_ratio = 0.45,
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
  if (n <= 0) return(data.frame(x=numeric(), y=numeric()))


  ring_counts <- (function(n, node_add){
    counts <- 1L; total <- 1L; i <- 2L
    while (total < n) {
      add <- node_add * (i - 1L)
      if (total + add <= n) { counts <- c(counts, add); total <- total + add
      } else { counts <- c(counts, n - total); total <- n }
      i <- i + 1L
    }
    counts
  })(n, node_add)

  info <- data.frame(ring = seq_along(ring_counts), m = ring_counts)


  star_vertices <- function(Ro, Ri){

    ang0 <- pi/2
    angs <- ang0 + (0:9) * (pi/5)
    rad  <- ifelse((0:9) %% 2 == 0, Ro, Ri)
    x <- rad * cos(angs)
    y <- rad * sin(angs)
    cbind(x, y)
  }


  map_s_to_star <- function(s, Ro, Ri){
    V  <- star_vertices(Ro, Ri)        # 10 x 2
    V2 <- V[c(2:10, 1), , drop = FALSE]
    seglen <- sqrt(rowSums((V2 - V)^2))
    P <- sum(seglen)
    if (P == 0) return(cbind(0,0))


    s <- s %% P
    cum <- c(0, cumsum(seglen))

    idx <- findInterval(s, cum, rightmost.closed = FALSE)  # 1..10
    idx[idx < 1]  <- 1L
    idx[idx > 10] <- 10L

    s0 <- cum[idx]
    s1 <- cum[idx + 1L]
    w  <- (s - s0) / pmax(s1 - s0, .Machine$double.eps)

    P0 <- V [idx, , drop = FALSE]
    P1 <- V2[idx, , drop = FALSE]
    xy <- P0 + (P1 - P0) * w
    xy
  }

  out <- list()

  out[[1]] <- data.frame(x = 0, y = 0, ring = 1L, idx = 1L)


  if (nrow(info) >= 2){
    for (row in 2:nrow(info)) {
      ring_i <- info$ring[row]
      m      <- info$m[row]
      Ro     <- (ring_i - 1) * r
      Ri     <- Ro * inner_ratio
      if (m <= 0 || Ro <= 0) next




      Vtmp  <- star_vertices(Ro, Ri)
      seg   <- sqrt(rowSums((Vtmp[c(2:10,1),]-Vtmp)^2))
      P     <- sum(seg)
      s_mid <- ((0:(m-1)) + 0.5) / m * P

      xy <- map_s_to_star(s_mid, Ro, Ri)


      if (theta_shift != 0) {
        Rm <- matrix(c(cos(theta_shift), -sin(theta_shift),
                       sin(theta_shift),  cos(theta_shift)), nrow = 2)
        xy <- t(Rm %*% t(xy))
      }

      out[[ring_i]] <- data.frame(x = xy[,1], y = xy[,2],
                                  ring = ring_i, idx = seq_len(m))
    }
  }

  dplyr::bind_rows(out) %>%
    dplyr::select(x, y)
}
