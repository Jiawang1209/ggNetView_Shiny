

#' @noRd
create_layout_diamond_outline <- function(
    graph_obj,
    r = 6,
    node_add = NULL,
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
  if (n == 0) {
    return(tibble::tibble(name = character(), x = numeric(), y = numeric()))
  }


  T0 <- c(0,  r)
  R0 <- c( r, 0)
  B0 <- c(0, -r)
  L0 <- c(-r, 0)


  edge_points <- function(p1, p2, k){
    if (k <= 0) return(matrix(numeric(0), ncol = 2))
    t <- seq_len(k) / (k + 1)
    cbind(p1[1] + (p2[1]-p1[1]) * t,
          p1[2] + (p2[2]-p1[2]) * t)
  }

  if (n <= 4) {

    pts <- switch(n,
                  `1` = rbind(T0),
                  `2` = rbind(T0, R0),
                  `3` = rbind(T0, R0, B0),
                  `4` = rbind(T0, R0, B0, L0)
    )
    coords <- as.data.frame(pts); names(coords) <- c("x","y")
  } else {

    m <- n - 4
    base <- m %/% 4
    rem  <- m %% 4

    counts <- rep(base, 4)
    if (rem >= 1) counts[1] <- counts[1] + 1
    if (rem >= 2) counts[3] <- counts[3] + 1
    if (rem >= 3) counts[2] <- counts[2] + 1


    X <- rbind(
      T0,
      edge_points(T0, R0, counts[1]), R0,
      edge_points(R0, B0, counts[2]), B0,
      edge_points(B0, L0, counts[3]), L0,
      edge_points(L0, T0, counts[4])
    )
    coords <- as.data.frame(X); names(coords) <- c("x","y")
  }


  if (theta_shift != 0) {
    Rm <- matrix(c(cos(theta_shift), -sin(theta_shift),
                   sin(theta_shift),  cos(theta_shift)), nrow = 2)
    xy <- as.matrix(coords[, c("x","y")])
    coords[, c("x","y")] <- t(Rm %*% t(xy))
  }

  tibble::tibble(
    name = if ("name" %in% names(node_df)) node_df$name else as.character(seq_len(nrow(coords))),
    x = coords$x,
    y = coords$y
  )
}
