

#' @noRd
create_layout_rectangle_outline <- function(
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

  a <- r


  V0 <- c(-a, -a)
  V1 <- c( a, -a)
  V2 <- c(-a,  a)



  edge_points <- function(p1, p2, k){
    if (k <= 0) return(matrix(numeric(0), ncol = 2))
    t <- seq_len(k) / (k + 1)
    cbind(p1[1] + (p2[1]-p1[1]) * t,
          p1[2] + (p2[2]-p1[2]) * t)
  }

  if (n == 1) {
    coords <- matrix(V0, ncol = 2, byrow = TRUE)
  } else if (n == 2) {
    coords <- rbind(V0, V1)
  } else {

    m <- n - 3

    w <- c(1, 1, sqrt(2))
    p <- w / sum(w)
    k_base <- floor(m * p)
    rem <- m - sum(k_base)

    frac <- m * p - k_base
    if (rem > 0) {
      order_idx <- order(frac, decreasing = TRUE)
      k_base[order_idx[seq_len(rem)]] <- k_base[order_idx[seq_len(rem)]] + 1
    }
    k01 <- k_base[1]
    k02 <- k_base[2]
    k12 <- k_base[3]

    X <- rbind(
      V0,
      edge_points(V0, V1, k01), V1,
      edge_points(V1, V2, k12), V2,
      edge_points(V2, V0, k02)
    )
    coords <- X
  }

  coords <- as.data.frame(coords)
  names(coords) <- c("x","y")


  if (theta_shift != 0) {
    Rm <- matrix(c(cos(theta_shift), -sin(theta_shift),
                   sin(theta_shift),  cos(theta_shift)), nrow = 2)
    xy <- as.matrix(coords[, c("x","y")])
    coords[, c("x","y")] <- t(Rm %*% t(xy))
  }


  ly <- data.frame(
    x = coords$x,
    y = coords$y
  )
}
