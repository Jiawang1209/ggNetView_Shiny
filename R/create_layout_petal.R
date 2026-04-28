#' @noRd
create_layout_petal <- function(
    graph_obj,
    node_add = 7,
    r = 0.1,
    scale = T,
    anchor_dist = 10,
    petals = 6,
    amp = 0.35,
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


  ly <- data.frame(x = 0, y = 0)


  for (index in 2:nrow(layout_df_info)) {
    m <- layout_df_info$number[index]
    R_base <- (index - 1) * r

    theta <- 2 * pi * (0:(m - 1)) / m
    radius <- R_base * (1 + amp * cos(petals * theta))


    x <- radius * cos(theta)
    y <- radius * sin(theta)

    ly <- dplyr::bind_rows(ly, data.frame(x = x, y = y))
  }



  if (theta_shift != 0) {
    Rm <- matrix(c(cos(theta_shift), -sin(theta_shift),
                   sin(theta_shift),  cos(theta_shift)), nrow = 2)
    xy <- as.matrix(ly[, c("x","y")])
    ly[, c("x","y")] <- t(Rm %*% t(xy))
  }

  return(ly)
}
