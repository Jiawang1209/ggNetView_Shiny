#' @noRd
create_layout_diamond <- function(
    graph_obj,
    node_add = 7,
    scale = T,
    anchor_dist = 10,
    r = 0.1,
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

  n <- dim(node_df)[1]


  # ring_counts <- function(n, node_add){
  #   counts <- 1
  #   total  <- 1
  #   i <- 2
  #   while (total < n) {
  #     add <- node_add * (i - 1)
  #     if (total + add <= n) {
  #       counts <- c(counts, add)
  #       total  <- total + add
  #     } else {
  #       counts <- c(counts, n - total)
  #       total  <- n
  #     }
  #     i <- i + 1
  #   }
  #   return(counts)
  # }



  # layout_df_info <- data.frame(
  #   number_circle = seq_along(ring_counts(n, node_add)),
  #   number = ring_counts(n, node_add)
  # )


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

  layout_df_info


  diamond_param_to_xy <- function(u, radius){

    u <- u %% 1
    seg <- floor(u * 4)
    t   <- (u * 4) - seg

    x <- numeric(length(u))
    y <- numeric(length(u))

    idx0 <- seg == 0
    x[idx0] <-  radius * t[idx0]
    y[idx0] <-  radius * (1 - t[idx0])

    idx1 <- seg == 1
    x[idx1] <-  radius * (1 - t[idx1])
    y[idx1] <- -radius * t[idx1]

    idx2 <- seg == 2
    x[idx2] <- -radius * t[idx2]
    y[idx2] <- -radius * (1 - t[idx2])

    idx3 <- seg == 3
    x[idx3] <- -radius * (1 - t[idx3])
    y[idx3] <-  radius * t[idx3]
    data.frame(x = x, y = y)
  }


  ly <- data.frame(x = 0, y = 0)

  offset_accum <- 0

  for (index in 2:nrow(layout_df_info)) {
    m <- layout_df_info$number[index]
    radius <- (index - 1) * r


    offset_accum <- (offset_accum + 0.5 / m) %% 1
    u <- ( (0:(m-1)) / m + offset_accum ) %% 1
    coords <- diamond_param_to_xy(u, radius)
    ly <- dplyr::bind_rows(ly, coords)
  }



  if (theta_shift != 0) {
    Rm <- matrix(c(cos(theta_shift), -sin(theta_shift),
                   sin(theta_shift),  cos(theta_shift)), nrow = 2)
    xy <- as.matrix(ly[, c("x","y")])
    ly[, c("x","y")] <- t(Rm %*% t(xy))
  }


  return(ly)
}
