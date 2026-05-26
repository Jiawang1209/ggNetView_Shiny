#' @noRd
create_layout_gephi <- function(
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
    tidygraph::as_tibble()


  graph_obj %>%
    tidygraph::activate(edges) %>%
    tidygraph::as_tibble()


  n <- dim(node_df)[1]


  circle_layout <- function(n, node_add){

    counts <- 1
    total <- 1
    i <- 2
    while (total < n) {
      add <- node_add * (i-1)
      if (total + add <= n) {
        counts <- c(counts, add)
        total <- total + add
      }else{

        counts <- c(counts, n-total)
        total <- n
      }
      i <- i + 1
    }
    return(counts)
  }


  layout_df_info <- data.frame(
    number_circle = seq_along(circle_layout(n = n, node_add = node_add)),
    number = circle_layout(n = n, node_add = node_add)
  )
  layout_df_info



  ly <- data.frame(x = 0, y = 0)
  offset <- 0
  prev_n <- layout_df_info$number


  for (index in seq_len(dim(layout_df_info)[1])[-1L]) {
    if (index == 2) {
      # index = 2
      # l <- seq(0, 2*pi, length.out = prev_n[index])
      l <- 2* pi * (0:(prev_n[index]-1)) / prev_n[index]
    }else{

      offset <- pi/prev_n[index] %% (2*pi) + offset
      # l <- offset + seq(0, 2*pi, length.out = prev_n[index])
      l <- offset + (2* pi * (0:(prev_n[index]-1)) / prev_n[index])
    }

    x <- sin(l) * (index-1)*r
    y <- cos(l) * (index-1)*r
    ly_tmp <- data.frame(x = x,
                         y = y)

    ly <- dplyr::bind_rows(ly,ly_tmp)

  }



  if (theta_shift != 0) {
    Rm <- matrix(c(cos(theta_shift), -sin(theta_shift),
                   sin(theta_shift),  cos(theta_shift)), nrow = 2)
    xy <- as.matrix(ly[, c("x","y")])
    ly[, c("x","y")] <- t(Rm %*% t(xy))
  }


  return(ly)
}
