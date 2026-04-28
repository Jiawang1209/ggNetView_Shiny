#' @noRd
create_layout_multirings <- function(graph_obj,
                                     r = 1,
                                     node_add = 7,
                                     anchor_dist = 0.5,
                                     scale = TRUE,
                                     orientation = c("up","down","left","right"),
                                     angle = 0){
  orientation <- match.arg(orientation)
  base_angle <- switch(
    orientation,
    up    = 0,
    right = -pi/2,
    down  = pi,
    left  = pi/2
  )
  theta_shift <- base_angle + angle



  node_df <- graph_obj %>%
    tidygraph::activate(nodes) %>%
    tidygraph::as_tibble()

  mod_levels <- node_df$Modularity %>%
    droplevels() %>%
    levels() %>%
    as.character()

  module_list <- node_df %>%
    dplyr::group_split(Modularity)


  n_vec <- purrr::map_int(module_list, nrow)
  n_mod <- length(n_vec)


  node_df_stat <- graph_obj %>%
    tidygraph::activate(nodes) %>%
    tidygraph::as_tibble() %>%
    dplyr::group_by(Modularity) %>%
    dplyr::summarise(n = n()) %>%
    dplyr::arrange(n) %>%
    dplyr::mutate(Modularity = droplevels(Modularity)) %>%
    dplyr::mutate(Modularity = as.character(Modularity))

  graph_obj <- graph_obj %>%
    tidygraph::mutate(Modularity = factor(Modularity, levels = node_df_stat$Modularity, ordered = T)) %>%
    tidygraph::arrange(Modularity)

  circle_list <- list()

  r = r

  for (i in 1:dim(node_df_stat)[1]) {

    n_points <- node_df_stat$n[i]
    radius <- r
    center_x <- 0
    center_y <- 0


    angles <- seq(0, 2*pi, length.out = n_points + 1)[-(n_points+1)]


    x <- center_x + radius * cos(angles)
    y <- center_y + radius * sin(angles)



    circle_df <- data.frame(
      id = 1:n_points,
      x = x,
      y = y
    )

    r = r + anchor_dist
    circle_list[[i]] <- circle_df

  }

  circle_df_layout <- do.call(rbind, circle_list)


  layout_manual <- ggraph::create_layout(graph_obj, layout = "circle")


  layout_manual_2 <- layout_manual %>%
    dplyr::mutate(x = circle_df_layout$x,
                  y = circle_df_layout$y)



  ly <- ggraph::create_layout(graph_obj,
                              layout = "manual",
                              x = layout_manual_2$x,
                              y = layout_manual_2$y
                              )




  if (theta_shift != 0) {
    Rm <- matrix(
      c(cos(theta_shift), -sin(theta_shift),
        sin(theta_shift),  cos(theta_shift)),
      nrow = 2
    )
    xy <- as.matrix(ly[, c("x", "y")])
    ly[, c("x", "y")] <- t(Rm %*% t(xy))
  }

  rownames(ly) <- NULL
  ly


}
