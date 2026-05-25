#' @noRd
create_layout_dendrogram <- function(
    graph_obj,
    node_add = NULL,
    r=NULL,
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
    tidygraph::as_tibble()


  graph_obj %>%
    tidygraph::activate(edges) %>%
    tidygraph::as_tibble()

  # NOTE: despite the function name, the underlying ggraph layout is "nicely"
  # rather than a true dendrogram. Kept for backward compatibility; switch to
  # `layout = "dendrogram"` only when the input graph is guaranteed acyclic.
  ly <- ggraph::create_layout(graph_obj, layout = "nicely")
  ly <- as.data.frame(ly) %>% dplyr::select(1,2)




  if (theta_shift != 0) {
    Rm <- matrix(c(cos(theta_shift), -sin(theta_shift),
                   sin(theta_shift),  cos(theta_shift)), nrow = 2)
    xy <- as.matrix(ly[, c("x","y")])
    ly[, c("x","y")] <- t(Rm %*% t(xy))
  }


  return(ly)
}
