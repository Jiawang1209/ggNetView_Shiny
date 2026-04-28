#' @noRd
create_layout_consensus_module_gephi <- function(
    graph_obj,
    scale = T,
    r = 1,
    anchor_dist = 10,
    node_add = 7,
    orientation = c("up","down","left","right"),
    angle = 0,
    nrow = NULL,
    ncol = NULL){

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

  n_vec <- purrr::map_int(module_list, base::nrow)
  # module size
  n_mod <- length(n_vec)


  if (!is.null(nrow) && !is.null(ncol)) {
    rows <- as.integer(nrow)
    cols <- as.integer(ncol)
  } else if (!is.null(nrow)) {
    rows <- as.integer(nrow)
    cols <- ceiling(n_mod / rows)
  } else if (!is.null(ncol)) {
    cols <- as.integer(ncol)
    rows <- ceiling(n_mod / cols)
  } else {
    rows <- floor(sqrt(n_mod))
    cols <- ceiling(n_mod / rows)
  }

  rows <- max(1L, rows)
  cols <- max(1L, cols)


  idx <- 0:(n_mod - 1)


  row_id <- idx %/% cols


  col_id <- idx %% cols



  anchors <- lapply(seq_len(n_mod), function(i) {
    c(col_id[i] * anchor_dist,
      -row_id[i] * anchor_dist)
  })


  circle_layout <- function(n, node_add) {
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
  }

  n_vec_node <- purrr::map(n_vec, ~{
    data.frame(
      number_circle = seq_along(circle_layout(.x, node_add)),
      number_node   = circle_layout(.x, node_add)
    )
  })


  concentric_from_anchor <- function(cx, cy, info_df, r_step) {

    ly <- data.frame(x = cx, y = cy)
    offset <- 0
    prev_n <- info_df$number_node

    if (nrow(info_df) >= 2) {
      for (index in 2:nrow(info_df)) {
        if (index == 2) {

          l <- 2 * pi * (0:(prev_n[index] - 1)) / prev_n[index]
        } else {

          offset <- (offset + pi / prev_n[index]) %% (2 * pi)
          l <- offset + 2 * pi * (0:(prev_n[index] - 1)) / prev_n[index]
        }

        x <- cx + sin(l) * (index - 1) * r_step
        y <- cy + cos(l) * (index - 1) * r_step
        ly <- dplyr::bind_rows(ly, data.frame(x = x, y = y))
      }
    }
    ly
  }



  ly_list <- vector("list", n_mod)
  for (i in seq_len(n_mod)) {
    cx <- anchors[[i]][1]
    cy <- anchors[[i]][2]
    ly_i <- concentric_from_anchor(cx, cy, n_vec_node[[i]], r_step = r)
    ly_i$group <- mod_levels[i]
    ly_list[[i]] <- ly_i
  }

  ly <- dplyr::bind_rows(ly_list)

  # ---- rotate ----
  if (theta_shift != 0) {
    Rm <- matrix(
      c(cos(theta_shift), -sin(theta_shift),
        sin(theta_shift),  cos(theta_shift)),
      nrow = 2
    )
    xy <- as.matrix(ly[, c("x", "y")])
    ly[, c("x", "y")] <- t(Rm %*% t(xy))
  }

  # reture layout
  rownames(ly) <- NULL
  ly
}
