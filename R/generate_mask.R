#' @noRd
generateMask_ggnetview <- function(dims,
                                   clusters,
                                   q = 0.88,
                                   expand = 1.02
) {
  # dims: data.frame / tibble with columns x, y (node coordinates)
  # clusters: vector (same length as nrow(dims)) giving cluster / module id for each node
  #

  #   x, y, cluster


  if (is.null(dims) || nrow(dims) == 0L) {
    return(data.frame(
      x = numeric(0),
      y = numeric(0),
      cluster = factor()
    ))
  }

  dims <- as.data.frame(dims)

  if (!all(c("x", "y") %in% colnames(dims))) {
    stop("`dims` must contain numeric columns `x` and `y`.")
  }

  if (length(clusters) != nrow(dims)) {
    stop("`clusters` must have the same length as the number of rows in `dims`.")
  }

  df <- dims
  df$cluster <- clusters


  polys <- lapply(split(df, df$cluster), function(d) {
    d <- stats::na.omit(d[, c("x", "y", "cluster")])


    if (nrow(d) < 5L) {
      return(NULL)
    }


    cx <- mean(d$x)
    cy <- mean(d$y)
    dx <- d$x - cx
    dy <- d$y - cy
    theta <- atan2(dy, dx)                 # [-pi, pi]
    r <- sqrt(dx^2 + dy^2)


    theta[theta < 0] <- theta[theta < 0] + 2 * pi


    n_bins <- max(36L, ceiling(sqrt(nrow(d)) * 6L))
    breaks <- seq(0, 2 * pi, length.out = n_bins + 1L)
    bin_id <- cut(theta, breaks = breaks, include.lowest = TRUE, labels = FALSE)


    theta_bin <- tapply(theta, bin_id, function(x) {
      if (length(x) == 0L) return(NA_real_)
      (min(x) + max(x)) / 2
    })
    r_bin <- tapply(r, bin_id, function(x) {
      if (length(x) == 0L) return(NA_real_)

      stats::quantile(x, probs = q, names = FALSE, type = 7)
    })

    keep <- !(is.na(theta_bin) | is.na(r_bin))
    theta_bin <- as.numeric(theta_bin[keep])
    r_bin <- as.numeric(r_bin[keep])


    if (length(theta_bin) < 8L) {
      return(NULL)
    }


    ord <- order(theta_bin)
    theta_bin <- theta_bin[ord]
    r_bin <- r_bin[ord]

    theta_ext <- c(theta_bin, theta_bin[1] + 2 * pi)
    r_ext <- c(r_bin, r_bin[1])


    n_grid <- max(120L, length(theta_ext) * 5L)
    theta_grid <- seq(from = theta_ext[1], to = theta_ext[length(theta_ext)], length.out = n_grid)
    spline_fit <- stats::smooth.spline(theta_ext, r_ext, spar = 0.5)
    r_smooth <- stats::predict(spline_fit, theta_grid)$y


    r_smooth <- pmax(r_smooth, 0)
    r_smooth <- r_smooth * expand


    x_smooth <- cx + r_smooth * cos(theta_grid)
    y_smooth <- cy + r_smooth * sin(theta_grid)

    hull_smooth <- data.frame(
      x = x_smooth,
      y = y_smooth,
      cluster = d$cluster[1]
    )


    hull_smooth <- rbind(hull_smooth, hull_smooth[1, , drop = FALSE])

    hull_smooth
  })

  polys <- polys[!vapply(polys, is.null, logical(1))]

  if (length(polys) == 0L) {
    return(data.frame(
      x = numeric(0),
      y = numeric(0),
      cluster = factor()
    ))
  }

  mask <- do.call(rbind, polys)


  mask$cluster <- factor(mask$cluster, levels = unique(mask$cluster), ordered = TRUE)

  rownames(mask) <- NULL
  mask
}

