#' Generate cluster masks via KDE + HDR contour
#'
#' Produces one or more closed polygons per cluster that visually enclose the
#' high-density region of the cluster's points. Uses 2D kernel density
#' estimation followed by a Highest-Density-Region (HDR) contour: the boundary
#' is drawn at the density level whose iso-density region contains a fraction
#' `q` of the cluster's probability mass.
#'
#' Small clusters (n < 10) automatically fall back to a convex hull, since
#' KDE bandwidth selection is unreliable for very few points.
#'
#' @param dims data.frame / tibble with numeric columns `x` and `y`
#'        (node coordinates).
#' @param clusters Vector (same length as `nrow(dims)`) giving the cluster /
#'        module id for each node.
#' @param q HDR coverage. The contour is drawn at the density level whose
#'        iso-density region contains a fraction `q` of the empirical
#'        probability mass. Same semantic as the previous polar-quantile
#'        `q_outer`: larger `q` -> wider mask; smaller `q` -> tighter mask.
#' @param expand Multiplicative scaling applied to each polygon from its own
#'        centroid. Same semantic as the previous polar `expand_outer`.
#' @param bandwidth_scale Multiplier on the robust normal-reference KDE
#'        bandwidth. > 1 produces smoother / wider contours, < 1 produces
#'        tighter / more wiggly ones.
#' @param n_grid Resolution of the KDE grid along each axis. 100 is usually
#'        sufficient; raise for finer contours, lower for speed.
#' @param min_polygon_points Minimum number of points a contour piece must
#'        have to be kept. Used to drop degenerate tiny artifacts.
#'
#' @return A data.frame with columns `x`, `y`, `cluster`, `polygon_id`.
#'         A single cluster may produce multiple rows of `polygon_id` if its
#'         HDR contour has multiple disconnected components.
#'
#' @noRd
generateMask_ggnetview <- function(dims,
                                   clusters,
                                   q = 0.88,
                                   expand = 1.02,
                                   bandwidth_scale = 1.0,
                                   n_grid = 100L,
                                   min_polygon_points = 4L) {

  if (is.null(dims) || nrow(dims) == 0L) {
    return(.empty_mask_table())
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
    n <- nrow(d)

    # Too small to draw anything meaningful
    if (n < 5L) return(NULL)

    # For small modules, KDE bandwidth would be unreliable -> convex-hull fallback
    if (n < 10L) {
      return(.mask_convex_hull(d, expand))
    }

    # Try KDE/HDR; fall back to convex hull on any failure (e.g. colinear points)
    res <- tryCatch(
      .mask_kde_hdr(
        d,
        q                  = q,
        expand             = expand,
        bandwidth_scale    = bandwidth_scale,
        n_grid             = n_grid,
        min_polygon_points = min_polygon_points
      ),
      error = function(e) NULL
    )

    if (is.null(res) || nrow(res) < 4L) {
      res <- .mask_convex_hull(d, expand)
    }

    res
  })

  polys <- polys[!vapply(polys, is.null, logical(1))]

  if (length(polys) == 0L) {
    return(.empty_mask_table())
  }

  mask <- do.call(rbind, polys)
  mask$cluster <- factor(mask$cluster,
                         levels = unique(mask$cluster),
                         ordered = TRUE)
  rownames(mask) <- NULL
  mask
}


#' Empty mask table with the expected column structure.
#' @noRd
.empty_mask_table <- function() {
  data.frame(
    x          = numeric(0),
    y          = numeric(0),
    cluster    = factor(),
    polygon_id = integer(0)
  )
}


#' KDE + HDR contour for a single cluster.
#' @noRd
.mask_kde_hdr <- function(d,
                          q,
                          expand,
                          bandwidth_scale,
                          n_grid,
                          min_polygon_points) {

  # Robust normal-reference bandwidth with safety floor
  hx <- .robust_bw(d$x) * bandwidth_scale
  hy <- .robust_bw(d$y) * bandwidth_scale
  if (hx <= 0 || !is.finite(hx)) hx <- diff(range(c(d$x, d$y))) * 0.05
  if (hy <= 0 || !is.finite(hy)) hy <- diff(range(c(d$x, d$y))) * 0.05
  # final safety: if all points coincide, give a tiny non-zero bandwidth so
  # kde2d does not crash; the convex-hull fallback below will catch the result.
  if (hx <= 0 || !is.finite(hx)) hx <- 1e-3
  if (hy <= 0 || !is.finite(hy)) hy <- 1e-3

  # Pad the grid limits so the contour is not clipped at the bounding box
  pad_x <- max(hx * 4, diff(range(d$x)) * 0.2)
  pad_y <- max(hy * 4, diff(range(d$y)) * 0.2)
  lims <- c(
    min(d$x) - pad_x, max(d$x) + pad_x,
    min(d$y) - pad_y, max(d$y) + pad_y
  )

  k <- MASS::kde2d(d$x, d$y, h = c(hx, hy), n = n_grid, lims = lims)

  # Density at each data point (bilinear interpolation from the grid)
  z_at_data <- .bilinear_interp(k$x, k$y, k$z, d$x, d$y)

  # HDR threshold: density level c such that fraction of data points with
  # z > c is exactly q -> the iso-density region contains a fraction q of the
  # empirical probability mass.
  threshold <- stats::quantile(z_at_data,
                               probs = 1 - q,
                               names = FALSE,
                               na.rm = TRUE)

  # If every data point evaluated to NA/NaN density (e.g. degenerate KDE), the
  # quantile is NA and `contourLines(level = NA)` would crash. Fall back to the
  # convex-hull path by returning NULL up the call chain.
  if (!is.finite(threshold)) return(NULL)

  contours <- grDevices::contourLines(k$x, k$y, k$z, levels = threshold)
  if (length(contours) == 0L) return(NULL)

  polys <- list()
  pid <- 1L
  for (i in seq_along(contours)) {
    ci <- contours[[i]]
    if (length(ci$x) < min_polygon_points) next   # drop tiny artifacts

    px <- ci$x
    py <- ci$y

    # Close polygon if not already
    if (px[1] != px[length(px)] || py[1] != py[length(py)]) {
      px <- c(px, px[1])
      py <- c(py, py[1])
    }

    # expand: scale each polygon outward from its own centroid
    if (expand != 1.0) {
      cx <- mean(px)
      cy <- mean(py)
      px <- cx + (px - cx) * expand
      py <- cy + (py - cy) * expand
    }

    polys[[length(polys) + 1L]] <- data.frame(
      x          = px,
      y          = py,
      cluster    = d$cluster[1],
      polygon_id = pid
    )
    pid <- pid + 1L
  }

  if (length(polys) == 0L) return(NULL)
  do.call(rbind, polys)
}


#' Convex-hull fallback (for small / degenerate clusters).
#' @noRd
.mask_convex_hull <- function(d, expand) {
  hi <- grDevices::chull(d$x, d$y)
  hx <- d$x[hi]
  hy <- d$y[hi]

  # Close hull
  hx <- c(hx, hx[1])
  hy <- c(hy, hy[1])

  if (expand != 1.0) {
    cx <- mean(hx)
    cy <- mean(hy)
    hx <- cx + (hx - cx) * expand
    hy <- cy + (hy - cy) * expand
  }

  data.frame(
    x          = hx,
    y          = hy,
    cluster    = d$cluster[1],
    polygon_id = 1L
  )
}


#' Robust normal-reference bandwidth (similar to `MASS::bandwidth.nrd`) with
#' safety floors so it never returns 0 / NA on small or degenerate input.
#' @noRd
.robust_bw <- function(x) {
  n <- length(x)
  if (n < 2L) return(0)
  iqr_term <- stats::IQR(x) / 1.34
  sd_term  <- stats::sd(x)
  scale <- if (is.finite(iqr_term) && iqr_term > 0) {
    min(iqr_term, sd_term)
  } else {
    sd_term
  }
  bw <- 1.06 * scale * n ^ (-1 / 5)
  if (!is.finite(bw) || bw <= 0) bw <- diff(range(x, finite = TRUE)) * 0.1
  if (!is.finite(bw) || bw <= 0) bw <- 1e-3
  bw
}


#' Bilinear interpolation of a 2D grid (xg x yg, values zg) at scattered
#' query points (x, y). Used to evaluate KDE density at the data points
#' themselves when computing the HDR threshold.
#' @noRd
.bilinear_interp <- function(xg, yg, zg, x, y) {
  nx <- length(xg)
  ny <- length(yg)

  ix <- findInterval(x, xg, all.inside = TRUE)
  iy <- findInterval(y, yg, all.inside = TRUE)
  ix <- pmax(1L, pmin(nx - 1L, ix))
  iy <- pmax(1L, pmin(ny - 1L, iy))

  tx <- (x - xg[ix]) / (xg[ix + 1L] - xg[ix])
  ty <- (y - yg[iy]) / (yg[iy + 1L] - yg[iy])

  z00 <- zg[cbind(ix,       iy)]
  z10 <- zg[cbind(ix + 1L,  iy)]
  z01 <- zg[cbind(ix,       iy + 1L)]
  z11 <- zg[cbind(ix + 1L,  iy + 1L)]

  z00 * (1 - tx) * (1 - ty) +
    z10 *      tx  * (1 - ty) +
    z01 * (1 - tx) *      ty  +
    z11 *      tx  *      ty
}
