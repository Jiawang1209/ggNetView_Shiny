generateMask_ggnetview <- function(dims,
                                   clusters,
                                   q = 0.88,       # 半径分位数，控制边界整体“鼓/瘪”
                                   expand = 1.02   # 全局缩放，>1 更外扩，<1 更收缩
) {
  # dims: data.frame / tibble with columns x, y (node coordinates)
  # clusters: vector (same length as nrow(dims)) giving cluster / module id for each node
  #
  # 返回一个 data.frame:
  #   x, y, cluster
  # 其中每一行是某个模块边界多边形上的一个顶点，按顺序连接即可画出外圈。

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

  # 按 cluster 分组，对每一类做“极坐标分桶 + 分位数 + 平滑”，得到更圆润且更贴合点云的包络多边形
  polys <- lapply(split(df, df$cluster), function(d) {
    d <- stats::na.omit(d[, c("x", "y", "cluster")])

    # 至少需要 5 个点才能比较稳定地估计轮廓
    if (nrow(d) < 5L) {
      return(NULL)
    }

    # 1) 以簇的质心为极坐标原点
    cx <- mean(d$x)
    cy <- mean(d$y)
    dx <- d$x - cx
    dy <- d$y - cy
    theta <- atan2(dy, dx)                 # [-pi, pi]
    r <- sqrt(dx^2 + dy^2)

    # 角度展开到 [0, 2*pi)
    theta[theta < 0] <- theta[theta < 0] + 2 * pi

    # 2) 按角度分桶，对每个角度区间取半径的高分位数，得到一个“贴着点云外缘”的粗轮廓
    n_bins <- max(36L, ceiling(sqrt(nrow(d)) * 6L))  # 随点数自适应，最少 36 个方向
    breaks <- seq(0, 2 * pi, length.out = n_bins + 1L)
    bin_id <- cut(theta, breaks = breaks, include.lowest = TRUE, labels = FALSE)

    # 对每一个角度桶求一个代表点（角度中点 + 半径分位数）
    theta_bin <- tapply(theta, bin_id, function(x) {
      if (length(x) == 0L) return(NA_real_)
      (min(x) + max(x)) / 2
    })
    r_bin <- tapply(r, bin_id, function(x) {
      if (length(x) == 0L) return(NA_real_)
      # 用可调的分位数，既控制外扩程度，又能保留一定凹凸
      stats::quantile(x, probs = q, names = FALSE, type = 7)
    })

    keep <- !(is.na(theta_bin) | is.na(r_bin))
    theta_bin <- as.numeric(theta_bin[keep])
    r_bin <- as.numeric(r_bin[keep])

    # 有效桶太少时直接退出
    if (length(theta_bin) < 8L) {
      return(NULL)
    }

    # 3) 按角度排序，并在头尾各补一点，帮助样条闭合
    ord <- order(theta_bin)
    theta_bin <- theta_bin[ord]
    r_bin <- r_bin[ord]

    theta_ext <- c(theta_bin, theta_bin[1] + 2 * pi)
    r_ext <- c(r_bin, r_bin[1])

    # 3) 用平滑样条拟合 r(theta)，在更细的角度网格上插值
    n_grid <- max(120L, length(theta_ext) * 5L)
    theta_grid <- seq(from = theta_ext[1], to = theta_ext[length(theta_ext)], length.out = n_grid)
    spline_fit <- stats::smooth.spline(theta_ext, r_ext, spar = 0.5)
    r_smooth <- stats::predict(spline_fit, theta_grid)$y

    # 4) 对半径做非负约束，并按给定比例整体放大/缩小
    r_smooth <- pmax(r_smooth, 0)
    r_smooth <- r_smooth * expand

    # 5) 转回直角坐标系
    x_smooth <- cx + r_smooth * cos(theta_grid)
    y_smooth <- cy + r_smooth * sin(theta_grid)

    hull_smooth <- data.frame(
      x = x_smooth,
      y = y_smooth,
      cluster = d$cluster[1]
    )

    # 闭合多边形
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

  # 保持 cluster 为有序因子，方便后续按模块顺序映射颜色
  mask$cluster <- factor(mask$cluster, levels = unique(mask$cluster), ordered = TRUE)

  rownames(mask) <- NULL
  mask
}

