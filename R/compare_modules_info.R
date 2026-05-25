#' @noRd
split_modules <- function(df, universe = NULL) {
  if (!is.null(universe)) {
    df <- df[df$name %in% universe, ]
  }
  split(df$name, df$Modularity)
}


compare_modules_by_overlap <- function(dfA, dfB) {


  U <- intersect(dfA$name, dfB$name)

  modA <- split_modules(dfA, U)
  modB <- split_modules(dfB, U)

  res <- expand.grid(
    modA = names(modA),
    modB = names(modB),
    stringsAsFactors = FALSE
  )

  res$overlap <- mapply(function(a, b) {
    length(intersect(modA[[a]], modB[[b]]))
  }, res$modA, res$modB)

  res$sizeA <- sapply(res$modA, function(x) length(modA[[x]]))
  res$sizeB <- sapply(res$modB, function(x) length(modB[[x]]))


  # Guard against zero-sized modules (otherwise overlap_coef = 0/0 = NaN).
  .min_size <- pmin(res$sizeA, res$sizeB)
  res$overlap_coef <- ifelse(.min_size > 0, res$overlap / .min_size, 0)


  # `phyper` requires non-negative arguments. When sizeA > length(U) (e.g. if
  # the two module sets do not share a universe), `length(U) - m` is negative
  # and phyper returns NaN; in that case the test is undefined -> NA p-value.
  res$pvalue <- mapply(function(k, m, n) {
    if (!is.finite(k) || !is.finite(m) || !is.finite(n) ||
        m < 0 || n < 0 || length(U) - m < 0) {
      return(NA_real_)
    }
    phyper(k - 1, m, length(U) - m, n, lower.tail = FALSE)
  }, res$overlap, res$sizeA, res$sizeB)

  res$FDR <- p.adjust(res$pvalue, method = "BH")

  res
}
