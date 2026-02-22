split_modules <- function(df, universe = NULL) {
  if (!is.null(universe)) {
    df <- df[df$name %in% universe, ]
  }
  split(df$name, df$Modularity)
}


compare_modules_by_overlap <- function(dfA, dfB) {

  # 1. 共同节点全集
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

  # overlap coefficient（比 Jaccard 更稳）
  res$overlap_coef <- res$overlap / pmin(res$sizeA, res$sizeB)

  # 超几何检验
  res$pvalue <- mapply(function(k, m, n) {
    phyper(k - 1, m, length(U) - m, n, lower.tail = FALSE)
  }, res$overlap, res$sizeA, res$sizeB)

  res$FDR <- p.adjust(res$pvalue, method = "BH")

  res
}
