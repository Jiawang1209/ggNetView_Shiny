#' @noRd
apply_transform_method <- function(mat, transfrom.method) {
  if (length(transfrom.method) != 1L || is.na(transfrom.method)) {
    stop("`transfrom.method` must be a single non-missing character value.", call. = FALSE)
  }

  rarefied <- NULL

  switch(
    transfrom.method,
    none = mat,
    scale = t(scale(t(mat), scale = TRUE, center = TRUE)),
    center = t(scale(t(mat), scale = FALSE, center = TRUE)),
    log2 = {
      if (any(mat <= -1, na.rm = TRUE)) {
        stop("`log2` transform requires all values to be greater than -1.", call. = FALSE)
      }
      log2(mat + 1)
    },
    log10 = {
      if (any(mat <= -1, na.rm = TRUE)) {
        stop("`log10` transform requires all values to be greater than -1.", call. = FALSE)
      }
      log10(mat + 1)
    },
    ln = {
      if (any(mat <= -1, na.rm = TRUE)) {
        stop("`ln` transform requires all values to be greater than -1.", call. = FALSE)
      }
      log(mat + 1)
    },
    rrarefy = {
      rarefied <- t(vegan::rrarefy(t(mat), min(colSums(mat))))
      rarefied
    },
    rrarefy_relative = {
      rarefied <- t(vegan::rrarefy(t(mat), min(colSums(mat))))
      sweep(rarefied, 2, colSums(rarefied), "/", check.margin = FALSE)
    }
  )
}
