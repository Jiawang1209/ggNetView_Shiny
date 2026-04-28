# Standalone SparCC extraction from SpiecEasi.
# Input: samples x taxa count matrix.
# Default output: numeric correlation matrix.

#' @noRd
norm_to_total <- function(x) {
  x / sum(x)
}

clr_vec <- function(x, base = exp(1), tol = .Machine$double.eps) {
  nzero <- (x >= tol)
  log_x <- log(ifelse(nzero, x, 1), base)
  ifelse(nzero, log_x - mean(log_x) / mean(nzero), 0.0)
}

clr_matrix <- function(x, mar = 2, ...) {
  apply(x, mar, clr_vec, ...)
}

cor2cov_local <- function(cor_mat, sds) {
  if (length(sds) != nrow(cor_mat)) {
    stop("inputs are of mismatched dimension")
  }
  cor_mat * sds * rep(sds, each = nrow(cor_mat))
}

norm_diric <- function(x, rep = 1) {
  dmat <- VGAM::rdiric(rep, x + 1)
  norm_to_total(colMeans(dmat))
}

av <- function(data) {
  cov_clr <- cov(clr_matrix(data))
  j <- matrix(1, ncol(data), ncol(data))
  (j %*% diag(diag(cov_clr))) + (diag(diag(cov_clr)) %*% j) - (2 * cov_clr)
}

basis_var <- function(
  T_mat,
  CovMat = matrix(0, nrow(T_mat), ncol(T_mat)),
  M = matrix(1, nrow(T_mat), ncol(T_mat)) + (diag(ncol(T_mat)) * (ncol(T_mat) - 2)),
  excluded = NULL,
  Vmin = 1e-4
) {
  if (!is.null(excluded)) {
    T_mat[excluded] <- 0
  }
  Ti <- matrix(rowSums(T_mat))
  CovVec <- matrix(rowSums(CovMat - diag(diag(CovMat))))
  M.I <- tryCatch(solve(M), error = function(e) MASS::ginv(M))
  Vbase <- M.I %*% (Ti + 2 * CovVec)
  Vbase[Vbase < Vmin] <- Vmin
  list(Vbase = Vbase, M = M)
}

C_from_V <- function(T_mat, Vbase) {
  j <- matrix(1, nrow(T_mat), ncol(T_mat))
  Vdiag <- diag(c(Vbase))
  CovMat <- 0.5 * ((j %*% Vdiag) + (Vdiag %*% j) - T_mat)
  CovMat <- (CovMat + t(CovMat)) / 2
  CorMat <- cov2cor(CovMat)
  CorMat[abs(CorMat) > 1] <- sign(CorMat[abs(CorMat) > 1])
  CovMat <- cor2cov_local(CorMat, sqrt(as.vector(Vbase)))
  list(Cov = CovMat, Cor = CorMat)
}

exclude_pairs <- function(Cor, M, th = 0.1, excluded = NULL) {
  break_flag <- FALSE
  C_temp <- abs(Cor - diag(diag(Cor)))
  if (!is.null(excluded)) {
    C_temp[excluded] <- 0
  }
  exclude <- which(abs(C_temp - max(C_temp)) < .Machine$double.eps * 100)[seq_len(2)]
  if (max(C_temp) > th) {
    i <- na.exclude(arrayInd(exclude, c(nrow(M), ncol(M)))[, 1])
    M[i, i] <- M[i, i] - 1
    excluded_new <- c(excluded, exclude)
  } else {
    excluded_new <- excluded
    break_flag <- TRUE
  }
  list(M = M, excluded = excluded_new, break_flag = break_flag)
}

sparccinner <- function(data.f, T_mat = NULL, iter = 10, th = 0.1) {
  if (is.null(T_mat)) {
    T_mat <- av(data.f)
  }
  res.bv <- basis_var(T_mat)
  Vbase <- res.bv$Vbase
  M <- res.bv$M
  cbase <- C_from_V(T_mat, Vbase)
  Cov <- cbase$Cov
  Cor <- cbase$Cor

  excluded <- NULL
  for (i in seq_len(iter)) {
    res.excl <- exclude_pairs(Cor, M, th, excluded)
    M <- res.excl$M
    excluded <- res.excl$excluded
    if (res.excl$break_flag) {
      break
    }
    res.bv <- basis_var(T_mat, M = M, excluded = excluded)
    Vbase <- res.bv$Vbase
    M <- res.bv$M
    cbase <- C_from_V(T_mat, Vbase)
    Cov <- cbase$Cov
    Cor <- cbase$Cor
  }
  list(Cov = Cov, Cor = Cor, i = i, M = M, excluded = excluded)
}

triu_local <- function(x) {
  x[upper.tri(x)]
}

triu2diag_local <- function(x, diagval = 0) {
  e <- length(x)
  n <- 0.5 * (sqrt(8 * e + 1) + 1)
  mat <- matrix(0, n, n)
  mat[upper.tri(mat)] <- x
  mat <- mat + t(mat)
  diag(mat) <- diagval
  mat
}

sparcc_matrix <- function(data, iter = 20, inner_iter = 10, th = 0.1) {
  if (!is.matrix(data)) {
    data <- as.matrix(data)
  }
  if (!is.numeric(data)) {
    stop("data must be a numeric matrix")
  }
  if (any(data < 0, na.rm = TRUE)) {
    stop("data contains negative values")
  }

  sparccs <- lapply(
    seq_len(iter),
    function(i) {
      sparccinner(t(apply(data, 1, norm_diric)), iter = inner_iter, th = th)
    }
  )
  cors <- array(unlist(lapply(sparccs, function(x) x$Cor)), c(ncol(data), ncol(data), iter))
  corMed <- apply(cors, 1:2, median)
  storage.mode(corMed) <- "double"
  corMed
}

sparcc_pvalue <- function(data, iter = 20, inner_iter = 10, th = 0.1, R = 100, ncpus = 1) {
  if (!requireNamespace("boot", quietly = TRUE)) {
    stop("package 'boot' is required for SparCC p-values")
  }
  if (!is.matrix(data)) {
    data <- as.matrix(data)
  }
  if (!is.numeric(data)) {
    stop("data must be a numeric matrix")
  }

  statisticboot <- function(data, indices) {
    triu_local(sparcc_matrix(data[indices, , drop = FALSE], iter = iter, inner_iter = inner_iter, th = th))
  }
  statisticperm <- function(data, indices) {
    permuted <- apply(data[indices, , drop = FALSE], 2, sample)
    triu_local(sparcc_matrix(permuted, iter = iter, inner_iter = inner_iter, th = th))
  }

  res <- boot::boot(data, statisticboot, R = R, parallel = "multicore", ncpus = ncpus)
  null_av <- boot::boot(data, statisticperm, sim = "permutation", R = R, parallel = "multicore", ncpus = ncpus)

  nparams <- ncol(res$t)
  tmeans <- colMeans(null_av$t)
  niters <- nrow(res$t)
  ind95 <- max(1, round(0.025 * niters)):round(0.975 * niters)
  boot_ord <- apply(res$t, 2, sort)
  boot_ord95 <- boot_ord[ind95, , drop = FALSE]
  outofrange <- vapply(
    seq_along(res$t0),
    function(i) {
      aitvar <- res$t0[i]
      rg <- range(boot_ord95[, i])
      rg[1] > aitvar || rg[2] < aitvar
    },
    logical(1)
  )
  bs_above <- vapply(
    seq_len(nparams),
    function(i) length(which(res$t[, i] > tmeans[i])),
    numeric(1)
  )
  is_above <- bs_above > res$R / 2
  pvals <- ifelse(is_above, 2 * (1 - bs_above / res$R), 2 * bs_above / res$R)
  pvals[pvals > 1] <- 1
  pvals[outofrange] <- NaN

  triu2diag_local(pvals, diagval = 0)
}

# Default usage:
# source("standalone/sparcc_matrix.R")
# cor_mat <- sparcc_matrix(asv_mat)
# p_mat   <- sparcc_pvalue(asv_mat, R = 100)
