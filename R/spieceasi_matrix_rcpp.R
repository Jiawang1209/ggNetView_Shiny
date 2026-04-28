# SPIEC-EASI Rcpp implementation
# Uses spieceasi_norm_cpp for fast CLR; rest mirrors spieceasi_matrix.R

#' @useDynLib ggNetView, .registration = TRUE

# Local helpers (mirror spieceasi_matrix.R)
sparseiCov_local <- function(data, method, npn = FALSE, verbose = FALSE, cov.output = TRUE, ...) {
  if (npn) {
    data <- huge::huge.npn(data, verbose = verbose)
  }
  args <- list(...)
  method <- switch(method, glasso = "glasso", mb = "mb", stop("method not supported"))
  if (is.null(args$lambda.min.ratio)) {
    args$lambda.min.ratio <- 1e-3
  }
  do.call(
    huge::huge,
    c(args, list(x = data, method = method, verbose = verbose, cov.output = cov.output))
  )
}

check_stars_params_local <- function(args = list()) {
  if (!inherits(args, "list") || (length(args) > 0 && is.null(names(args))) || any("" %in% names(args))) {
    stop("stars.params must be a named list")
  }
  allowed <- c("rep.num", "stars.thresh", "stars.subsample.ratio", "thresh")
  if (length(args) > 0) {
    extrargs <- setdiff(names(args), allowed)
    if (length(extrargs) > 0) {
      stop(sprintf("unrecognized arguments to 'stars.params': %s", paste(extrargs, collapse = ", ")))
    }
  }
  TRUE
}

get_opt_index_local <- function(fit) {
  if (!is.null(fit$select) && !is.null(fit$select$stars$opt.index)) {
    fit$select$stars$opt.index
  } else {
    length(fit$est$path)
  }
}

get_refit_local <- function(fit) {
  Matrix::drop0(fit$refit$stars)
}

get_opt_cov_local <- function(fit) {
  idx <- get_opt_index_local(fit)
  Matrix::drop0(fit$est$cov[[idx]])
}

get_opt_icov_local <- function(fit) {
  idx <- get_opt_index_local(fit)
  Matrix::drop0(fit$est$icov[[idx]])
}

get_opt_beta_local <- function(fit) {
  idx <- get_opt_index_local(fit)
  Matrix::drop0(fit$est$beta[[idx]])
}

symBeta_local <- function(beta, mode = "maxabs") {
  t_beta <- Matrix::t
  if (nrow(beta) != ncol(beta)) {
    stop("expecting a square matrix")
  }
  if (mode == "ave") {
    symbeta <- (beta + t_beta(beta)) / 2
  } else if (mode == "maxabs") {
    upt <- Matrix::triu(beta)
    lot <- t_beta(Matrix::tril(beta))
    suppressMessages(maxt <- pmax(abs(upt), abs(lot)))
    uptind <- Matrix::which(maxt == abs(upt))
    lotind <- Matrix::which(maxt == abs(lot))
    if (length(uptind) != 0) {
      maxt[uptind] <- maxt[uptind] * sign(upt[uptind])
    }
    if (length(lotind) != 0) {
      maxt[lotind] <- maxt[lotind] * sign(lot[lotind])
    }
    symbeta <- maxt + t_beta(maxt)
  } else {
    stop("mode not recognized")
  }
  as(symbeta, "symmetricMatrix")
}

precision_to_partial_corr <- function(theta) {
  theta <- as.matrix(theta)
  d <- sqrt(diag(theta))
  pcor <- -theta / outer(d, d)
  diag(pcor) <- 1
  storage.mode(pcor) <- "double"
  pcor
}


#' SPIEC-EASI fit (Rcpp-accelerated CLR)
#'
#' Same as \code{spieceasi_fit} but uses fast C++ CLR transformation.
#' @keywords internal
spieceasi_fit_rcpp <- function(
  data,
  method = "glasso",
  sel.criterion = "stars",
  verbose = TRUE,
  pulsar.select = TRUE,
  pulsar.params = list(),
  stars.params = list(),
  lambda.log = TRUE,
  ...
) {
  args <- list(...)
  if (!is.matrix(data)) {
    data <- as.matrix(data)
  }
  if (!is.numeric(data)) {
    stop("data must be a numeric matrix")
  }

  if (method %in% c("glasso", "mb")) {
    X <- spieceasi_norm_cpp(data)
    rownames(X) <- colnames(data)
    colnames(X) <- rownames(data)
    args$method <- method
  } else {
    stop("standalone script currently supports only method = 'glasso' or 'mb'")
  }

  if (is.null(args[["lambda"]])) {
    if (is.null(args[["lambda.min.ratio"]])) {
      args$lambda.min.ratio <- 1e-3
    }
    if (is.null(args[["nlambda"]])) {
      args$nlambda <- 20
    }
  }

  stars.params <- c(stars.params, pulsar.params)

  if (!is.null(stars.params[["data"]])) {
    stop("supply data directly to spieceasi_fit_rcpp, not stars.params")
  }
  if (!is.null(stars.params[["criterion"]])) {
    stop("supply sel.criterion directly to spieceasi_fit_rcpp, not stars.params")
  }

  if (isTRUE(pulsar.select)) {
    if (sel.criterion != "stars" && sel.criterion != "bstars") {
      stop("only sel.criterion = 'stars' or 'bstars' supported; use pulsar.select = FALSE for other methods")
    }
    check_stars_params_local(stars.params)
    stars.thresh <- if (!is.null(stars.params[["stars.thresh"]])) stars.params[["stars.thresh"]] else if (!is.null(stars.params[["thresh"]])) stars.params[["thresh"]] else 0.05
    rep.num <- if (!is.null(stars.params[["rep.num"]])) as.integer(stars.params[["rep.num"]]) else 50L
    stars.subsample.ratio <- stars.params[["stars.subsample.ratio"]]

    if (verbose) {
      message("Applying data transformations (Rcpp) and selecting model with StARS (huge)...")
    }
    est <- do.call(
      huge::huge,
      c(list(x = t(X), verbose = verbose, cov.output = TRUE), args)
    )
    select_args <- list(
      est = est,
      criterion = "stars",
      stars.thresh = stars.thresh,
      rep.num = rep.num,
      verbose = verbose
    )
    if (!is.null(stars.subsample.ratio)) {
      select_args$stars.subsample.ratio <- stars.subsample.ratio
    }
    select_obj <- do.call(huge::huge.select, select_args)
    fit <- list(
      est = select_obj,
      refit = list(stars = select_obj$refit),
      select = list(stars = list(opt.index = select_obj$opt.index, merge = select_obj$merge)),
      lambda = select_obj$lambda
    )
    class(fit) <- "standalone_spieceasi_fit"
  } else {
    est <- do.call(sparseiCov_local, c(list(data = t(X)), args))
    fit <- list(
      est = est,
      refit = list(stars = est$path[[length(est$path)]]),
      select = NULL,
      lambda = est$lambda
    )
    class(fit) <- "standalone_spieceasi_fit"
  }

  fit$lambda <- fit$est$lambda
  fit
}


#' SPIEC-EASI matrix (Rcpp implementation)
#'
#' Fast C++-accelerated SPIEC-EASI. Uses Rcpp for CLR transformation;
#' Uses \code{huge} for estimation and \code{huge.select} for StARS model selection. Input: samples x taxa matrix.
#'
#' @param data Numeric matrix. Rows = samples, columns = taxa (ASVs/OTUs).
#' @param method Character. "glasso" or "mb".
#' @param output Character. One of "partial_correlation", "adjacency", "covariance",
#'   "correlation", "precision", "beta", "stability".
#' @param ... Passed to \code{spieceasi_fit_rcpp} (e.g. \code{pulsar.select},
#'   \code{pulsar.params} or \code{stars.params}, \code{verbose}).
#'
#' @return Matrix as requested by \code{output}.
#' @export
#'
#' @examples
#' \dontrun{
#' pcor_mat <- spieceasi_matrix_rcpp(asv_mat, method = "glasso", output = "partial_correlation")
#' adj_mat  <- spieceasi_matrix_rcpp(asv_mat, method = "glasso", output = "adjacency")
#' }
spieceasi_matrix_rcpp <- function(
  data,
  method = "glasso",
  output = c("partial_correlation", "adjacency", "covariance", "correlation", "precision", "beta", "stability"),
  ...
) {
  output <- match.arg(output)
  fit <- spieceasi_fit_rcpp(data = data, method = method, ...)

  if (output == "adjacency") {
    out <- as.matrix(get_refit_local(fit)) * 1
    dimnames(out) <- list(colnames(data), colnames(data))
    return(out)
  }
  if (output == "stability") {
    if (is.null(fit$select)) {
      stop("stability matrix requires pulsar.select = TRUE")
    }
    idx <- get_opt_index_local(fit)
    out <- as.matrix(Matrix::drop0(fit$select$stars$merge[[idx]]))
    dimnames(out) <- list(colnames(data), colnames(data))
    return(out)
  }
  if (method == "glasso") {
    if (output == "covariance") {
      out <- as.matrix(get_opt_cov_local(fit))
      dimnames(out) <- list(colnames(data), colnames(data))
      return(out)
    }
    if (output == "correlation") {
      out <- cov2cor(as.matrix(get_opt_cov_local(fit)))
      dimnames(out) <- list(colnames(data), colnames(data))
      return(out)
    }
    if (output == "precision") {
      out <- as.matrix(get_opt_icov_local(fit))
      dimnames(out) <- list(colnames(data), colnames(data))
      return(out)
    }
    if (output == "partial_correlation") {
      out <- precision_to_partial_corr(get_opt_icov_local(fit))
      dimnames(out) <- list(colnames(data), colnames(data))
      return(out)
    }
  }
  if (method == "mb" && output == "beta") {
    out <- as.matrix(symBeta_local(get_opt_beta_local(fit), mode = "maxabs"))
    dimnames(out) <- list(colnames(data), colnames(data))
    return(out)
  }
  stop("requested output is not supported for this method")
}
