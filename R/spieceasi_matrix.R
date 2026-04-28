# Standalone SPIEC-EASI extraction from SpiecEasi.
# Input: samples x taxa count matrix.
# Default output: numeric partial-correlation matrix for method = "glasso".

#' @noRd
clr_vec <- function(x.f, base = exp(1), tol = .Machine$double.eps) {
  nzero <- (x.f >= tol)
  log_x <- log(ifelse(nzero, x.f, 1), base)
  ifelse(nzero, log_x - mean(log_x) / mean(nzero), 0.0)
}

clr_matrix <- function(x.f, mar = 2, ...) {
  apply(x.f, mar, clr_vec, ...)
}

spieceasi_norm <- function(data) {
  if (!is.matrix(data)) {
    data <- as.matrix(data)
  }
  if (!is.numeric(data)) {
    stop("input data must be a numeric matrix")
  }
  t(clr_matrix(data + 1, 1))
}

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

spieceasi_fit <- function(
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
    X <- spieceasi_norm(data)
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
    stop("supply data directly to spieceasi_fit, not stars.params")
  }
  if (!is.null(stars.params[["criterion"]])) {
    stop("supply sel.criterion directly to spieceasi_fit, not stars.params")
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
      message("Applying data transformations and selecting model with StARS (huge)...")
    }
    est <- do.call(
      huge::huge,
      c(list(x = X, verbose = verbose, cov.output = TRUE), args)
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
    est <- do.call(sparseiCov_local, c(list(data = X), args))
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

spieceasi_matrix <- function(
  data,
  method = "glasso",
  output = c("partial_correlation", "adjacency", "covariance", "correlation", "precision", "beta", "stability"),
  ...
) {
  output <- match.arg(output)
  fit <- spieceasi_fit(data = data, method = method, ...)

  if (output == "adjacency") {
    return(as.matrix(get_refit_local(fit)) * 1)
  }
  if (output == "stability") {
    if (is.null(fit$select)) {
      stop("stability matrix requires pulsar.select = TRUE")
    }
    idx <- get_opt_index_local(fit)
    return(as.matrix(Matrix::drop0(fit$select$stars$merge[[idx]])))
  }
  if (method == "glasso") {
    if (output == "covariance") {
      return(as.matrix(get_opt_cov_local(fit)))
    }
    if (output == "correlation") {
      return(cov2cor(as.matrix(get_opt_cov_local(fit))))
    }
    if (output == "precision") {
      return(as.matrix(get_opt_icov_local(fit)))
    }
    if (output == "partial_correlation") {
      return(precision_to_partial_corr(get_opt_icov_local(fit)))
    }
  }
  if (method == "mb" && output == "beta") {
    return(as.matrix(symBeta_local(get_opt_beta_local(fit), mode = "maxabs")))
  }
  stop("requested output is not supported for this method")
}

spieceasi_pvalue <- function(...) {
  stop(
    "SPIEC-EASI does not provide standard edge-level p-values like correlation testing. ",
    "Use sparcc_pvalue() for SparCC, or use output = 'stability' in spieceasi_matrix() ",
    "to inspect edge-selection stability instead."
  )
}

# Default usage:
# source("standalone/spieceasi_matrix.R")
# pcor_mat <- spieceasi_matrix(asv_mat, method = "glasso", output = "partial_correlation")
# adj_mat  <- spieceasi_matrix(asv_mat, method = "glasso", output = "adjacency")
# stab_mat <- spieceasi_matrix(asv_mat, method = "glasso", output = "stability")
