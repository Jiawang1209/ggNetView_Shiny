#' Mantel test utilities for species-environment distance matrix correlation
#'
#' These functions compute Mantel statistics between species and environmental
#' distance matrices. The implementation uses \code{vegan::mantel} and
#' \code{vegan::mantel.partial} directly, with a workflow designed for
#' ggNetView's heatmap-link visualization.
#'
#' @param spec_df Data frame or matrix of species abundances (samples as rows).
#' @param env_df Data frame or matrix of environmental variables (samples as rows).
#' @param method Correlation method: \code{"pearson"}, \code{"spearman"}, or \code{"kendall"}.
#' @param alternative Alternative hypothesis for the test.
#' @param permutations Number of permutations for significance testing.
#' @param na_omit If \code{TRUE}, remove incomplete cases before computing distances.
#' @param spec_dist_method Distance method for species matrix when using
#'   \code{mantel_between_blocks}. One of \code{"euclidean"}, \code{"bray"},
#'   \code{"manhattan"}, etc. (see \code{vegan::vegdist}).
#' @param env_dist_method Distance method for environmental matrix.
#' @param env_ctrl Optional control matrix for partial Mantel (only when
#'   \code{test_type = "mantel.partial"}).
#'
#' @return A data frame with columns \code{ID} (species/block), \code{Type}
#'   (env/block), \code{Correlation} (Mantel r), and \code{Pvalue}.
#'
#' @references
#' Legendre, P. and Legendre, L. (2012) Numerical Ecology. 3rd English Edition.
#' Elsevier.
#'
#' @name mantel_utils
NULL


#' Pairwise Mantel test: one r and p per (species, env) pair
#'
#' For each species column and each environmental column, builds a distance
#' matrix and runs Mantel test. Output format matches \code{psych::corr.test}
#' for drop-in use in \code{gglink_heatmaps}.
#'
#' @rdname mantel_utils
#' @export
mantel_pairwise <- function(spec_df,
                            env_df,
                            method = c("pearson", "spearman", "kendall"),
                            alternative = c("two.sided", "less", "greater"),
                            permutations = 999L,
                            na_omit = TRUE) {
  method <- match.arg(method)
  alternative <- match.arg(alternative)
  spec_df <- as.data.frame(spec_df)
  env_df <- as.data.frame(env_df)

  if (nrow(spec_df) != nrow(env_df)) {
    stop("'spec_df' and 'env_df' must have the same number of rows.", call. = FALSE)
  }

  ids <- colnames(spec_df)
  types <- colnames(env_df)
  grid <- expand.grid(ID = ids, Type = types, stringsAsFactors = FALSE)
  r_vec <- rep(NA_real_, nrow(grid))
  p_vec <- rep(NA_real_, nrow(grid))

  for (i in seq_len(nrow(grid))) {
    s <- spec_df[[grid$ID[i]]]
    e <- env_df[[grid$Type[i]]]
    if (isTRUE(na_omit)) {
      ok <- stats::complete.cases(s, e)
      s <- s[ok]
      e <- e[ok]
    }
    if (length(s) < 3L) next
    d_s <- stats::dist(as.matrix(s))
    d_e <- stats::dist(as.matrix(e))
    m <- vegan::mantel(
      d_s, d_e,
      method = method,
      permutations = permutations
    )
    r_vec[i] <- m$statistic
    p_vec[i] <- m$signif
  }

  data.frame(
    ID = grid$ID,
    Type = grid$Type,
    Correlation = r_vec,
    Pvalue = p_vec,
    stringsAsFactors = FALSE
  )
}


#' Mantel test between spec and env blocks (group-level)
#'
#' Runs Mantel test between each (spec_block, env_block) pair. Each block is
#' a subset of columns. Uses full distance matrices per block. Output format
#' is compatible with downstream processing when blocks are treated as
#' single "species" and "env" units.
#'
#' @param spec Data frame of species abundances.
#' @param env Data frame of environmental variables.
#' @param spec_select Named list of column indices or names for species blocks.
#'   E.g. \code{list(block1 = 1:5, block2 = 6:10)}.
#' @param env_select Named list of column indices or names for env blocks.
#' @param test_type \code{"mantel"} or \code{"mantel.partial"}.
#' @param env_ctrl For \code{test_type = "mantel.partial"}, a data frame of
#'   controlling variables (same rows as spec/env).
#' @param na_omit If \code{TRUE}, remove incomplete cases.
#' @param permutations Number of permutations.
#' @param seed Random seed for reproducibility.
#'
#' @rdname mantel_utils
#' @export
mantel_between_blocks <- function(spec,
                                  env,
                                  spec_select = NULL,
                                  env_select = NULL,
                                  test_type = c("mantel", "mantel.partial"),
                                  env_ctrl = NULL,
                                  method = c("pearson", "spearman", "kendall"),
                                  spec_dist_method = "euclidean",
                                  env_dist_method = "euclidean",
                                  na_omit = TRUE,
                                  permutations = 999L,
                                  seed = NULL) {
  test_type <- match.arg(test_type)
  method <- match.arg(method)
  spec <- as.data.frame(spec)
  env <- as.data.frame(env)

  if (nrow(spec) != nrow(env)) {
    stop("'spec' and 'env' must have the same number of rows.", call. = FALSE)
  }

  if (test_type == "mantel.partial" && is.null(env_ctrl)) {
    stop("'env_ctrl' is required when test_type = 'mantel.partial'.", call. = FALSE)
  }

  if (is.null(spec_select)) {
    spec_select <- list(spec = seq_len(ncol(spec)))
    names(spec_select)[1] <- "spec"
  }
  if (is.null(env_select)) {
    env_select <- as.list(stats::setNames(seq_len(ncol(env)), names(env)))
  }

  if (!is.list(spec_select) || !is.list(env_select)) {
    stop("'spec_select' and 'env_select' must be lists.", call. = FALSE)
  }

  spec_names <- names(spec_select)
  if (is.null(spec_names)) spec_names <- paste0("spec", seq_along(spec_select))
  env_names <- names(env_select)
  if (is.null(env_names)) env_names <- paste0("env", seq_along(env_select))

  grid <- expand.grid(
    spec_block = spec_names,
    env_block = env_names,
    stringsAsFactors = FALSE
  )

  r_vec <- numeric(nrow(grid))
  p_vec <- numeric(nrow(grid))

  for (i in seq_len(nrow(grid))) {
    sb <- spec_select[[grid$spec_block[i]]]
    eb <- env_select[[grid$env_block[i]]]
    spec_sub <- spec[, sb, drop = FALSE]
    env_sub <- env[, eb, drop = FALSE]

    if (isTRUE(na_omit)) {
      ok <- stats::complete.cases(spec_sub) & stats::complete.cases(env_sub)
      if (test_type == "mantel.partial") {
        ok <- ok & stats::complete.cases(env_ctrl)
        env_ctrl_sub <- env_ctrl[ok, , drop = FALSE]
      }
      spec_sub <- spec_sub[ok, , drop = FALSE]
      env_sub <- env_sub[ok, , drop = FALSE]
    }

    if (nrow(spec_sub) < 3L) {
      r_vec[i] <- NA_real_
      p_vec[i] <- NA_real_
      next
    }

    d_spec <- vegan::vegdist(spec_sub, method = spec_dist_method)
    d_env <- vegan::vegdist(env_sub, method = env_dist_method)

    if (!is.null(seed)) set.seed(seed + i)

    if (test_type == "mantel.partial") {
      d_ctrl <- vegan::vegdist(env_ctrl_sub, method = "euclidean")
      m <- vegan::mantel.partial(
        d_spec, d_env, d_ctrl,
        method = method,
        permutations = permutations
      )
    } else {
      m <- vegan::mantel(
        d_spec, d_env,
        method = method,
        permutations = permutations
      )
    }
    r_vec[i] <- m$statistic
    p_vec[i] <- m$signif
  }

  data.frame(
    ID = grid$spec_block,
    Type = grid$env_block,
    Correlation = r_vec,
    Pvalue = p_vec,
    stringsAsFactors = FALSE
  )
}
