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
#' \strong{NOTE (statistical caveat).} This is the \strong{column-vs-column}
#' Mantel variant: each species column and each env column is reduced to a
#' single-variable distance matrix before the Mantel test. With one variable
#' per side, \code{vegan::mantel} is mathematically close to a (rank)
#' correlation between the two columns and does \strong{not} carry the
#' "community-vs-environment" interpretation that ecology papers usually
#' associate with a Mantel test. For the standard ecological pattern, where
#' a whole species block (community matrix) is tested against each
#' environmental gradient, use \code{\link{mantel_block_vs_col}} instead.
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


#' Block-vs-column Mantel test (ecological standard)
#'
#' Treats the whole \code{spec_df} as a single community matrix: all of its
#' columns together form ONE distance matrix (\code{vegan::vegdist} with
#' \code{spec_dist_method}). For each column of \code{env_df}, that single
#' column is converted into its own distance matrix
#' (\code{vegan::vegdist} with \code{env_dist_method}) and a Mantel test is
#' run between the two distance matrices.
#'
#' This is the \strong{ecologically meaningful} Mantel pattern (also used by
#' linkET / ggcor): "community structure of a spec block vs each
#' environmental gradient". Use this instead of
#' \code{\link{mantel_pairwise}} when you want a Mantel result that carries
#' the standard "community-vs-environment" interpretation.
#'
#' @param spec_df Data frame or matrix; rows = samples, columns = species
#'   (or any community variables). The full matrix is converted into ONE
#'   distance matrix.
#' @param env_df Data frame or matrix; rows = samples, columns = env
#'   variables. Each column is converted into its own distance matrix and
#'   tested separately.
#' @param block_name Character (default \code{"block"}). Value placed in the
#'   \code{ID} column of the result, useful for tagging which block these
#'   rows came from when binding many results together.
#' @param method Correlation method for the Mantel test. One of
#'   \code{"pearson"}, \code{"spearman"}, or \code{"kendall"}.
#' @param spec_dist_method Distance method for the spec matrix
#'   (\code{vegan::vegdist}). Common ecological choices:
#'   \code{"bray"}, \code{"jaccard"}, \code{"euclidean"}.
#' @param env_dist_method Distance method for each env column
#'   (\code{vegan::vegdist}). Default \code{"euclidean"} is the standard
#'   choice for continuous env variables.
#' @param permutations Integer. Number of permutations for the test.
#' @param na_omit If \code{TRUE}, drop incomplete cases jointly across
#'   \code{spec_df} and \code{env_df} before computing distances.
#'
#' @return A data frame with one row per env column. Columns:
#'   \code{ID} (= \code{block_name}), \code{Type} (env column name),
#'   \code{Correlation} (Mantel r), \code{Pvalue} (Mantel p).
#' @rdname mantel_utils
#' @export
mantel_block_vs_col <- function(spec_df,
                                env_df,
                                block_name = "block",
                                method = c("pearson", "spearman", "kendall"),
                                spec_dist_method = "bray",
                                env_dist_method = "euclidean",
                                permutations = 999L,
                                na_omit = TRUE) {
  method <- match.arg(method)
  spec_df <- as.data.frame(spec_df)
  env_df  <- as.data.frame(env_df)

  if (nrow(spec_df) != nrow(env_df)) {
    stop("'spec_df' and 'env_df' must have the same number of rows.", call. = FALSE)
  }

  if (isTRUE(na_omit)) {
    ok <- stats::complete.cases(spec_df) & stats::complete.cases(env_df)
    spec_df <- spec_df[ok, , drop = FALSE]
    env_df  <- env_df[ok, , drop = FALSE]
  }

  env_cols <- colnames(env_df)
  if (nrow(spec_df) < 3L || length(env_cols) < 1L) {
    return(data.frame(
      ID = character(0), Type = character(0),
      Correlation = numeric(0), Pvalue = numeric(0),
      stringsAsFactors = FALSE
    ))
  }

  d_spec <- vegan::vegdist(as.matrix(spec_df), method = spec_dist_method)

  r_vec <- rep(NA_real_, length(env_cols))
  p_vec <- rep(NA_real_, length(env_cols))
  for (k in seq_along(env_cols)) {
    e_mat <- as.matrix(env_df[, k, drop = FALSE])
    d_env <- tryCatch(
      vegan::vegdist(e_mat, method = env_dist_method),
      error = function(e) NULL
    )
    if (is.null(d_env)) next
    m <- vegan::mantel(
      d_spec, d_env,
      method = method,
      permutations = permutations
    )
    r_vec[k] <- m$statistic
    p_vec[k] <- m$signif
  }

  data.frame(
    ID = rep(block_name, length(env_cols)),
    Type = env_cols,
    Correlation = r_vec,
    Pvalue = p_vec,
    stringsAsFactors = FALSE
  )
}


#' Get module-to-OTU membership from a tbl_graph
#'
#' Internal helper used by the modularity-based heatmap function. Returns a
#' named list mapping each module label to its set of OTU/node names.
#'
#' @param graph_obj A \code{tbl_graph} or \code{igraph} with a node
#'   \code{name} attribute and a module column.
#' @param module_col Module column name. If missing, falls back to one of
#'   \code{"Modularity"}, \code{"modularity3"}, \code{"modularity2"}.
#' @param exclude_others If \code{TRUE}, drop nodes labelled \code{"Others"}.
#'
#' @return Named list. Names are module labels, values are character vectors
#'   of OTU/node names.
#' @keywords internal
get_module_members <- function(graph_obj,
                               module_col = "Modularity",
                               exclude_others = TRUE) {
  node_df <- graph_obj %>%
    tidygraph::activate(nodes) %>%
    tidygraph::as_tibble()

  if (!"name" %in% colnames(node_df)) {
    stop("`graph_obj` nodes must have a `name` column.", call. = FALSE)
  }

  mod_candidates <- c("Modularity", "modularity3", "modularity2")
  if (!module_col %in% colnames(node_df)) {
    hit <- mod_candidates[mod_candidates %in% colnames(node_df)]
    module_col <- if (length(hit) > 0) hit[1] else stop("No module column found.", call. = FALSE)
  }

  node_names <- as.character(node_df$name)
  modules    <- as.character(node_df[[module_col]])

  if (isTRUE(exclude_others)) {
    keep <- modules != "Others"
    node_names <- node_names[keep]
    modules    <- modules[keep]
  }

  mod_levels <- unique(modules)
  mod_levels <- mod_levels[mod_levels != "Others"]
  if (length(mod_levels) == 0L) {
    stop("No non-'Others' modules found in graph_obj.", call. = FALSE)
  }

  out <- lapply(mod_levels, function(m) node_names[modules == m])
  names(out) <- mod_levels
  out
}
