#' Translate TOM matrin to create graph object
#'
#' @param TOM matrix
#' TOM matrix from WGCNA result
#'
#' @param mat matrix
#' matrox to WGCNA analysis
#' @param threshold numeric
#' Absolute-weight cutoff.  Edges with \code{abs(weight) <= threshold}
#' are dropped.  Set to \code{NULL} (default) to skip.
#' @param top_k integer
#' Per-node top-\code{k} nearest-neighbour sparsification.  For each
#' node, only its \code{k} strongest TOM neighbours are eligible to
#' contribute edges.  An undirected edge (i, j) survives if j is in
#' i's top-\code{k} \strong{or} i is in j's top-\code{k}
#' (mutual-kNN \emph{union}, the standard choice for WGCNA
#' visualization).  Set to \code{NULL} (default) to skip.  Typical
#' values for WGCNA plots are 10 - 30.
#'
#' @returns A Data frame contain from, to and weight
#' @export
#'
#' @examples
#' \dontrun{
#' # `TOM` is a topological overlap matrix from WGCNA and `mat` is the
#' # expression matrix used to compute it.
#'
#' # Simple absolute-weight threshold:
#' edge_df <- trans_TOM_in_WGCNA(TOM = TOM, mat = mat, threshold = 0.1)
#'
#' # Per-node top-20 neighbours (recommended for visualization):
#' edge_df <- trans_TOM_in_WGCNA(TOM = TOM, mat = mat, top_k = 20)
#'
#' # Combine: top-20 per node, then drop anything below 0.05 globally.
#' edge_df <- trans_TOM_in_WGCNA(TOM = TOM, mat = mat,
#'                               threshold = 0.05, top_k = 20)
#' head(edge_df)
#' }
trans_TOM_in_WGCNA <- function(TOM, mat, threshold = NULL, top_k = NULL){

  TOM_mat <- as.matrix(TOM)

  # Column names of `mat` are the node ids used downstream (from / to).
  nm <- colnames(mat)
  if (length(nm) != nrow(TOM_mat) || length(nm) != ncol(TOM_mat)) {
    stop("length(colnames(mat)) must equal both nrow(TOM) and ncol(TOM).")
  }
  dimnames(TOM_mat) <- list(nm, nm)

  # Strict upper triangle of TOM.
  #
  # TOM is symmetric by construction (TOM[i,j] == TOM[j,i]) and the
  # diagonal is self-loops, so the strict upper triangle carries the
  # complete, non-redundant edge information.  Using upper.tri() here
  # simultaneously strips:
  #   (a) self-loops on the diagonal, and
  #   (b) the (j,i) duplicate of every (i,j) entry.
  idx <- which(upper.tri(TOM_mat), arr.ind = TRUE)

  edges <- data.frame(
    from   = nm[idx[, 1]],
    to     = nm[idx[, 2]],
    weight = TOM_mat[idx],
    stringsAsFactors = FALSE
  )

  # --- (optional) Absolute-weight threshold ---
  # TOM values live in [0, 1] so abs() is a no-op for canonical WGCNA
  # input; it is kept only for backward compatibility with callers that
  # may pass a signed matrix (e.g. raw correlations).
  if (!is.null(threshold)) {
    mask  <- abs(edges$weight) > threshold
    edges <- edges[mask, , drop = FALSE]
    idx   <- idx[mask, , drop = FALSE]
  }

  # --- (optional) Top-k nearest-neighbour sparsification ---
  # For each node, mark its k strongest TOM neighbours; then keep the
  # undirected edge (i, j) if either endpoint retains the other as a
  # top-k neighbour (union / OR semantics).  This is the standard
  # WGCNA visualization sparsifier -- it preserves hub edges and keeps
  # the network visually connected at small k, which a raw absolute
  # threshold usually cannot do without also keeping a lot of noise.
  if (!is.null(top_k)) {
    n <- nrow(TOM_mat)
    k <- as.integer(top_k)
    if (k < 1L) {
      stop("`top_k` must be >= 1.")
    }
    k <- min(k, n - 1L)

    # Write -Inf on the diagonal so self-loops never enter any row's
    # top-k, even in the degenerate case where the off-diagonal TOM
    # values are all zero.
    TOM_work <- TOM_mat
    diag(TOM_work) <- -Inf

    keep_mat <- matrix(FALSE, nrow = n, ncol = n)
    for (i in seq_len(n)) {
      top_idx <- order(TOM_work[i, ], decreasing = TRUE)[seq_len(k)]
      keep_mat[i, top_idx] <- TRUE
    }
    # Union with its transpose: kept if kNN in EITHER direction.
    keep_mat <- keep_mat | t(keep_mat)

    # `idx` and `edges` are still in sync after the threshold step,
    # so matrix-index keep_mat directly to get one logical per edge.
    kept  <- keep_mat[idx]
    edges <- edges[kept, , drop = FALSE]
  }

  rownames(edges) <- NULL
  edges
}
