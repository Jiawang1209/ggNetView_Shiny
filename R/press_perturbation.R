#' Press-perturbation (sustained-disturbance) analysis
#'
#' Approximates the classic ecological *press perturbation*: it treats the
#' signed correlation matrix as a proxy for the community (interaction)
#' matrix \eqn{A}, adds negative self-regulation on the diagonal, and
#' inverts it to obtain the net-effect matrix
#' \eqn{N = -A^{-1}}. Entry \eqn{N_{ij}} is the long-run net response of
#' node \eqn{i} when node \eqn{j} is held under sustained pressure
#' (continuously elevated or suppressed). This is the closest one can get
#' to a "type 3" dynamical perturbation from a static correlation network.
#'
#' @section Assumptions and honest limits:
#' This is a deliberately approximate, *qualitative* method. Correlation
#' is **not** causation and carries no direction, so the interaction signs
#' and magnitudes are proxies, not measured coefficients. The framework
#' also requires the community matrix to be **dynamically stable** (all
#' eigenvalues of `A` have negative real part); `press_perturbation()`
#' checks this and warns when it fails. Treat the output as a defensible
#' qualitative scenario ("if I keep suppressing taxon A, the community
#' tends to shift this way"), never as a quantitative prediction. For a
#' purely structural read with no stability assumption, use
#' [get_network_perturbation()] or [get_node_influence()].
#'
#' @param graph_obj A `tbl_graph` from any `build_graph_from_*()`
#'   constructor. Ignored if `cor_mat` is supplied.
#' @param cor_mat Optional numeric matrix. A signed correlation /
#'   interaction matrix with matching row/col names, used directly instead
#'   of extracting one from `graph_obj`.
#' @param self_regulation Numeric scalar, or `NULL` (default). The
#'   diagonal of `A` (intraspecific density dependence; must be negative).
#'   When `NULL`, it is set automatically to
#'   `-(max Re eigenvalue(off-diagonal A) + 1)`, which guarantees a stable
#'   matrix. Supply your own (e.g. `-1`) to encode a specific assumption.
#' @param source Optional character vector of node `name`s. When given,
#'   `response` returns only the net effect on every node of pressing
#'   these source node(s).
#'
#' @returns A list with:
#'   \itemize{
#'     \item `net_effect`: the \eqn{N = -A^{-1}} matrix (columns = pressed
#'       node, rows = responding node).
#'     \item `stable`: logical; `TRUE` if `A` is dynamically stable.
#'     \item `eigen_real_max`: largest real part of `A`'s eigenvalues
#'       (must be `< 0` for stability).
#'     \item `self_regulation`: the diagonal value actually used.
#'     \item `response`: if `source` was given, a data frame of the net
#'       response of each node to pressing the source(s); else `NULL`.
#'   }
#'
#' @references
#' Bender EA, Case TJ, Gilpin ME (1984). "Perturbation experiments in
#' community ecology: theory and practice." \emph{Ecology} 65(1):1-13.
#' May RM (1972). "Will a large complex system be stable?"
#' \emph{Nature} 238:413-414.
#' Novak M et al. (2016). "Characterizing species interactions to
#' understand press perturbations." \emph{Annu. Rev. Ecol. Evol. Syst.}
#' 47:409-432.
#'
#' @seealso [get_network_perturbation()], [get_node_influence()].
#'
#' @export
#'
#' @examples
#' \donttest{
#' data(ppi_example)
#' obj <- build_graph_from_df(
#'   df              = ppi_example$ppi,
#'   node_annotation = ppi_example$annotation
#' )
#' pp <- press_perturbation(obj)
#' pp$stable
#' pp$net_effect[1:3, 1:3]
#' }
press_perturbation <- function(
  graph_obj       = NULL,
  cor_mat         = NULL,
  self_regulation = NULL,
  source          = NULL
) {

  # ---- obtain a signed interaction matrix ---------------------------------
  if (is.null(cor_mat)) {
    if (is.null(graph_obj) || !inherits(graph_obj, "tbl_graph")) {
      stop("Provide either `cor_mat` or a `tbl_graph` `graph_obj`.",
           call. = FALSE)
    }
    ig <- tidygraph::as.igraph(graph_obj)
    if (igraph::vcount(ig) == 0L) {
      stop("`graph_obj` has zero vertices.", call. = FALSE)
    }
    vnames <- igraph::V(ig)$name
    if (is.null(vnames)) vnames <- as.character(seq_len(igraph::vcount(ig)))
    attr_use <- if ("correlation" %in% igraph::edge_attr_names(ig)) "correlation" else "weight"
    C <- as.matrix(igraph::as_adjacency_matrix(ig, attr = attr_use, sparse = FALSE))
    dimnames(C) <- list(vnames, vnames)
  } else {
    C <- as.matrix(cor_mat)
    if (is.null(rownames(C)) || is.null(colnames(C))) {
      stop("`cor_mat` must have row and column names.", call. = FALSE)
    }
  }
  C[!is.finite(C)] <- 0
  diag(C) <- 0  # off-diagonal interactions only; diagonal set below

  # ---- self-regulation / stability ----------------------------------------
  ev_off <- eigen(C, symmetric = isSymmetric(C), only.values = TRUE)$values
  max_re_off <- max(Re(ev_off))
  if (is.null(self_regulation)) {
    # diagonal shift A = C + d*I shifts every eigenvalue by d, so this
    # choice forces max Re eigenvalue(A) = -1 (guaranteed stable).
    self_regulation <- -(max_re_off + 1)
  }
  if (self_regulation >= 0) {
    warning("`self_regulation` should be negative (density dependence); ",
            "results may be unstable.", call. = FALSE)
  }

  A <- C
  diag(A) <- self_regulation
  ev_A <- eigen(A, symmetric = isSymmetric(A), only.values = TRUE)$values
  eigen_real_max <- max(Re(ev_A))
  stable <- eigen_real_max < 0
  if (!stable) {
    warning(sprintf(
      "Community matrix is not dynamically stable (max Re eigenvalue = %.3f >= 0). ",
      eigen_real_max),
      "Net-effect estimates are unreliable; try a more negative ",
      "`self_regulation`.", call. = FALSE)
  }

  # ---- net-effect matrix N = -A^{-1} --------------------------------------
  N <- tryCatch(-solve(A), error = function(e) {
    stop("Community matrix is singular and cannot be inverted; ",
         "adjust `self_regulation`.", call. = FALSE)
  })
  dimnames(N) <- dimnames(A)

  response <- NULL
  if (!is.null(source)) {
    bad <- setdiff(source, colnames(N))
    if (length(bad) > 0L) {
      stop(sprintf("Unknown source node(s): %s", paste(bad, collapse = ", ")),
           call. = FALSE)
    }
    resp_vec <- if (length(source) == 1L) N[, source] else rowSums(N[, source, drop = FALSE])
    response <- data.frame(
      name          = rownames(N),
      net_response  = as.numeric(resp_vec),
      row.names = NULL, stringsAsFactors = FALSE
    )
    response <- response[order(-abs(response$net_response)), ]
  }

  list(
    net_effect      = N,
    stable          = stable,
    eigen_real_max  = eigen_real_max,
    self_regulation = self_regulation,
    response        = response
  )
}
