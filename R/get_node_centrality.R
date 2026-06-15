#' Compute per-node centralities and attach them to a graph object
#'
#' Computes a panel of standard per-node centrality measures and adds
#' them as new vertex columns on the input `tbl_graph`. Use this for
#' node-importance analysis when you want to rank nodes (or map a
#' centrality to a visual aesthetic in [ggNetView()]) by something more
#' informative than degree alone.
#'
#' Note that [get_network_topology()] reports the same families of
#' metrics but as **network-level summaries** (a single mean / sum per
#' network). `get_node_centrality()` is the per-node counterpart -- it
#' keeps every individual value so you can sort, rank, threshold, or
#' colour by it.
#'
#' @section Available measures:
#' All measures wrap the corresponding `igraph` function:
#' \describe{
#'   \item{`"Betweenness"`}{Number of shortest paths through each node
#'     (`igraph::betweenness`).}
#'   \item{`"Closeness"`}{Inverse mean shortest-path distance from a
#'     node to every other node (`igraph::closeness`, `mode = "all"`).
#'     Returns `NaN` for nodes in their own connected component when
#'     the component is a singleton.}
#'   \item{`"Eigenvector"`}{Eigenvector centrality
#'     (`igraph::eigen_centrality`).}
#'   \item{`"PageRank"`}{Google PageRank score (`igraph::page_rank`).}
#'   \item{`"Hub_score"`}{HITS hub score
#'     (`igraph::hub_score`).}
#'   \item{`"Authority_score"`}{HITS authority score
#'     (`igraph::authority_score`).}
#'   \item{`"Coreness"`}{k-core membership of each vertex
#'     (`igraph::coreness`).}
#'   \item{`"Harmonic"`}{Harmonic centrality
#'     (`igraph::harmonic_centrality`); robust to disconnected graphs
#'     where Closeness becomes ill-defined.}
#' }
#'
#' @param graph_obj A `tbl_graph` produced by any
#'   `build_graph_from_*()` constructor.
#' @param measures Character vector. Which centralities to compute.
#'   Pass `"all"` to compute every supported measure. Defaults to all
#'   eight measures listed in \strong{Available measures}.
#' @param weighted Logical (default `FALSE`). If `TRUE`, the edge
#'   `weight` attribute is used as a distance (`igraph` convention:
#'   higher weight = farther). Because correlation networks have
#'   `weight = |correlation|` -- where higher means *closer* -- the
#'   distance used internally is `1 / weight` so that strongly
#'   correlated pairs count as short paths. Set `FALSE` (default) for
#'   the textbook unweighted versions.
#' @param overwrite Logical (default `TRUE`). If a measure column
#'   already exists on the input graph, controls whether to overwrite
#'   it (silent overwrite when `TRUE`; warning + skip when `FALSE`).
#'
#' @returns A `tbl_graph` whose node table is augmented with one column
#'   per requested measure (using the column names listed in
#'   \strong{Available measures}). Other vertex / edge columns are
#'   preserved verbatim.
#'
#' @seealso [get_network_topology()] for network-level summaries of
#'   the same metrics; [get_node_ivi()] for an integrative
#'   importance score that combines local, semi-local, and global
#'   centralities.
#'
#' @export
#'
#' @examples
#' \donttest{
#' set.seed(1)
#' mat <- matrix(stats::rnorm(40 * 20), nrow = 40, ncol = 20)
#' rownames(mat) <- paste0("feature", seq_len(40))
#' colnames(mat) <- paste0("sample",  seq_len(20))
#' obj <- build_graph_from_mat(
#'   mat = mat, method = "cor", cor.method = "pearson",
#'   proc = "none", r.threshold = 0.3, p.threshold = 0.05
#' )
#'
#' obj_aug <- get_node_centrality(obj)
#' obj_aug %>%
#'   tidygraph::activate(nodes) %>%
#'   tidygraph::as_tibble() %>%
#'   dplyr::arrange(dplyr::desc(Betweenness)) %>%
#'   utils::head(5)
#' }
get_node_centrality <- function(
  graph_obj,
  measures = c("Betweenness", "Closeness", "Eigenvector",
               "PageRank", "Hub_score", "Authority_score",
               "Coreness", "Harmonic"),
  weighted = FALSE,
  overwrite = TRUE
) {

  if (!inherits(graph_obj, "tbl_graph")) {
    stop("`graph_obj` must be a tbl_graph (e.g. produced by build_graph_from_mat()).",
         call. = FALSE)
  }

  all_measures <- c("Betweenness", "Closeness", "Eigenvector",
                    "PageRank", "Hub_score", "Authority_score",
                    "Coreness", "Harmonic")

  # `measures = "all"` shorthand -- expand before validation.
  if (length(measures) == 1L && identical(measures, "all")) {
    measures <- all_measures
  }
  measures <- as.character(measures)
  bad <- setdiff(measures, all_measures)
  if (length(bad) > 0L) {
    stop(sprintf(
      "Unknown measure(s): %s.\n  Available: %s.",
      paste(bad, collapse = ", "),
      paste(all_measures, collapse = ", ")
    ), call. = FALSE)
  }

  ig <- tidygraph::as.igraph(graph_obj)

  if (igraph::vcount(ig) == 0L) {
    stop("`graph_obj` has zero vertices.", call. = FALSE)
  }

  # Edge-weight handling.
  #
  # ggNetView's build_graph_from_*() family stores |correlation| as `weight`
  # (higher = closer / stronger).  igraph's distance-semantic measures
  # (Betweenness, Closeness, Harmonic) expect weights as DISTANCES, so we
  # invert: w_dist = 1 / weight.  Strength-semantic measures (Eigenvector,
  # PageRank, Hub/Authority) expect raw strengths, so we pass w_str = weight
  # directly.  The two weight vectors are computed once and routed to the
  # appropriate igraph call below.
  #
  # M2 guard: non-finite (NA, NaN, Inf) weights bypass the <= 0 check and
  # cause a raw C-level igraph crash.  Catch them here alongside non-positive
  # weights and fall back to unweighted in either case.
  w_dist <- NULL   # distance weights  (1/raw)  for Betweenness/Closeness/Harmonic
  w_str  <- NULL   # strength weights  (raw)     for Eigenvector/PageRank/HITS
  if (isTRUE(weighted)) {
    if ("weight" %in% igraph::edge_attr_names(ig) && igraph::ecount(ig) > 0L) {
      raw_w <- igraph::E(ig)$weight
      if (any(!is.finite(raw_w)) || any(raw_w <= 0, na.rm = TRUE)) {
        warning(
          "`weighted = TRUE` but the graph has non-finite or non-positive ",
          "edge weights; falling back to the unweighted computation.",
          call. = FALSE
        )
        # w_dist and w_str remain NULL → unweighted path below
      } else {
        w_dist <- 1 / raw_w
        w_str  <- raw_w
      }
    } else {
      warning(
        "`weighted = TRUE` but the graph has no `weight` edge attribute; ",
        "falling back to the unweighted computation.",
        call. = FALSE
      )
    }
  }

  # Existing node column names (for the `overwrite = FALSE` branch).
  existing_cols <- graph_obj %>%
    tidygraph::activate(nodes) %>%
    tidygraph::as_tibble() %>%
    colnames()

  values <- list()

  # Distance-semantic measures: use w_dist (= 1/weight), so strongly-correlated
  # pairs count as short paths.
  if ("Betweenness" %in% measures) {
    values$Betweenness <- as.numeric(
      igraph::betweenness(ig, directed = FALSE, weights = w_dist)
    )
  }
  if ("Closeness" %in% measures) {
    closeness_vals <- as.numeric(
      igraph::closeness(ig, mode = "all", weights = w_dist)
    )
    # L4: isolated / singleton-component nodes produce NaN from igraph.
    # Coerce to NA so downstream code can use is.na() reliably.
    if (any(!is.finite(closeness_vals))) {
      warning(
        "The graph is disconnected; Closeness is undefined for nodes that ",
        "cannot reach all others. Non-finite values are coerced to NA.",
        call. = FALSE
      )
      closeness_vals[!is.finite(closeness_vals)] <- NA_real_
    }
    values$Closeness <- closeness_vals
  }

  # Strength-semantic measures: use w_str (= raw |correlation|), so stronger
  # edges confer higher centrality — the correct direction for Eigenvector,
  # PageRank, and HITS on correlation networks.
  if ("Eigenvector" %in% measures) {
    values$Eigenvector <- as.numeric(
      igraph::eigen_centrality(ig, weights = w_str, directed = FALSE)$vector
    )
  }
  if ("PageRank" %in% measures) {
    values$PageRank <- as.numeric(
      igraph::page_rank(ig, weights = w_str, directed = FALSE)$vector
    )
  }
  # ---- HITS scores (Hub + Authority) ---------------------------------------
  # igraph 2.0.3+ deprecated `hub_score()` and `authority_score()` in favour
  # of a single `hits_scores()` returning both. Detect at call time and use
  # the modern API when available; fall back to the deprecated pair on older
  # igraph installs so the wrapper stays version-agnostic.
  need_hub  <- "Hub_score"       %in% measures
  need_auth <- "Authority_score" %in% measures
  if (need_hub || need_auth) {
    has_hits <- "hits_scores" %in% getNamespaceExports("igraph")
    if (has_hits) {
      hits <- igraph::hits_scores(ig, weights = w_str)
      if (need_hub)  values$Hub_score       <- as.numeric(hits$hub)
      if (need_auth) values$Authority_score <- as.numeric(hits$authority)
    } else {
      if (need_hub) {
        values$Hub_score <- as.numeric(
          igraph::hub_score(ig, weights = w_str)$vector
        )
      }
      if (need_auth) {
        values$Authority_score <- as.numeric(
          igraph::authority_score(ig, weights = w_str)$vector
        )
      }
    }
  }
  if ("Coreness" %in% measures) {
    # `igraph::coreness` does not accept a `weights` argument; the
    # k-core decomposition is defined on the unweighted graph.
    values$Coreness <- as.numeric(igraph::coreness(ig))
  }
  if ("Harmonic" %in% measures) {
    # Distance-semantic: use w_dist so harmonic mean of path lengths is correct.
    values$Harmonic <- as.numeric(
      igraph::harmonic_centrality(ig, weights = w_dist)
    )
  }

  # Honour `overwrite = FALSE`: drop measures that would clobber an
  # existing column, with a single grouped warning.
  if (!isTRUE(overwrite)) {
    clash <- intersect(names(values), existing_cols)
    if (length(clash) > 0L) {
      warning(sprintf(
        "Skipping measure(s) that already exist on the graph (set `overwrite = TRUE` to replace): %s",
        paste(clash, collapse = ", ")
      ), call. = FALSE)
      values[clash] <- NULL
    }
  }

  if (length(values) == 0L) {
    return(graph_obj)
  }

  # Splice the named list into a single mutate() call so all measures
  # land on the node table in one pass.
  graph_obj %>%
    tidygraph::activate(nodes) %>%
    tidygraph::mutate(!!!values)
}
