#' Extract sample-level subgraphs from a graph object
#'
#' For each sample (column in \code{mat}), OTUs whose abundance exceeds
#' \code{min_abundance} are treated as present in that sample. The induced
#' subgraph of \code{graph_obj} on those OTUs is returned. Optionally,
#' multiple selected samples can be combined into a single merged subgraph
#' via union or intersection of their present OTUs.
#'
#' This function is the sample-wise analogue of \code{\link{get_subgraph}},
#' which splits a graph by its \code{Modularity} node attribute. Unlike module
#' membership (which is a partition), an OTU can belong to multiple samples,
#' so a \code{combine} switch is exposed.
#'
#' @param graph_obj A \code{tbl_graph} object from
#'   \code{\link{build_graph_from_mat}} or \code{\link{build_graph_from_df}}.
#'   Its node table must contain a \code{name} column matching
#'   \code{rownames(mat)}.
#' @param mat Numeric matrix. Rows are OTUs / features (must have rownames
#'   matching graph node names); columns are samples (must have colnames as
#'   sample IDs).
#' @param min_abundance Numeric (default = 0). An OTU is considered present in
#'   a sample when \code{mat[OTU, sample] > min_abundance}. The appropriate
#'   value depends on the scale of \code{mat}: e.g. \code{0} for raw or
#'   rarefied counts, \code{0.001} for relative abundance. The function does
#'   not infer data type; the user is responsible for choosing a meaningful
#'   threshold for their data.
#' @param select_sample Character vector (default = \code{NULL}). Sample IDs
#'   to extract into a single merged subgraph. Must be a subset of
#'   \code{colnames(mat)}. When \code{NULL}, only per-sample subgraphs are
#'   returned and \code{sub_graph_select} is \code{NULL}.
#' @param combine Character. One of \code{"union"} (default) or
#'   \code{"intersect"}. Controls how OTUs from the selected samples are
#'   combined when building \code{sub_graph_select}:
#'   \itemize{
#'     \item \code{"union"}: keep OTUs present in any of the selected samples.
#'     \item \code{"intersect"}: keep only OTUs present in all of the
#'       selected samples.
#'   }
#'   Edges are always the induced edges from \code{graph_obj} between the
#'   surviving nodes.
#'
#' @returns A list with three elements:
#' \itemize{
#'   \item \code{sub_graph_all}: named list of per-sample \code{tbl_graph}
#'     subgraphs. Samples with no present OTUs in the graph are dropped from
#'     this list (but are still recorded in \code{stat_sample}).
#'   \item \code{stat_sample}: data frame with columns \code{Sample},
#'     \code{Node}, \code{Edge}, \code{Status}. One row per sample in
#'     \code{mat}, including samples with empty subgraphs.
#'   \item \code{sub_graph_select}: a single merged \code{tbl_graph} of the
#'     nodes combined from \code{select_sample} according to \code{combine},
#'     or \code{NULL} if \code{select_sample} is \code{NULL} or the
#'     combination produces an empty node set. The node table of this graph
#'     carries two additional columns:
#'     \itemize{
#'       \item \code{n_present_samples} (integer): how many of the selected
#'         samples this node appears in.
#'       \item \code{present_in_samples} (character): comma-separated sample
#'         IDs the node appears in, ordered following \code{select_sample}.
#'     }
#' }
#'
#' @export
#'
#' @examples
#' \donttest{
#' data("otu_rare_relative")
#' data("tax_tab")
#' obj <- build_graph_from_mat(
#'   mat              = otu_rare_relative,
#'   transfrom.method = "none",
#'   r.threshold      = 0.7,
#'   p.threshold      = 0.05,
#'   method           = "WGCNA",
#'   cor.method       = "pearson",
#'   proc             = "bonferroni",
#'   module.method    = "Fast_greedy",
#'   node_annotation  = tax_tab,
#'   top_modules      = 15,
#'   seed             = 1115
#' )
#'
#' # All per-sample subgraphs + per-sample stats
#' res <- get_sample_subgraph(graph_obj = obj, mat = otu_rare_relative)
#' head(res$stat_sample)
#'
#' # Merged subgraph for 3 samples, union of present OTUs
#' res_u <- get_sample_subgraph(
#'   graph_obj     = obj,
#'   mat           = otu_rare_relative,
#'   select_sample = colnames(otu_rare_relative)[1:3],
#'   combine       = "union"
#' )
#' res_u$sub_graph_select
#'
#' # Same selection but intersection (core OTUs across the 3 samples)
#' res_i <- get_sample_subgraph(
#'   graph_obj     = obj,
#'   mat           = otu_rare_relative,
#'   select_sample = colnames(otu_rare_relative)[1:3],
#'   combine       = "intersect"
#' )
#' res_i$sub_graph_select
#' }
get_sample_subgraph <- function(graph_obj,
                                mat,
                                min_abundance = 0,
                                select_sample = NULL,
                                combine       = c("union", "intersect")) {

  combine <- match.arg(combine)

  # ---- input validation --------------------------------------------------
  if (!inherits(graph_obj, "tbl_graph")) {
    stop("`graph_obj` must be a `tbl_graph` object.", call. = FALSE)
  }
  if (missing(mat) || is.null(mat)) {
    stop("`mat` is required.", call. = FALSE)
  }
  if (is.data.frame(mat)) {
    mat <- as.matrix(mat)
  }
  if (!is.matrix(mat) || !is.numeric(mat)) {
    stop("`mat` must be a numeric matrix.", call. = FALSE)
  }
  if (is.null(rownames(mat))) {
    stop("`mat` must have rownames (OTU IDs).", call. = FALSE)
  }
  if (is.null(colnames(mat))) {
    stop("`mat` must have colnames (sample IDs).", call. = FALSE)
  }
  if (!is.numeric(min_abundance) || length(min_abundance) != 1L) {
    stop("`min_abundance` must be a single numeric value.", call. = FALSE)
  }

  # soft warning when the default threshold meets transformed data
  if (min_abundance == 0 && any(mat < 0, na.rm = TRUE)) {
    warning(
      "`mat` contains negative values; `min_abundance = 0` may not be ",
      "meaningful on transformed data. Pass an appropriate threshold ",
      "for your data scale.",
      call. = FALSE
    )
  }

  # ---- extract node info from the graph ----------------------------------
  node_df <- graph_obj %>%
    tidygraph::activate(nodes) %>%
    tidygraph::as_tibble()

  if (!"name" %in% colnames(node_df)) {
    stop("`graph_obj` node data must contain a `name` column.",
         call. = FALSE)
  }

  ig         <- tidygraph::as.igraph(graph_obj)
  node_name  <- as.character(node_df$name)
  sample_ids <- colnames(mat)

  if (length(intersect(rownames(mat), node_name)) == 0L) {
    stop("No overlap between `rownames(mat)` and graph node names. ",
         "Check that `mat` corresponds to the matrix used to build ",
         "`graph_obj`.",
         call. = FALSE)
  }

  # validate select_sample early so the user fails fast on typos
  if (!is.null(select_sample)) {
    if (!is.character(select_sample)) {
      select_sample <- as.character(select_sample)
    }
    bad <- setdiff(select_sample, sample_ids)
    if (length(bad) > 0L) {
      stop("The following `select_sample` IDs are not in `colnames(mat)`: ",
           paste(bad, collapse = ", "),
           call. = FALSE)
    }
  }

  # ---- per-sample subgraph extraction ------------------------------------
  # For each sample, find OTUs that are present (above min_abundance) AND
  # also exist in the graph, then induce the subgraph on those OTUs.
  .one_sample <- function(sid) {
    present_otu <- rownames(mat)[mat[, sid] > min_abundance]
    present_otu <- intersect(present_otu, node_name)
    vids        <- which(node_name %in% present_otu)

    if (length(vids) == 0L) {
      return(list(
        sample   = sid,
        subgraph = NULL,
        present  = character(0),
        stat = data.frame(
          Sample = sid,
          Node   = 0L,
          Edge   = 0L,
          Status = "No present OTUs in graph",
          stringsAsFactors = FALSE
        )
      ))
    }

    ig_sub    <- igraph::subgraph(ig, vids)
    graph_sub <- tidygraph::as_tbl_graph(ig_sub)

    list(
      sample   = sid,
      subgraph = graph_sub,
      present  = present_otu,
      stat = data.frame(
        Sample = sid,
        Node   = igraph::vcount(ig_sub),
        Edge   = igraph::ecount(ig_sub),
        Status = "OK",
        stringsAsFactors = FALSE
      )
    )
  }

  sample_res        <- lapply(sample_ids, .one_sample)
  names(sample_res) <- sample_ids

  # named list of per-sample subgraphs (drop empty samples)
  sub_graph_all <- sample_res %>%
    purrr::map("subgraph") %>%
    purrr::compact()

  # per-sample stat table; keep ALL samples (even empty ones)
  stat_sample <- sample_res %>%
    purrr::map("stat") %>%
    dplyr::bind_rows()

  # ---- merged sub_graph_select -------------------------------------------
  sub_graph_select <- NULL

  if (!is.null(select_sample)) {

    # preserve the user's input order so present_in_samples reads in the
    # intended order (e.g. T0,T1,T2 rather than alphabetical)
    presence_sets        <- lapply(select_sample, function(sid) {
      sample_res[[sid]]$present
    })
    names(presence_sets) <- select_sample

    selected_otu <- switch(
      combine,
      union     = Reduce(union,     presence_sets),
      intersect = Reduce(intersect, presence_sets)
    )

    if (length(selected_otu) == 0L) {
      warning(
        "No OTUs satisfy `combine = \"", combine,
        "\"` across the selected samples; `sub_graph_select` is NULL.",
        call. = FALSE
      )
    } else {
      vids_sel  <- which(node_name %in% selected_otu)
      ig_sel    <- igraph::subgraph(ig, vids_sel)
      graph_sel <- tidygraph::as_tbl_graph(ig_sel)

      # per-node sample membership annotation -----------------------------
      # For each surviving node, list which of the SELECTED samples it
      # appears in. Order follows `select_sample`.
      sel_node_df <- graph_sel %>%
        tidygraph::activate(nodes) %>%
        tidygraph::as_tibble()
      sel_names   <- as.character(sel_node_df$name)

      member_list <- lapply(sel_names, function(nm) {
        in_which <- vapply(select_sample,
                           function(sid) nm %in% presence_sets[[sid]],
                           logical(1))
        select_sample[in_which]
      })

      n_present_samples  <- vapply(member_list, length, integer(1))
      present_in_samples <- vapply(member_list,
                                   function(x) paste(x, collapse = ","),
                                   character(1))

      sub_graph_select <- graph_sel %>%
        tidygraph::activate(nodes) %>%
        dplyr::mutate(
          n_present_samples  = n_present_samples,
          present_in_samples = present_in_samples
        )
    }
  }

  list(
    sub_graph_all    = sub_graph_all,
    stat_sample      = stat_sample,
    sub_graph_select = sub_graph_select
  )
}
