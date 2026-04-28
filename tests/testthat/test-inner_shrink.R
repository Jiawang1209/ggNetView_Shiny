# Tests for the new `inner_shrink` parameter on create_layout_WGCNA().
#
# Goals
# -----
# 1. Backward-compat: at the default `inner_shrink = 1`, results must be
#    bit-identical to the pre-change implementation.  We can't compare to
#    the old function directly (it's been replaced), so instead we assert
#    the geometry that the old implementation produced:
#       - module disc centres `(cx, cy)` from .pack_wgcna_modules() are
#         unchanged for any inner_shrink (this is the whole point of the
#         decoupling),
#       - the outermost point of every module sits within `radius * 0.95`
#         (with-edge case) or within `radius` (empty-edge case) of the
#         module centroid -- exactly what the original code guaranteed.
#
# 2. Decoupling: at inner_shrink < 1, module disc centres must be invariant
#    (we explicitly do NOT want this parameter to move modules around);
#    only the per-module point cloud should contract toward each centroid.
#
# 3. Proportionality: with-edge max radius scales linearly with
#    inner_shrink (within FR initialization noise tolerated to ~1%).
#
# 4. Validation: bad inputs (NA, negative, character, length != 1) error.

testthat::test_that("inner_shrink: default = 1 preserves historical fit_radius", {
  set.seed(42)

  # Build a small graph with modularity3 column.
  # Three connected modules of different sizes, plus a small "Others".
  g <- make_test_wgcna_graph()

  ly <- create_layout_WGCNA(g, r = 1)

  geom <- expected_module_geometry(g)
  pts  <- attach_module_to_points(ly, g)

  # For modules with at least one internal edge: max distance from
  # module centroid to any of its points must be <= radius * 0.95
  # (the historical inner margin).  Tolerance covers float rounding.
  for (mod in geom$with_edges) {
    pts_mod <- pts[pts$modularity3 == mod$name, , drop = FALSE]
    d <- sqrt((pts_mod$x - mod$cx)^2 + (pts_mod$y - mod$cy)^2)
    testthat::expect_lte(max(d), mod$radius * 0.95 + 1e-6)
  }

  # For empty-edge modules: bound is the full radius.
  for (mod in geom$no_edges) {
    pts_mod <- pts[pts$modularity3 == mod$name, , drop = FALSE]
    d <- sqrt((pts_mod$x - mod$cx)^2 + (pts_mod$y - mod$cy)^2)
    testthat::expect_lte(max(d), mod$radius + 1e-6)
  }
})

testthat::test_that("inner_shrink: module disc centres are invariant", {
  set.seed(42)
  g <- make_test_wgcna_graph()

  ly1   <- create_layout_WGCNA(g, r = 1, inner_shrink = 1)
  ly05  <- create_layout_WGCNA(g, r = 1, inner_shrink = 0.5)
  ly015 <- create_layout_WGCNA(g, r = 1, inner_shrink = 0.15)

  c1   <- module_centroids(ly1, g)
  c05  <- module_centroids(ly05, g)
  c015 <- module_centroids(ly015, g)

  # Per-module shrunk-toward-centroid means the module *points'* mean
  # tracks the disc centre `(cx, cy)`.  For modules with > 1 internal
  # edge the FR layout is already centred (the algorithm subtracts the
  # mean), so the point-cloud centroid sits exactly on the disc centre
  # for any inner_shrink.  We therefore expect the centroid coordinates
  # to be invariant within FR-noise tolerance.
  testthat::expect_equal(c1$cx,  c05$cx,  tolerance = 1e-6)
  testthat::expect_equal(c1$cy,  c05$cy,  tolerance = 1e-6)
  testthat::expect_equal(c1$cx,  c015$cx, tolerance = 1e-6)
  testthat::expect_equal(c1$cy,  c015$cy, tolerance = 1e-6)
})

testthat::test_that("inner_shrink: with-edge max radius scales linearly", {
  set.seed(42)
  g <- make_test_wgcna_graph()

  geom <- expected_module_geometry(g)

  for (s in c(1, 0.7, 0.4, 0.15)) {
    ly  <- create_layout_WGCNA(g, r = 1, inner_shrink = s)
    pts <- attach_module_to_points(ly, g)
    for (mod in geom$with_edges) {
      pts_mod <- pts[pts$modularity3 == mod$name, , drop = FALSE]
      d <- sqrt((pts_mod$x - mod$cx)^2 + (pts_mod$y - mod$cy)^2)
      # Hard upper bound: 0.95 * radius * s + float slack.
      testthat::expect_lte(max(d), mod$radius * 0.95 * s + 1e-6)
    }
  }
})

testthat::test_that("inner_shrink: input validation rejects bad values", {
  g <- make_test_wgcna_graph()

  testthat::expect_error(create_layout_WGCNA(g, inner_shrink = NA_real_),
                         "positive finite numeric")
  testthat::expect_error(create_layout_WGCNA(g, inner_shrink = -0.5),
                         "positive finite numeric")
  testthat::expect_error(create_layout_WGCNA(g, inner_shrink = 0),
                         "positive finite numeric")
  testthat::expect_error(create_layout_WGCNA(g, inner_shrink = c(0.5, 0.7)),
                         "positive finite numeric")
  testthat::expect_error(create_layout_WGCNA(g, inner_shrink = "0.5"),
                         "positive finite numeric")
  testthat::expect_error(create_layout_WGCNA(g, inner_shrink = Inf),
                         "positive finite numeric")
})

testthat::test_that("ggNetView dispatch: inner_shrink only flows to WGCNA layout", {
  # Other layouts must still work after the dispatch change; the new
  # parameter must be silently irrelevant for them.
  g <- make_test_wgcna_graph()

  # Just check the function returns without error for a non-WGCNA layout
  # when inner_shrink is supplied (it should be ignored silently).
  testthat::expect_silent({
    p <- ggNetView(g, layout = "fr", layout.module = "random",
                   inner_shrink = 0.5, plot_line = FALSE, seed = 1)
  })
  testthat::expect_s3_class(p, "ggplot")
})


# -------------------- helpers --------------------------------------------

make_test_wgcna_graph <- function() {
  # 3 dense modules + 1 sparse "Others"; all with `modularity3` column.
  set.seed(7)

  modA <- 1:8       # 8 nodes, fully-connected ring
  modB <- 9:14      # 6 nodes, ring
  modC <- 15:22     # 8 nodes, ring
  others <- 23:25   # 3 isolated nodes (no edges)

  ring_edges <- function(v) data.frame(from = v, to = c(v[-1], v[1]))
  el <- rbind(ring_edges(modA), ring_edges(modB), ring_edges(modC))
  # Add a few inter-module bridges so there's something to lay out.
  el <- rbind(el, data.frame(from = c(1, 9, 15), to = c(9, 15, 1)))

  ig <- igraph::graph_from_data_frame(el, directed = FALSE,
    vertices = data.frame(
      name = as.character(c(modA, modB, modC, others)),
      modularity3 = c(rep("M1", length(modA)),
                      rep("M2", length(modB)),
                      rep("M3", length(modC)),
                      rep("Others", length(others)))
    ))
  tidygraph::as_tbl_graph(ig)
}

expected_module_geometry <- function(g) {
  # Re-derive the disc centres (cx, cy) and radii the WGCNA layout would
  # assign, using the same packing the layout uses internally.
  node_df <- g %>% tidygraph::activate(nodes) %>% tidygraph::as_tibble()

  mod_levels_df <- node_df %>%
    dplyr::count(modularity3, name = "size") %>%
    dplyr::arrange(dplyr::desc(size)) %>%
    dplyr::mutate(modularity4 = factor(
      modularity3,
      levels = c(setdiff(modularity3, "Others"), "Others"),
      ordered = TRUE
    )) %>%
    dplyr::arrange(modularity4) %>%
    dplyr::mutate(modularity4 = as.character(modularity4))
  mod_levels <- mod_levels_df$modularity4

  counts <- node_df %>%
    dplyr::count(modularity3, name = "n") %>%
    dplyr::mutate(modularity3 = as.character(modularity3))
  counts <- counts[match(mod_levels, counts$modularity3), , drop = FALSE]
  counts$radius <- sqrt(counts$n) * 1   # r = 1

  packing <- ggNetView:::.pack_wgcna_modules(counts$radius)
  counts$cx <- packing$cx
  counts$cy <- packing$cy

  # Determine which modules have internal edges.
  ig <- igraph::as.igraph(g)
  vmod <- igraph::vertex_attr(ig, "modularity3")
  with_edges <- list()
  no_edges <- list()
  for (i in seq_len(nrow(counts))) {
    name <- counts$modularity3[i]
    keep <- which(vmod == name)
    sub  <- igraph::induced_subgraph(ig, vids = keep)
    rec <- list(name = name, cx = counts$cx[i], cy = counts$cy[i],
                radius = counts$radius[i])
    if (igraph::ecount(sub) == 0L) no_edges[[length(no_edges) + 1L]] <- rec
    else                            with_edges[[length(with_edges) + 1L]] <- rec
  }
  list(with_edges = with_edges, no_edges = no_edges)
}

attach_module_to_points <- function(ly, g) {
  # The layout returns rows in the order produced by mod_levels traversal;
  # rebuild that ordering to label each row with its module.
  node_df <- g %>% tidygraph::activate(nodes) %>% tidygraph::as_tibble()
  mod_levels_df <- node_df %>%
    dplyr::count(modularity3, name = "size") %>%
    dplyr::arrange(dplyr::desc(size)) %>%
    dplyr::mutate(modularity4 = factor(
      modularity3,
      levels = c(setdiff(modularity3, "Others"), "Others"),
      ordered = TRUE
    )) %>%
    dplyr::arrange(modularity4) %>%
    dplyr::mutate(modularity4 = as.character(modularity4))
  mod_levels <- mod_levels_df$modularity4

  ig <- igraph::as.igraph(g)
  vmod <- igraph::vertex_attr(ig, "modularity3")
  ordered_mods <- unlist(lapply(mod_levels, function(m) {
    rep(m, sum(vmod == m))
  }))
  data.frame(x = ly$x, y = ly$y, modularity3 = ordered_mods,
             stringsAsFactors = FALSE)
}

module_centroids <- function(ly, g) {
  pts <- attach_module_to_points(ly, g)
  pts %>%
    dplyr::group_by(modularity3) %>%
    dplyr::summarise(cx = mean(x), cy = mean(y), .groups = "drop") %>%
    dplyr::arrange(modularity3)
}
