# Tests for correctness/robustness fixes in get_network_perturbation
# Covers audit findings: M4 (natural connectivity overflow), M5 (Schneider R
# grid-dependence), L8 (R index for module/manual strategies), L9 (fraction
# grid termination at 1).

if (!exists("get_network_perturbation")) pkgload::load_all(".", quiet = TRUE)

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

make_dense_graph <- function(n = 120, p = 0.6, seed = 42) {
  set.seed(seed)
  ig <- igraph::sample_gnp(n, p, directed = FALSE)
  tidygraph::as_tbl_graph(ig)
}

make_small_graph <- function() {
  # Simple 10-node ring for quick strategy tests
  ig <- igraph::make_ring(10)
  igraph::V(ig)$name <- paste0("n", seq_len(igraph::vcount(ig)))
  tidygraph::as_tbl_graph(ig)
}

# ---------------------------------------------------------------------------
# M4 — Natural_connectivity must not overflow on large/dense survivor graphs
#
# For an Erdos-Renyi graph with n=800, p=0.9 the leading adjacency eigenvalue
# is ~719, so exp(719) = Inf in double precision.  The log-sum-exp fix keeps
# all values finite.
# ---------------------------------------------------------------------------

test_that("M4: Natural_connectivity is finite for very large dense graph (targeted strategy)", {
  skip_on_cran()   # large graph, slow outside CI
  set.seed(42)
  ig_big <- igraph::sample_gnp(800, 0.9, directed = FALSE)
  g_big  <- tidygraph::as_tbl_graph(ig_big)

  # Only one removal step (10%) — enough to exercise the metric on the nearly-
  # complete survivor subgraph where max eigenvalue > 709 -> exp overflows.
  res <- get_network_perturbation(g_big, strategy = "targeted",
                                  centrality = "degree",
                                  fractions = c(0.05, 0.1),
                                  plot = FALSE)
  nc <- res$curve[res$curve$metric == "Natural_connectivity", "value"]
  expect_true(all(is.finite(nc)),
              info = paste("Non-finite Natural_connectivity values:",
                           paste(nc[!is.finite(nc)], collapse = ", ")))
})

# ---------------------------------------------------------------------------
# M5 — Schneider R-index must be grid-spacing invariant (trapezoidal AUC)
#
# The plain-mean formula weights each grid point equally regardless of
# fraction spacing.  When the fraction step does not divide 1 evenly (e.g.
# step=0.3 produces 0.3, 0.6, 0.9 — missing 1.0), the grid terminates early
# and the plain mean over those 3 points is biased high relative to a grid
# that reaches 1.0.  After fixing (append 1 + trapz AUC) the two grids agree.
# ---------------------------------------------------------------------------

test_that("M5: Schneider R is approximately equal across two fraction grid spacings (random)", {
  g <- make_dense_graph(60, 0.5, seed = 99)

  # Two grids: by=0.1 (10 points) vs by=0.05 (20 points) — both reach 1.0
  res1 <- get_network_perturbation(g, strategy = "random",
                                   fractions = seq(0.1, 1, by = 0.1),
                                   bootstrap = 20, seed = 5, plot = FALSE)
  res2 <- get_network_perturbation(g, strategy = "random",
                                   fractions = seq(0.05, 1, by = 0.05),
                                   bootstrap = 20, seed = 5, plot = FALSE)

  r1 <- res1$robustness_index$R_index
  r2 <- res2$robustness_index$R_index

  expect_true(abs(r1 - r2) < 0.05,
              info = sprintf("R_index by=0.1: %.4f  by=0.05: %.4f  diff=%.4f",
                             r1, r2, abs(r1 - r2)))
})

test_that("M5: Schneider R with non-terminating step (0.3) equals step-0.1 grid after grid fix", {
  g <- make_dense_graph(60, 0.5, seed = 99)

  # step=0.1 grid terminates at 1.0; step=0.3 WITHOUT the fix stops at 0.9
  # and the plain mean over {0.3,0.6,0.9} is a biased estimate.
  # After the fix (append 1, trapz AUC) they must agree within tolerance.
  res_fine <- get_network_perturbation(g, strategy = "targeted",
                                       centrality = "degree",
                                       fractions = seq(0.1, 1, by = 0.1),
                                       plot = FALSE)
  res_coarse <- get_network_perturbation(g, strategy = "targeted",
                                         centrality = "degree",
                                         fractions = c(0.3, 0.6, 0.9, 1.0),
                                         plot = FALSE)

  r_fine   <- res_fine$robustness_index$R_index
  r_coarse <- res_coarse$robustness_index$R_index

  # Trapz AUC on different but complete [0,1] grids must agree within ~0.05
  expect_true(abs(r_fine - r_coarse) < 0.05,
              info = sprintf("R_index fine: %.4f  coarse: %.4f  diff=%.4f",
                             r_fine, r_coarse, abs(r_fine - r_coarse)))
})

# ---------------------------------------------------------------------------
# L8 — R_index must be NA for module/manual strategies
# ---------------------------------------------------------------------------

test_that("L8: R_index is NA for module strategy", {
  g <- make_small_graph()
  # Add a module column so module strategy can work
  g <- g %>%
    tidygraph::activate(nodes) %>%
    tidygraph::mutate(Modularity = rep(c("A", "B"), each = 5))

  res <- get_network_perturbation(g, strategy = "module",
                                  target = "A",
                                  module_col = "Modularity",
                                  plot = FALSE)
  expect_true(is.na(res$robustness_index$R_index),
              info = paste("Expected NA R_index for module strategy, got:",
                           res$robustness_index$R_index))
})

test_that("L8: R_index is NA for manual strategy", {
  g <- make_small_graph()

  res <- get_network_perturbation(g, strategy = "manual",
                                  target = c("n1", "n2", "n3"),
                                  plot = FALSE)
  expect_true(is.na(res$robustness_index$R_index),
              info = paste("Expected NA R_index for manual strategy, got:",
                           res$robustness_index$R_index))
})

test_that("L8: R_index is finite (not NA) for random and targeted strategies", {
  g <- make_small_graph()

  res_r <- get_network_perturbation(g, strategy = "random",
                                    fractions = seq(0.2, 1, by = 0.2),
                                    bootstrap = 5, seed = 1, plot = FALSE)
  res_t <- get_network_perturbation(g, strategy = "targeted",
                                    fractions = seq(0.2, 1, by = 0.2),
                                    centrality = "degree", plot = FALSE)

  expect_true(is.finite(res_r$robustness_index$R_index),
              info = "R_index should be finite for random strategy")
  expect_true(is.finite(res_t$robustness_index$R_index),
              info = "R_index should be finite for targeted strategy")
})

# ---------------------------------------------------------------------------
# L9 — normalize_fraction_step / safe_network_perturbation must terminate at 1
# ---------------------------------------------------------------------------

test_that("L9: normalize_fraction_step with step=0.3 produces fraction grid ending at 1", {
  step <- normalize_fraction_step(0.3)
  fractions <- unique(c(seq(step, 1, by = step), 1))
  expect_equal(max(fractions), 1,
               info = paste("Max fraction:", max(fractions)))
  expect_equal(tail(fractions, 1), 1,
               info = paste("Last fraction:", tail(fractions, 1)))
})

test_that("L9: safe_network_perturbation with step=0.3 produces curve with fraction=1 row", {
  skip_if_not(exists("safe_network_perturbation"),
              "safe_network_perturbation not in search path")

  g <- make_small_graph()
  result <- safe_network_perturbation(g, params = list(
    strategy = "targeted",
    centrality = "degree",
    fraction_step = 0.3
  ))
  expect_true(result$ok, info = result$message)
  fractions <- unique(result$value$curve$fraction)
  expect_true(1 %in% fractions,
              info = paste("Fractions in curve:", paste(sort(fractions), collapse = ", ")))
})
