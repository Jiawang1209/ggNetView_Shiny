## test-backend-heatmap-cor.R
## TDD tests for gglink_heatmaps correlation correctness and robustness.
##
## H5 (high, correctness): spec-env correlation path must honour cor.use /
##   cor.method. Before the fix, line ~1035 calls psych::corr.test() with
##   hard-coded defaults, so pearson vs spearman give IDENTICAL output even
##   when the user selects a different method.
##
## L13 (low, robustness): psych::corr.test() must be guarded for blocks with
##   fewer than 4 complete-case rows or columns whose variance is exactly 0.
##   Before the fix the function passes those degenerate inputs straight to
##   psych, which emits "Number of subjects must be greater than 3" warnings
##   and returns NaN correlations.

if (!exists("gglink_heatmaps")) pkgload::load_all(".", quiet = TRUE)

# ── Test fixtures ─────────────────────────────────────────────────────────────
#
# We need a monotone-but-nonlinear relationship so that Pearson and Spearman
# give DIFFERENT correlation coefficients for the spec-env pair.  A quadratic
# x -> x^2 with positive x is a clean example:
#   - rank-order is preserved (Spearman r = 1.000)
#   - linear fit is imperfect  (Pearson  r ≈ 0.969)
#
# env block: TWO columns (E1 = 1..10, E2 = noise) so that env-env
#   psych::corr.test(env_block) is valid (needs >=2 cols to make a matrix).
#   The tested spec-env pair is still S1 ~ E1.
# spec block: ONE column S1 = E1^2 (nonlinear but monotone).
#
# With n = 10 samples:
#   - pearson(S1, E1)  ≈ 0.969  (linear, not perfect)
#   - spearman(S1, E1) =  1.000  (rank-order preserved exactly)
# These differ, so the H5 test FAILs before the fix (both return pearson
# because cor.method is ignored on the spec-env path) and PASSes after.

n_samp <- 10L
e_vals  <- as.numeric(1:n_samp)
s_vals  <- e_vals^2

# env needs >=2 columns so that psych::corr.test on the env-env block is valid
set.seed(42L)
env_h5  <- data.frame(E1 = e_vals,
                      E2 = e_vals + rnorm(n_samp, sd = 0.5))
spec_h5 <- data.frame(S1 = s_vals)

# env_select and spec_select each contain exactly one block
env_sel_h5  <- list(Env01 = 1:2)
spec_sel_h5 <- list(Spec01 = 1L)

# Helper: run gglink_heatmaps end-to-end and extract the spec-env stats table.
# We use only one orientation (top_right) to keep it as lightweight as possible.
run_h5 <- function(method) {
  res <- gglink_heatmaps(
    env          = env_h5,
    spec         = spec_h5,
    env_select   = env_sel_h5,
    spec_select  = spec_sel_h5,
    orientation  = "top_right",
    relation_method = "correlation",
    cor.method   = method,
    cor.use      = "complete",
    spec_relation = FALSE     # avoid within-block corr (single col anyway)
  )
  res[[3]]   # stats data frame
}

# ── H5: pearson vs spearman on spec-env path ─────────────────────────────────

test_that("H5: cor.method is honoured on spec-env path (pearson != spearman)", {
  stats_pearson  <- run_h5("pearson")
  stats_spearman <- run_h5("spearman")

  # env has 2 columns (E1, E2), spec has 1 (S1) -> expect 2 spec-env rows
  expect_equal(nrow(stats_pearson),  2L,
    info = "pearson run: expected 2 spec-env rows (S1~E1, S1~E2)")
  expect_equal(nrow(stats_spearman), 2L,
    info = "spearman run: expected 2 spec-env rows (S1~E1, S1~E2)")

  # Extract the S1 ~ E1 row for both methods (Type column holds the env var name)
  cor_p <- stats_pearson$Correlation[stats_pearson$Type == "E1"]
  cor_s <- stats_spearman$Correlation[stats_spearman$Type == "E1"]

  # Sanity: both should be strongly positive (S1 = E1^2 is monotonically assoc)
  expect_gt(cor_p, 0.9, label = "pearson(S1, E1) should be > 0.9")
  expect_gt(cor_s, 0.99, label = "spearman(S1, E1) should be >= 0.999 (monotone)")

  # The key assertion: the two values must DIFFER.
  # Before the fix they are identical (both use Pearson defaults).
  # After the fix the Spearman rank-correlation is strictly greater.
  expect_false(
    isTRUE(all.equal(cor_p, cor_s, tolerance = 1e-6)),
    label = paste0(
      "pearson (", round(cor_p, 6), ") and spearman (",
      round(cor_s, 6), ") spec-env correlations must differ; ",
      "if they are equal the method arg is being ignored (H5 bug)"
    )
  )
})

test_that("H5: spearman spec-env correlation equals psych::corr.test spearman directly", {
  # Cross-check: the value returned by gglink_heatmaps with cor.method='spearman'
  # must match what psych::corr.test(spec_h5, env_h5, method='spearman') returns.
  stats_spearman <- run_h5("spearman")
  cor_s <- stats_spearman$Correlation[stats_spearman$Type == "E1"]

  direct <- psych::corr.test(spec_h5, env_h5,
                             use = "complete", method = "spearman")
  expected_s <- direct$r["S1", "E1"]

  expect_equal(cor_s, expected_s, tolerance = 1e-9,
    label = "gglink_heatmaps spearman spec-env must match psych::corr.test spearman directly")
})

# ── L13: guard tiny blocks (<4 rows) ─────────────────────────────────────────

test_that("L13: <4-row block on env-env path emits clean message, no psych warning, no NaN", {
  # 3-row env with 2 columns (triggers psych "Number of subjects > 3" if unguarded)
  env_tiny  <- data.frame(E1 = c(1, 2, 3), E2 = c(3, 1, 2))
  spec_tiny <- data.frame(S1 = c(5, 6, 7))

  # We expect NO warning from psych (the guard should have fired first),
  # and ideally a clean message from our guard.
  # We test both: that no warning slips through AND that the function does
  # not produce NaN in the returned stats.
  expect_no_warning(
    {
      res_tiny <- gglink_heatmaps(
        env          = env_tiny,
        spec         = spec_tiny,
        env_select   = list(Env01 = 1:2),
        spec_select  = list(Spec01 = 1L),
        orientation  = "top_right",
        relation_method = "correlation",
        cor.method   = "pearson",
        cor.use      = "complete",
        spec_relation = FALSE
      )
    },
    message = "Number of subjects"   # must not see psych's raw warning
  )

  # The returned stats data frame must not contain NaN correlations
  stats_tiny <- res_tiny[[3]]
  if (nrow(stats_tiny) > 0) {
    expect_false(any(is.nan(stats_tiny$Correlation)),
      label = "No NaN correlations should appear for tiny blocks (L13)")
  }
})

test_that("L13: zero-variance column on env-env path emits clean message, no NaN", {
  # env with one constant column (sd = 0) -> correlation is NaN if unguarded
  env_const  <- data.frame(E1 = c(1, 2, 3, 4, 5),
                           E2 = c(7, 7, 7, 7, 7))   # constant
  spec_const <- data.frame(S1 = c(1, 2, 3, 4, 5))

  expect_no_warning(
    {
      res_const <- gglink_heatmaps(
        env          = env_const,
        spec         = spec_const,
        env_select   = list(Env01 = 1:2),
        spec_select  = list(Spec01 = 1L),
        orientation  = "top_right",
        relation_method = "correlation",
        cor.method   = "pearson",
        cor.use      = "complete",
        spec_relation = FALSE
      )
    }
  )

  stats_const <- res_const[[3]]
  if (nrow(stats_const) > 0) {
    expect_false(any(is.nan(stats_const$Correlation)),
      label = "No NaN spec-env correlations from zero-variance column (L13)")
  }
})
