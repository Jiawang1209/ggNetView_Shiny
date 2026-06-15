## test-backend-mantel.R
## TDD tests for Mantel-test determinism (H4) and n<4 guard (L14).
##
## H4 determinism: mantel_pairwise() and mantel_block_vs_col() must return
##   identical results when called twice with the same seed.
## L14 guard: both functions must warn (or error) when n < 4.

if (!exists("mantel_pairwise")) pkgload::load_all(".", quiet = TRUE)

# ── minimal valid fixtures (n = 7) ──────────────────────────────────────────

set.seed(99)
n_samples <- 7L

# spec: 2 columns (for mantel_pairwise) or multi-column block (block_vs_col)
spec5 <- data.frame(
  sp1 = runif(n_samples, 0, 10),
  sp2 = runif(n_samples, 0, 10),
  sp3 = runif(n_samples, 0, 10)
)

# env: 2 columns
env5 <- data.frame(
  env1 = runif(n_samples, 0, 5),
  env2 = runif(n_samples, 0, 5)
)

# ── small (n = 3) fixture for the guard test ────────────────────────────────

spec3 <- data.frame(sp1 = c(1, 2, 3), sp2 = c(3, 1, 2))
env3  <- data.frame(env1 = c(0.1, 0.5, 0.9))

# ── H4: mantel_pairwise() is reproducible with seed ─────────────────────────

test_that("mantel_pairwise returns identical results when called twice with same seed", {
  run1 <- mantel_pairwise(spec5, env5, method = "pearson",
                          permutations = 499L, seed = 42L)
  run2 <- mantel_pairwise(spec5, env5, method = "pearson",
                          permutations = 499L, seed = 42L)

  expect_identical(run1$Correlation, run2$Correlation,
    info = "Mantel statistic (r) should be identical across runs with same seed")
  expect_identical(run1$Pvalue, run2$Pvalue,
    info = "Mantel p-value should be identical across runs with same seed")
})

# ── H4: mantel_block_vs_col() is reproducible with seed ─────────────────────

test_that("mantel_block_vs_col returns identical results when called twice with same seed", {
  run1 <- mantel_block_vs_col(spec5, env5,
                              block_name = "test_block",
                              method = "pearson",
                              spec_dist_method = "euclidean",
                              env_dist_method = "euclidean",
                              permutations = 499L,
                              seed = 42L)
  run2 <- mantel_block_vs_col(spec5, env5,
                              block_name = "test_block",
                              method = "pearson",
                              spec_dist_method = "euclidean",
                              env_dist_method = "euclidean",
                              permutations = 499L,
                              seed = 42L)

  expect_identical(run1$Correlation, run2$Correlation,
    info = "Block-vs-col Mantel r should be identical across runs with same seed")
  expect_identical(run1$Pvalue, run2$Pvalue,
    info = "Block-vs-col Mantel p-value should be identical across runs with same seed")
})

# ── H4 (extra): different seeds produce different p-values (anti-gaming check)
# Note: this test can theoretically fail with astronomically bad luck (seeds
# happen to produce same permutation p-value).  We use a large permutation
# count and compare the full numeric vector across all pairs; the probability
# of ALL values coinciding by chance is negligible.

test_that("mantel_pairwise gives different p-values for different seeds (anti-gaming)", {
  r1 <- mantel_pairwise(spec5, env5, method = "pearson",
                        permutations = 999L, seed = 1L)
  r2 <- mantel_pairwise(spec5, env5, method = "pearson",
                        permutations = 999L, seed = 9999L)

  # At least one p-value should differ between the two seed runs.
  # (If everything is NA, the test would erroneously pass; check we have data.)
  expect_true(any(!is.na(r1$Pvalue)), info = "run1 should return finite p-values")
  expect_false(identical(r1$Pvalue, r2$Pvalue),
    info = "Different seeds should (very likely) produce different permutation p-values")
})

# ── L14: n < 4 guard in mantel_pairwise ─────────────────────────────────────

test_that("mantel_pairwise warns or errors when n < 4", {
  # expect at minimum a warning; treat error as also acceptable
  result <- tryCatch(
    withCallingHandlers(
      mantel_pairwise(spec3, env3, method = "pearson",
                      permutations = 99L, seed = 42L),
      warning = function(w) {
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) e
  )
  # We need to verify a warning was issued. Use expect_warning which accepts
  # any warning message (regexp = NA means "any warning").
  expect_warning(
    mantel_pairwise(spec3, env3, method = "pearson",
                    permutations = 99L, seed = 42L)
  )
})

# ── L14: n < 4 guard in mantel_block_vs_col ─────────────────────────────────

test_that("mantel_block_vs_col warns when n < 4", {
  expect_warning(
    mantel_block_vs_col(spec3, env3,
                        block_name = "tiny",
                        method = "pearson",
                        spec_dist_method = "euclidean",
                        env_dist_method = "euclidean",
                        permutations = 99L,
                        seed = 42L),
    regexp = NULL   # any warning message is acceptable
  )
})
