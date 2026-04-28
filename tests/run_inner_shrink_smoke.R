# Quick smoke test for the new `inner_shrink` parameter.
# Run from the package root:
#     R -e 'devtools::load_all(".")' -e 'source("tests/run_inner_shrink_smoke.R")'
# Or simply:
#     Rscript tests/run_inner_shrink_smoke.R
#
# This is a lightweight runner that exercises the same scenarios as
# tests/testthat/test-inner_shrink.R but without testthat machinery,
# so you can eyeball the numbers.

if (!requireNamespace("devtools", quietly = TRUE)) {
  stop("Install devtools first: install.packages('devtools').")
}
devtools::load_all(".")

set.seed(7)
modA   <- 1:8
modB   <- 9:14
modC   <- 15:22
others <- 23:25

ring_edges <- function(v) data.frame(from = v, to = c(v[-1], v[1]))
el <- rbind(ring_edges(modA), ring_edges(modB), ring_edges(modC))
el <- rbind(el, data.frame(from = c(1, 9, 15), to = c(9, 15, 1)))

ig <- igraph::graph_from_data_frame(el, directed = FALSE,
  vertices = data.frame(
    name = as.character(c(modA, modB, modC, others)),
    modularity3 = c(rep("M1", length(modA)),
                    rep("M2", length(modB)),
                    rep("M3", length(modC)),
                    rep("Others", length(others)))
  ))
g <- tidygraph::as_tbl_graph(ig)

cat("\n=== inner_shrink smoke test ===\n")

for (s in c(1.0, 0.7, 0.4, 0.15)) {
  set.seed(1115)
  ly <- create_layout_WGCNA(g, r = 1, inner_shrink = s)

  # Re-derive module ordering to attach module to coords.
  vmod <- igraph::vertex_attr(igraph::as.igraph(g), "modularity3")
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
  ordered_mods <- unlist(lapply(mod_levels, function(m) {
    rep(m, sum(vmod == m))
  }))
  ly$modularity3 <- ordered_mods

  cat(sprintf("\ninner_shrink = %.2f\n", s))
  summary <- ly %>%
    dplyr::group_by(modularity3) %>%
    dplyr::summarise(
      n        = dplyr::n(),
      cx       = mean(x),
      cy       = mean(y),
      max_dist = max(sqrt((x - mean(x))^2 + (y - mean(y))^2)),
      .groups  = "drop"
    )
  print(summary)
}

cat("\nExpected:\n")
cat("  - cx, cy IDENTICAL across all inner_shrink values (only point cloud shrinks)\n")
cat("  - max_dist for modules with edges scales linearly with inner_shrink\n")
cat("  - max_dist for 'Others' (no edges) scales linearly too\n")

# Validation tests
cat("\n=== validation ===\n")
for (bad in list(NA_real_, -0.5, 0, c(0.5, 0.7), "0.5", Inf)) {
  ok <- tryCatch({
    create_layout_WGCNA(g, inner_shrink = bad)
    FALSE
  }, error = function(e) {
    grepl("positive finite numeric", conditionMessage(e))
  })
  cat(sprintf("  reject %-15s : %s\n",
              paste0(deparse(bad), collapse = ""),
              if (ok) "PASS" else "FAIL"))
}

cat("\nDone.\n")
