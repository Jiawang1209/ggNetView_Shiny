#' @noRd
VirtualPerturbation <- function(mat,
                                transfrom.method = c("none", "scale", "center", "log2", "log10", "ln", "rrarefy", "rrarefy_relative"),
                                r.threshold = 0.7,
                                p.threshold = 0.05,
                                method = c("WGCNA", "SpiecEasi", "SPARCC", "cor"),
                                cor.method = c("pearson", "kendall", "spearman"),
                                proc = c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none"),
                                module.method = c("Fast_greedy", "Walktrap", "Edge_betweenness", "Spinglass"),
                                SpiecEasi.method = c("mb", "glasso"),
                                node_annotation = NULL,
                                top_modules = 15,
                                seed = 1115){

  method <- match.arg(method)

}
