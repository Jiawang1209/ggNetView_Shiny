# global.R — sourced once when the app starts
# All UI/server files share these helpers and constants.

suppressPackageStartupMessages({
  library(shiny)
  library(shinydashboard)
  library(shinycssloaders)
  library(shinyWidgets)
  library(DT)
  library(ggplot2)
  library(ggNetView)
})

# ---- expose internal helpers from ggNetView.shiny ---------------------------
# When run interactively from inst/app, ggNetView.shiny may not be installed,
# so fall back to sourcing R/utils_io.R directly.
if (requireNamespace("ggNetView.shiny", quietly = TRUE)) {
  read_user_table     <- getFromNamespace("read_user_table",     "ggNetView.shiny")
  list_ggNetView_data <- getFromNamespace("list_ggNetView_data", "ggNetView.shiny")
  get_ggNetView_data  <- getFromNamespace("get_ggNetView_data",  "ggNetView.shiny")
  is_graph_obj        <- getFromNamespace("is_graph_obj",        "ggNetView.shiny")
  describe_object     <- getFromNamespace("describe_object",     "ggNetView.shiny")
} else {
  utils_path <- file.path("..", "..", "R", "utils_io.R")
  if (file.exists(utils_path)) source(utils_path, local = TRUE)
}

# ---- constants used across UI -----------------------------------------------
LAYOUTS <- c(
  "gephi", "fr", "kk", "stress", "circle", "grid",
  "circular_modules_grid_layout",
  "circular_modules_gephi_layout",
  "circular_modules_equal_gephi_layout",
  "circular_modules_petal_layout",
  "circular_modules_star_layout",
  "circular_modules_diamond_layout",
  "circular_modules_square_layout",
  "circular_modules_heart_centered_layout",
  "tripartite_layout",
  "quadripartite_layout",
  "pentapartite_layout",
  "bipartite_layout",
  "WGCNA",
  "dendrogram",
  "nicely",
  "lgl",
  "randomly"
)

CORR_METHODS    <- c("pearson", "kendall", "spearman")
NETWORK_METHODS <- c("WGCNA", "SpiecEasi", "SPARCC", "cor", "Hmisc")
PADJ_METHODS    <- c("BH", "BY", "fdr", "holm", "hochberg",
                     "hommel", "bonferroni", "none")
TRANSFORM_METHODS <- c("none", "scale", "center", "log2", "log10",
                       "ln", "rrarefy", "rrarefy_relative")
MODULE_METHODS  <- c("Fast_greedy", "Walktrap",
                     "Edge_betweenness", "Spinglass")
LAYOUT_MODULES  <- c("random", "adjacent", "order")

GGN_DATASETS <- list_ggNetView_data()
