mod_environment_links_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      title = "Environment Links",
      width = 390,
      shiny::selectInput(ns("spec_id"), "Spec matrix", choices = character()),
      shiny::selectInput(ns("env_id"), "Environment matrix", choices = character()),
      shiny::selectInput(ns("relation_method"), "Relation method", choices = c("correlation", "mantel")),
      shiny::selectInput(ns("cor_method"), "Correlation", choices = c("pearson", "spearman", "kendall")),
      shiny::selectInput(ns("mantel_kind"), "Mantel kind", choices = c("block_vs_col", "col_vs_col")),
      shiny::selectInput(ns("mantel_method"), "Mantel correlation", choices = c("pearson", "spearman", "kendall")),
      shiny::selectInput(ns("mantel_alternative"), "Mantel alternative", choices = c("two.sided", "less", "greater")),
      shiny::selectInput(ns("spec_dist_method"), "Spec distance", choices = c("bray", "euclidean", "manhattan", "jaccard")),
      shiny::selectInput(ns("env_dist_method"), "Env distance", choices = c("euclidean", "manhattan", "bray", "jaccard")),
      shiny::numericInput(ns("mantel_permutations"), "Mantel permutations", value = 99, min = 1, step = 1),
      shiny::checkboxInput(ns("spec_collapse"), "Collapse species block", value = FALSE),
      shiny::checkboxInput(ns("drop_nonsig"), "Drop non-significant links", value = FALSE),
      bslib::accordion(
        open = FALSE,
        bslib::accordion_panel(
          "Blocks and pairs",
          shiny::textAreaInput(
            ns("env_blocks"),
            "Environment blocks",
            value = "",
            placeholder = "Optional: Climate: temperature,pH\nWater: moisture",
            rows = 3
          ),
          shiny::textAreaInput(
            ns("spec_blocks"),
            "Species blocks",
            value = "",
            placeholder = "Optional: Early: OTU1,OTU2,OTU3\nLate: OTU4,OTU5,OTU6",
            rows = 3
          ),
          shiny::textAreaInput(
            ns("env_spec_pairs"),
            "Environment/spec pairs",
            value = "",
            placeholder = "Optional: Climate,Early\nWater,Late",
            rows = 3
          )
        ),
        bslib::accordion_panel(
          "Geometry",
          shiny::textAreaInput(
            ns("env_orientation"),
            "Heatmap orientations",
            value = "top_right",
            placeholder = "top_right,bottom_right,top_left,bottom_left",
            rows = 2
          ),
          shiny::textAreaInput(
            ns("env_spec_layouts"),
            "Spec block layouts",
            value = "circle_outline",
            placeholder = "circle_outline,square_outline,rectangle_outline",
            rows = 2
          ),
          shiny::selectInput(
            ns("env_group_layout"),
            "Core group layout",
            choices = c("circle", "row", "column", "square", "diamond", "triangle", "triangle_down", "snake", "arc")
          ),
          shiny::numericInput(ns("env_group_angle"), "Core group rotation", value = 0, step = 15),
          shiny::numericInput(ns("env_group_arc_angle"), "Core arc angle", value = 90, step = 15),
          shiny::numericInput(ns("env_anchor_dist"), "Core anchor distance", value = 6, min = 0.5, step = 0.5),
          shiny::numericInput(ns("env_distance"), "Heatmap distance", value = 3, step = 0.5),
          shiny::numericInput(ns("env_nrow"), "Core rows", value = 1, min = 1, step = 1),
          shiny::numericInput(ns("env_ncol"), "Core columns", value = NA, min = 1, step = 1),
          shiny::checkboxInput(ns("env_scale_networks"), "Scale core networks", value = TRUE)
        ),
        bslib::accordion_panel(
          "Style",
          shiny::numericInput(ns("env_core_point_size"), "Core point size", value = 8.5, min = 0.5, step = 0.5),
          shiny::numericInput(ns("env_heatmap_label_size"), "Heatmap label size", value = 5, min = 0.5, step = 0.5),
          shiny::numericInput(ns("env_heatmap_sig_size"), "Heatmap sig size", value = 5, min = 0.5, step = 0.5),
          shiny::numericInput(ns("env_heatmap_point_size"), "Heatmap point size", value = 5, min = 0.5, step = 0.5),
          shiny::numericInput(ns("env_sig_line_width_min"), "Sig line min width", value = 0.5, min = 0.05, step = 0.05),
          shiny::numericInput(ns("env_sig_line_width_max"), "Sig line max width", value = 2, min = 0.05, step = 0.05),
          shiny::textInput(ns("env_sig_line_color_low"), "Sig line low color", value = "#fdbb84"),
          shiny::textInput(ns("env_sig_line_color_high"), "Sig line high color", value = "#d7301f"),
          shiny::numericInput(ns("env_sig_line_alpha"), "Sig line alpha", value = 0.5, min = 0, max = 1, step = 0.05)
        ),
        bslib::accordion_panel(
          "Module and triple heatmaps",
          shiny::selectInput(ns("module_graph_id"), "Module heatmap graph", choices = character()),
          shiny::selectInput(ns("module_index"), "Module index", choices = c("eigengene", "abundance")),
          shiny::selectInput(ns("module_abundance_type"), "Module abundance", choices = c("sum", "mean")),
          shiny::selectInput(ns("triple_graph_id"), "Triple heatmap graph", choices = character()),
          shiny::numericInput(ns("triple_feature_count"), "Triple feature count", value = 3, min = 1, step = 1)
        )
      ),
      shiny::actionButton(ns("run_environment"), "Run environment link", class = "w-100"),
      shiny::actionButton(ns("run_environment_manual"), "Run manual heatmap", class = "w-100"),
      shiny::actionButton(ns("run_module_environment"), "Run module heatmap", class = "w-100"),
      shiny::actionButton(ns("run_environment_triple"), "Run triple heatmap", class = "w-100"),
      shiny::actionButton(ns("run_mantel"), "Run Mantel table", class = "w-100")
    ),
    bslib::layout_columns(
      bslib::card(
        bslib::card_header("Preview — standard vs. adaptive tile sizing"),
        shiny::uiOutput(ns("compare_metrics")),
        bslib::layout_columns(
          col_widths = c(6, 6),
          shiny::plotOutput(ns("plot"), height = 650),
          shiny::plotOutput(ns("plot_adaptive"), height = 650)
        ),
        shiny::verbatimTextOutput(ns("status"))
      ),
      bslib::card(
        bslib::card_header("Statistics"),
        DT::DTOutput(ns("stats"))
      ),
      bslib::card(
        bslib::card_header("Link Summary"),
        DT::DTOutput(ns("compare_links"))
      ),
      bslib::card(
        bslib::card_header("Report Presets"),
        DT::DTOutput(ns("report_presets"))
      ),
      shiny::div(
        class = "ggnv-hidden-output-binding",
        DT::DTOutput(ns("compare_topology"))
      ),
      col_widths = c(12, 6, 6, 12)
    )
  )
}

mod_environment_links_server <- function(id, registry) {
  mod_compare_environment_server(id, registry)
}
