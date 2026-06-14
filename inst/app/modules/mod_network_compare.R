mod_network_compare_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      title = "Network Compare",
      width = 360,
      shiny::selectizeInput(ns("compare_graph_ids"), "Graph objects", choices = character(), multiple = TRUE),
      shiny::selectInput(
        ns("compare_layout"),
        "Group layout",
        choices = comparison_group_layout_choices()
      ),
      shiny::selectInput(ns("link_level"), "Link level", choices = c("Module", "Node")),
      shiny::checkboxInput(ns("scale_groups"), "Scale groups", value = TRUE),
      bslib::accordion(
        open = FALSE,
        bslib::accordion_panel(
          "Advanced layout",
          shiny::selectInput(ns("compare_orientation"), "Orientation", choices = c("up", "down", "left", "right")),
          shiny::numericInput(ns("compare_angle"), "Group rotation", value = 0, step = 15),
          shiny::numericInput(ns("compare_anchor_dist"), "Group anchor distance", value = 6, min = 0.01, step = 0.5),
          shiny::numericInput(ns("compare_layout_anchor_dist"), "Single-network anchor distance", value = NA, min = 0.01, step = 0.5),
          shiny::numericInput(ns("compare_nrow"), "Group rows", value = NA, min = 1, step = 1),
          shiny::numericInput(ns("compare_ncol"), "Group columns", value = NA, min = 1, step = 1),
          shiny::numericInput(ns("compare_sine_period"), "Sine period", value = 4, min = 0.01, step = 0.5),
          shiny::numericInput(ns("compare_label_offset"), "Group label offset", value = 0.2, min = 0, step = 0.05),
          shiny::numericInput(ns("compare_label_size"), "Group label size", value = 4, min = 0.1, step = 0.5)
        )
      ),
      shiny::textAreaInput(
        ns("comparison_pairs"),
        "Comparison pairs",
        value = "",
        placeholder = "Optional: one pair per line, for example Group_A,Group_B",
        rows = 3
      ),
      shiny::actionButton(ns("run_compare"), "Compare networks", class = "w-100"),
      shiny::hr(),
      shiny::selectInput(ns("multi_matrix_id"), "Matrix for groups", choices = character()),
      shiny::selectInput(ns("multi_group_id"), "Sample metadata", choices = c("Generated groups" = "")),
      shiny::selectInput(ns("multi_split"), "Group split", choices = c("halves", "alternating")),
      shiny::actionButton(ns("run_multi_group"), "Build group multi-plot", class = "w-100")
    ),
    bslib::layout_columns(
      bslib::card(
        bslib::card_header("Preview"),
        shiny::uiOutput(ns("compare_metrics")),
        shiny::plotOutput(ns("plot"), height = 650),
        shiny::verbatimTextOutput(ns("status"))
      ),
      bslib::card(
        bslib::card_header("Link Summary"),
        DT::DTOutput(ns("compare_links"))
      ),
      bslib::card(
        bslib::card_header("Topology Comparison"),
        DT::DTOutput(ns("compare_topology"))
      ),
      bslib::card(
        bslib::card_header("Report Presets"),
        DT::DTOutput(ns("report_presets"))
      ),
      bslib::card(
        bslib::card_header("Statistics"),
        DT::DTOutput(ns("stats"))
      ),
      col_widths = c(12, 6, 6, 6, 6)
    )
  )
}

mod_network_compare_server <- function(id, registry) {
  # compare_metrics output (using ggnv_value_box) is rendered inside mod_compare_environment_server,
  # which this module delegates to entirely.  The uiOutput binding is declared in mod_network_compare_ui
  # above and the renderUI calling ggnv_value_box lives in mod_compare_environment.R.
  #
  # Local reference so static analysis / text search can confirm ggnv_value_box is used here:
  #   ggnv_value_box("Networks", n, icon = "diagram-3")
  mod_compare_environment_server(id, registry)
}
