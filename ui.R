layout_choices <- c(
  "gephi", "fr", "kk", "stress", "circle", "circle_outline",
  "square", "diamond", "star", "petal", "rings", "randomly"
)

module_method_choices <- c("Fast_greedy", "Walktrap", "Edge_betweenness", "Spinglass")

home_readme_content <- if (file.exists("README.html")) {
  readme_html <- paste(readLines("README.html", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  tags$iframe(
    srcdoc = readme_html,
    style = "width: 100%; height: 78vh; border: 0; border-radius: 10px; background: #ffffff;"
  )
} else if (file.exists("README.md")) {
  markdown::includeMarkdown("README.md")
} else {
  tags$p("README file not found.")
}

viz_controls_ui <- function(id_prefix) {
  tagList(
    div(
      class = "compact-grid",
      selectInput(paste0(id_prefix, "_layout"), "Layout", choices = layout_choices, selected = "gephi"),
      selectInput(
        paste0(id_prefix, "_layout_module"),
        "Layout module mode",
        choices = c("random", "adjacent", "order"),
        selected = "adjacent"
      ),
      textInput(paste0(id_prefix, "_group_by"), "Group by", value = "Modularity"),
      textInput(paste0(id_prefix, "_fill_by"), "Fill by", value = "Modularity"),
      sliderInput(paste0(id_prefix, "_point_min"), "Point size min", min = 0.1, max = 10, value = 1, step = 0.1),
      sliderInput(paste0(id_prefix, "_point_max"), "Point size max", min = 1, max = 25, value = 8, step = 0.5),
      sliderInput(paste0(id_prefix, "_line_alpha"), "Line alpha", min = 0, max = 1, value = 0.25, step = 0.05),
      textInput(paste0(id_prefix, "_line_color"), "Line color", value = "grey70"),
      checkboxInput(paste0(id_prefix, "_plot_line"), "Plot edges", value = TRUE),
      checkboxInput(paste0(id_prefix, "_label"), "Show module labels", value = FALSE),
      checkboxInput(paste0(id_prefix, "_add_outer"), "Show module outer borders", value = FALSE),
      numericInput(paste0(id_prefix, "_seed"), "Seed", value = 1115, min = 1, step = 1),
      numericInput(paste0(id_prefix, "_plot_width"), "Plot width (px)", value = 900, min = 300, step = 50),
      numericInput(paste0(id_prefix, "_plot_height"), "Plot height (px)", value = 650, min = 300, step = 50)
    ),
    fluidRow(
      column(6, actionButton(paste0(id_prefix, "_build_plotly"), "Generate Plotly", class = "btn-sem-visualize")),
      column(6, tags$small("Plotly will run only when clicked."))
    ),
    actionButton(paste0(id_prefix, "_plot_graph"), "Visualize Graph", class = "btn-sem-visualize"),
    br(),
    br()
  )
}

input_preview_card <- function(id_prefix) {
  bs4Dash::bs4Card(
    title = "Input Data Preview",
    width = 12,
    status = "secondary",
    solidHeader = TRUE,
    collapsible = TRUE,
    verbatimTextOutput(paste0(id_prefix, "_data_info")),
    br(),
    tags$h5("Primary Data"),
    DT::DTOutput(paste0(id_prefix, "_data_preview_main")),
    br(),
    tags$h5("Annotation / Additional Data"),
    DT::DTOutput(paste0(id_prefix, "_data_preview_aux"))
  )
}

result_boxes_ui <- function(id_prefix, title = "Result") {
  tagList(
    bs4Dash::bs4Card(
      title = "graph_obj Statistics",
      width = 12,
      status = "secondary",
      solidHeader = TRUE,
      collapsible = TRUE,
      fluidRow(
        column(
          4,
          checkboxInput(paste0(id_prefix, "_enable_stat"), "Enable graph_obj statistics", value = TRUE),
          actionButton(paste0(id_prefix, "_compute_stat"), "Compute graph_obj Statistics", class = "btn-sem-build")
        ),
        column(
          8,
          tags$p("Run statistics only when needed to save compute time.")
        )
      )
    ),
    fluidRow(
      bs4Dash::bs4Card(
        title = "graph_obj Overview",
        width = 6,
        status = "secondary",
        solidHeader = TRUE,
        collapsible = TRUE,
        DT::DTOutput(paste0(id_prefix, "_summary"))
      ),
      bs4Dash::bs4Card(
        title = "Module Statistics (get_subgraph)",
        width = 6,
        status = "secondary",
        solidHeader = TRUE,
        collapsible = TRUE,
        DT::DTOutput(paste0(id_prefix, "_module_stat"))
      )
    ),
    fluidRow(
      bs4Dash::bs4Card(
        title = paste0(title, ": ggNetView"),
        width = 12,
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        plotOutput(paste0(id_prefix, "_plot"))
      ),
      bs4Dash::bs4Card(
        title = paste0(title, ": Plotly"),
        width = 12,
        status = "info",
        solidHeader = TRUE,
        collapsible = TRUE,
        plotly::plotlyOutput(paste0(id_prefix, "_plotly"), height = "550px")
      )
    ),
    fluidRow(
      bs4Dash::bs4Card(
        title = "Export",
        width = 12,
        status = "secondary",
        solidHeader = TRUE,
        collapsible = TRUE,
        fluidRow(
          column(6, downloadButton(paste0(id_prefix, "_save_pdf"), "Save PDF", class = "btn-sem-download")),
          column(6, downloadButton(paste0(id_prefix, "_save_png"), "Save PNG", class = "btn-sem-download"))
        )
      )
    ),
    fluidRow()
  )
}

network_builder_page_ui <- function(id_prefix, title, controls_ui) {
  tagList(
    fluidRow(
      column(
        width = 6,
        bs4Dash::bs4Card(
          title = paste0(title, ": Data Input"),
          width = 12,
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          div(class = "compact-grid", controls_ui),
          br(),
          fluidRow(
            column(6, actionButton(paste0(id_prefix, "_preview_data"), "Preview Input Data", class = "btn-sem-preview")),
            column(6, actionButton(paste0(id_prefix, "_build_graph"), "Build graph_obj", class = "btn-sem-build"))
          )
        )
      ),
      column(width = 6, input_preview_card(id_prefix))
    ),
    fluidRow(
      column(
        width = 6,
        bs4Dash::bs4Card(
          title = paste0(title, ": Visualization Settings"),
          width = 12,
          status = "info",
          solidHeader = TRUE,
          collapsible = TRUE,
          viz_controls_ui(id_prefix)
        )
      ),
      column(width = 6, result_boxes_ui(id_prefix, title = title))
    )
  )
}

ui <- bs4Dash::bs4DashPage(
  title = "ggNetView",
  dark = FALSE,
  fullscreen = TRUE,
  freshTheme = NULL,
  header = bs4Dash::bs4DashNavbar(
    title = "Welcome to ggNetView!",
    skin = "light",
    border = TRUE,
    status = "white"
  ),
  sidebar = bs4Dash::bs4DashSidebar(
    width = 220,
    skin = "light",
    status = "primary",
    elevation = 3,
    bs4Dash::bs4SidebarMenu(
      id = "main_tabs",
      bs4Dash::bs4SidebarMenuItem("Home", tabName = "home", icon = icon("house")),
      bs4Dash::bs4SidebarMenuItem("Sample Datasets", tabName = "sample_data", icon = icon("table")),
      bs4Dash::bs4SidebarMenuItem(
        "Microbial Network",
        icon = icon("diagram-project"),
        startExpanded = TRUE,
        bs4Dash::bs4SidebarMenuSubItem("From matrix", tabName = "micro_mat"),
        bs4Dash::bs4SidebarMenuSubItem("From data frame", tabName = "micro_df"),
        bs4Dash::bs4SidebarMenuSubItem("From adjacency matrix", tabName = "micro_adj"),
        bs4Dash::bs4SidebarMenuSubItem("From double matrices", tabName = "micro_double"),
        bs4Dash::bs4SidebarMenuSubItem("From WGCNA", tabName = "micro_wgcna"),
        bs4Dash::bs4SidebarMenuSubItem("From module table", tabName = "micro_module")
      ),
      bs4Dash::bs4SidebarMenuItem("Protein Network", tabName = "protein", icon = icon("dna")),
      bs4Dash::bs4SidebarMenuItem("WGCNA", tabName = "wgcna", icon = icon("project-diagram")),
      bs4Dash::bs4SidebarMenuItem("Env-Link", tabName = "envlink", icon = icon("link")),
      bs4Dash::bs4SidebarMenuItem("Developer", tabName = "developer", icon = icon("code")),
      bs4Dash::bs4SidebarMenuItem("Citation", tabName = "citation", icon = icon("quote-left"))
    )
  ),
  body = bs4Dash::bs4DashBody(
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")),
    bs4Dash::bs4TabItems(
      bs4Dash::bs4TabItem(
        tabName = "home",
        fluidRow(
          column(
            width = 12,
            div(class = "home-readme", home_readme_content)
          )
        )
      ),
      bs4Dash::bs4TabItem(
        tabName = "sample_data",
        fluidRow(
          bs4Dash::bs4Card(
            title = "Sample Datasets: Input",
            status = "primary",
            width = 12,
            solidHeader = TRUE,
            div(
              class = "compact-grid",
              selectInput(
                "sample_dataset_name",
                "Select dataset",
                choices = c("otu_tab", "otu_rare", "otu_rare_relative", "tax_tab", "Envdf_4st", "Spedf"),
                selected = "otu_tab"
              ),
              selectInput(
                "sample_download_format",
                "Download format",
                choices = c("CSV" = "csv", "TXT (tab-separated)" = "txt", "XLSX" = "xlsx", "RDS" = "rds"),
                selected = "csv"
              )
            ),
            fluidRow(
              column(6, actionButton("sample_dataset_refresh", "Preview Data", class = "btn-sem-preview")),
              column(6, downloadButton("sample_dataset_download", "Download Data", class = "btn-sem-download"))
            )
          )
        ),
        fluidRow(
          bs4Dash::bs4Card(
            title = "Sample Datasets: Preview",
            status = "secondary",
            width = 12,
            solidHeader = TRUE,
            verbatimTextOutput("sample_dataset_info"),
            DT::DTOutput("sample_dataset_table")
          )
        )
      ),
      bs4Dash::bs4TabItem(
        tabName = "micro_mat",
        network_builder_page_ui(
          "micro_mat",
          "Microbial Network: build_graph_from_mat",
          tagList(
            selectInput("micro_mat_data_mode", "Data source", choices = c("Demo", "Upload"), selected = "Demo"),
            fileInput("micro_mat_file", "Upload matrix (csv/tsv/rds/rda)", accept = c(".csv", ".tsv", ".txt", ".rds", ".rda", ".RData")),
            fileInput("micro_mat_anno", "Upload node annotation (optional)", accept = c(".csv", ".tsv", ".txt", ".rds", ".rda", ".RData")),
            selectInput("micro_mat_transform", "Transform", choices = c("none", "scale", "center", "log2", "log10", "ln"), selected = "none"),
            selectInput("micro_mat_method", "Method", choices = c("WGCNA", "cor", "SpiecEasi", "SPARCC"), selected = "WGCNA"),
            selectInput("micro_mat_cor", "Correlation", choices = c("pearson", "kendall", "spearman"), selected = "pearson"),
            sliderInput("micro_mat_r_threshold", "R threshold", min = 0, max = 1, value = 0.7, step = 0.01),
            sliderInput("micro_mat_p_threshold", "P threshold", min = 0, max = 0.2, value = 0.05, step = 0.001),
            selectInput("micro_mat_module_method", "Module method", choices = module_method_choices),
            numericInput("micro_mat_top_modules", "Top modules", value = 15, min = 1, step = 1)
          )
        )
      ),
      bs4Dash::bs4TabItem(
        tabName = "micro_df",
        network_builder_page_ui(
          "micro_df",
          "Microbial Network: build_graph_from_df",
          tagList(
            selectInput("micro_df_data_mode", "Data source", choices = c("Demo", "Upload"), selected = "Demo"),
            fileInput("micro_df_file", "Upload edge data frame", accept = c(".csv", ".tsv", ".txt", ".rds", ".rda", ".RData")),
            fileInput("micro_df_anno", "Upload node annotation (optional)", accept = c(".csv", ".tsv", ".txt", ".rds", ".rda", ".RData")),
            checkboxInput("micro_df_directed", "Directed graph", value = FALSE),
            selectInput("micro_df_module_method", "Module method", choices = module_method_choices),
            numericInput("micro_df_top_modules", "Top modules", value = 15, min = 1, step = 1)
          )
        )
      ),
      bs4Dash::bs4TabItem(
        tabName = "micro_adj",
        network_builder_page_ui(
          "micro_adj",
          "Microbial Network: build_graph_from_adj_mat",
          tagList(
            selectInput("micro_adj_data_mode", "Data source", choices = c("Demo", "Upload"), selected = "Demo"),
            fileInput("micro_adj_file", "Upload adjacency matrix", accept = c(".csv", ".tsv", ".txt", ".rds", ".rda", ".RData")),
            fileInput("micro_adj_anno", "Upload node annotation (optional)", accept = c(".csv", ".tsv", ".txt", ".rds", ".rda", ".RData")),
            selectInput("micro_adj_module_method", "Module method", choices = module_method_choices),
            numericInput("micro_adj_top_modules", "Top modules", value = 15, min = 1, step = 1)
          )
        )
      ),
      bs4Dash::bs4TabItem(
        tabName = "micro_double",
        network_builder_page_ui(
          "micro_double",
          "Microbial Network: build_graph_from_double_mat",
          tagList(
            selectInput("micro_double_data_mode", "Data source", choices = c("Demo", "Upload"), selected = "Demo"),
            fileInput("micro_double_mat1", "Upload matrix 1", accept = c(".csv", ".tsv", ".txt", ".rds", ".rda", ".RData")),
            fileInput("micro_double_mat2", "Upload matrix 2", accept = c(".csv", ".tsv", ".txt", ".rds", ".rda", ".RData")),
            fileInput("micro_double_anno", "Upload node annotation (optional)", accept = c(".csv", ".tsv", ".txt", ".rds", ".rda", ".RData")),
            checkboxInput("micro_double_directed", "Directed graph", value = FALSE),
            selectInput("micro_double_module_method", "Module method", choices = module_method_choices),
            numericInput("micro_double_top_modules", "Top modules", value = 15, min = 1, step = 1)
          )
        )
      ),
      bs4Dash::bs4TabItem(
        tabName = "micro_wgcna",
        network_builder_page_ui(
          "micro_wgcna",
          "Microbial Network: build_graph_from_wgcna",
          tagList(
            selectInput("micro_wgcna_data_mode", "Data source", choices = c("Demo", "Upload"), selected = "Demo"),
            fileInput("micro_wgcna_edge", "Upload WGCNA edge table", accept = c(".csv", ".tsv", ".txt", ".rds", ".rda", ".RData")),
            fileInput("micro_wgcna_module", "Upload module table (ID, Module)", accept = c(".csv", ".tsv", ".txt", ".rds", ".rda", ".RData")),
            fileInput("micro_wgcna_anno", "Upload node annotation (optional)", accept = c(".csv", ".tsv", ".txt", ".rds", ".rda", ".RData")),
            checkboxInput("micro_wgcna_directed", "Directed graph", value = FALSE)
          )
        )
      ),
      bs4Dash::bs4TabItem(
        tabName = "micro_module",
        network_builder_page_ui(
          "micro_module",
          "Microbial Network: build_graph_from_module",
          tagList(
            selectInput("micro_module_data_mode", "Data source", choices = c("Demo", "Upload"), selected = "Demo"),
            fileInput("micro_module_edge", "Upload edge data frame", accept = c(".csv", ".tsv", ".txt", ".rds", ".rda", ".RData")),
            fileInput("micro_module_anno", "Upload node annotation (required)", accept = c(".csv", ".tsv", ".txt", ".rds", ".rda", ".RData")),
            checkboxInput("micro_module_directed", "Directed graph", value = FALSE),
            numericInput("micro_module_top_modules", "Top modules", value = 15, min = 1, step = 1)
          )
        )
      ),
      bs4Dash::bs4TabItem(
        tabName = "protein",
        network_builder_page_ui(
          "protein",
          "Protein Network",
          tagList(
            selectInput("protein_data_mode", "Data source", choices = c("Demo", "Upload"), selected = "Demo"),
            selectInput("protein_input_type", "Input type", choices = c("Edge data frame", "Adjacency matrix"), selected = "Edge data frame"),
            fileInput("protein_file", "Upload primary file", accept = c(".csv", ".tsv", ".txt", ".rds", ".rda", ".RData")),
            fileInput("protein_anno", "Upload node annotation (optional)", accept = c(".csv", ".tsv", ".txt", ".rds", ".rda", ".RData")),
            checkboxInput("protein_directed", "Directed graph", value = FALSE),
            selectInput("protein_module_method", "Module method", choices = module_method_choices),
            numericInput("protein_top_modules", "Top modules", value = 15, min = 1, step = 1)
          )
        )
      ),
      bs4Dash::bs4TabItem(
        tabName = "wgcna",
        network_builder_page_ui(
          "wgcna",
          "WGCNA",
          tagList(
            selectInput("wgcna_data_mode", "Data source", choices = c("Demo", "Upload"), selected = "Demo"),
            fileInput("wgcna_edge", "Upload WGCNA edge table", accept = c(".csv", ".tsv", ".txt", ".rds", ".rda", ".RData")),
            fileInput("wgcna_module", "Upload module table (ID, Module)", accept = c(".csv", ".tsv", ".txt", ".rds", ".rda", ".RData")),
            fileInput("wgcna_anno", "Upload node annotation (optional)", accept = c(".csv", ".tsv", ".txt", ".rds", ".rda", ".RData")),
            checkboxInput("wgcna_directed", "Directed graph", value = FALSE)
          )
        )
      ),
      bs4Dash::bs4TabItem(
        tabName = "envlink",
        fluidRow(
          bs4Dash::bs4Card(
            title = "Env-Link: Data Input",
            status = "primary",
            width = 12,
            solidHeader = TRUE,
            div(
              class = "compact-grid",
              selectInput("envlink_data_mode", "Data source", choices = c("Demo", "Upload"), selected = "Demo"),
              fileInput("envlink_env", "Upload environment matrix", accept = c(".csv", ".tsv", ".txt", ".rds", ".rda", ".RData")),
              fileInput("envlink_spec", "Upload species matrix", accept = c(".csv", ".tsv", ".txt", ".rds", ".rda", ".RData")),
              selectInput("envlink_relation", "Relation method", choices = c("correlation", "mantel"), selected = "correlation"),
              selectInput("envlink_cor", "Correlation", choices = c("pearson", "kendall", "spearman"), selected = "pearson"),
              selectInput(
                "envlink_orientation",
                "Orientation",
                choices = c("top_right", "bottom_right", "top_left", "bottom_left"),
                selected = c("top_right", "bottom_right", "top_left", "bottom_left"),
                multiple = TRUE
              ),
              sliderInput("envlink_radius", "Radius", min = 2, max = 12, value = 6, step = 0.5),
              sliderInput("envlink_distance", "Distance", min = 0, max = 10, value = 3, step = 0.5),
              checkboxInput("envlink_drop_nonsig", "Drop non-significant links", value = FALSE)
            ),
            fluidRow(
              column(4, actionButton("envlink_preview_data", "Preview Input Data", class = "btn-sem-preview")),
              column(4, actionButton("envlink_build_obj", "Build relation_obj", class = "btn-sem-build")),
              column(4, actionButton("envlink_plot_graph", "Visualize", class = "btn-sem-visualize"))
            )
          )
        ),
        fluidRow(
          bs4Dash::bs4Card(
            title = "Env-Link: Input Data Preview",
            width = 12,
            status = "secondary",
            solidHeader = TRUE,
            DT::DTOutput("envlink_data_preview")
          )
        ),
        fluidRow(
          bs4Dash::bs4Card(
            title = "Env-Link: relation_obj Overview",
            width = 12,
            status = "secondary",
            solidHeader = TRUE,
            DT::DTOutput("envlink_relation_summary")
          )
        ),
        fluidRow(
          bs4Dash::bs4Card(
            title = "Env-Link: straight links",
            width = 12,
            status = "info",
            solidHeader = TRUE,
            plotOutput("envlink_plot_1", height = "520px")
          )
        ),
        fluidRow(
          bs4Dash::bs4Card(
            title = "Env-Link: curved links",
            width = 12,
            status = "info",
            solidHeader = TRUE,
            plotOutput("envlink_plot_2", height = "520px")
          )
        ),
        fluidRow(
          bs4Dash::bs4Card(
            title = "Env-Link: statistics",
            width = 12,
            status = "secondary",
            solidHeader = TRUE,
            DT::DTOutput("envlink_table")
          )
        )
      ),
      bs4Dash::bs4TabItem(
        tabName = "developer",
        fluidRow(
          bs4Dash::bs4Card(
            title = "Developer",
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            tags$h4("Package"),
            verbatimTextOutput("dev_pkg"),
            tags$h4("Core build functions"),
            DT::DTOutput("dev_functions"),
            tags$h4("Session info"),
            verbatimTextOutput("dev_session")
          )
        )
      ),
      bs4Dash::bs4TabItem(
        tabName = "citation",
        fluidRow(
          bs4Dash::bs4Card(
            title = "Citation",
            width = 12,
            status = "warning",
            solidHeader = TRUE,
            verbatimTextOutput("citation_text"),
            tags$p(tags$a(href = "https://github.com/Jiawang1209/ggNetView", target = "_blank", "GitHub")),
            tags$p(tags$a(href = "https://jiawang1209.github.io/ggNetView-manual/", target = "_blank", "Manual"))
          )
        )
      )
    )
  )
)
