# ui.R — shinydashboard layout

header <- shinydashboard::dashboardHeader(
  title = tags$span(
    style = "font-weight:700; letter-spacing:0.5px;",
    tags$i(class = "fa fa-network-wired",
           style = "margin-right:6px;"),
    "ggNetView GUI"
  ),
  titleWidth = 270
)

sidebar <- shinydashboard::dashboardSidebar(
  width = 270,
  shinydashboard::sidebarMenu(
    id = "main_tabs",

    # ---------- group: GUIDE ----------
    tags$li(class = "gg-side-header", "GUIDE"),
    shinydashboard::menuItem(
      "Home",            tabName = "tab_home",  icon = icon("heart")
    ),
    shinydashboard::menuItem(
      "Usage",           tabName = "tab_usage", icon = icon("book-open")
    ),

    # ---------- group: WORKFLOW ----------
    tags$li(class = "gg-side-header", "WORKFLOW"),
    shinydashboard::menuItem(
      "Data",            tabName = "tab_data",     icon = icon("database")
    ),
    shinydashboard::menuItem(
      "Build Network",   tabName = "tab_build",    icon = icon("project-diagram")
    ),
    shinydashboard::menuItem(
      "Visualize",       tabName = "tab_vis",      icon = icon("eye")
    ),
    shinydashboard::menuItem(
      "Topology / zi-pi",tabName = "tab_topo",     icon = icon("chart-line")
    ),
    shinydashboard::menuItem(
      "Env-Spec Linkage",tabName = "tab_envspec",  icon = icon("link")
    ),

    # ---------- group: INFO ----------
    tags$li(class = "gg-side-header", "INFO"),
    shinydashboard::menuItem(
      "About",           tabName = "tab_about",    icon = icon("info-circle")
    )
  )
)

# ----------------------------- Home tab --------------------------------------
# Renders three variants of the package README:
#   * Default     — full-color pink theme (English)
#   * Monochrome  — grayscale rendering (English, no color accents)
#   * Bilingual   — interleaved English + Chinese version
#
# Both README.md and README_bilingual.md (and their referenced figures
# under man/figures/*.png) live in inst/app/www/ so image paths resolve
# through Shiny's static file server.

# Helper to resolve a markdown file across the most common run scenarios
.gg_resolve_md <- function(name) {
  candidates <- c(
    file.path("www", name),
    name,
    system.file("app", "www", name, package = "ggNetView.shiny")
  )
  hits <- candidates[nzchar(candidates) & file.exists(candidates)]
  if (length(hits)) hits[1] else file.path("www", name)
}
home_readme_path    <- .gg_resolve_md("README.md")
home_bilingual_path <- .gg_resolve_md("README_bilingual.md")

# Renders one variant or a friendly error message
.gg_render_md <- function(path, extra_class = NULL) {
  classes <- paste(c("gg-readme", extra_class), collapse = " ")
  div(
    class = classes,
    if (file.exists(path))
      shiny::includeMarkdown(path)
    else
      tags$p(tags$em(
        "Markdown file not found at ", tags$code(path), ". ",
        "Make sure it (and its referenced figures) are copied to ",
        tags$code("inst/app/www/"), "."
      ))
  )
}

tab_home <- shinydashboard::tabItem(
  tabName = "tab_home",
  div(
    class = "gg-home-switch",
    tabsetPanel(
      id   = "home_variant",
      type = "pills",
      tabPanel(
        title = tagList(icon("sun"), " Light"),
        value = "light",
        .gg_render_md(home_readme_path)
      ),
      tabPanel(
        title = tagList(icon("moon"), " Dark"),
        value = "dark",
        .gg_render_md(home_readme_path, extra_class = "gg-dark")
      ),
      tabPanel(
        title = tagList(icon("language"), " Bilingual / 中英"),
        value = "bilingual",
        .gg_render_md(home_bilingual_path)
      )
    )
  )
)

# ----------------------------- Usage tab -------------------------------------
# Step-by-step guide on how to use this Shiny GUI. Reuses the .gg-readme
# article card so the look is consistent with the Home page.
tab_usage <- shinydashboard::tabItem(
  tabName = "tab_usage",
  div(
    class = "gg-readme gg-usage",

    tags$h1(icon("book-open"), "  Usage Guide"),
    tags$p(
      "This guide walks you through a complete network-analysis workflow with ",
      tags$b("ggNetView Shiny GUI"), ": ",
      "load data, build a network, visualize it, compute topology metrics, ",
      "and explore environment-species linkage heatmaps."
    ),
    tags$p(
      tags$b("Workflow:"), " ",
      tags$span(class = "gg-pill", "1 Data"), " → ",
      tags$span(class = "gg-pill", "2 Build Network"), " → ",
      tags$span(class = "gg-pill", "3 Visualize"), " → ",
      tags$span(class = "gg-pill", "4 Topology / zi-pi"), " → ",
      tags$span(class = "gg-pill", "5 Env-Spec Linkage")
    ),

    tags$hr(),

    # ---------------- Step 1 ----------------
    tags$h2(tags$span(class = "gg-step-num", "1"),
            "Data — load your inputs"),
    tags$p("Open the ", tags$b("Data"), " tab and choose one of two options:"),
    tags$ul(
      tags$li(tags$b("Built-in ggNetView dataset:"),
              " pick a packaged example, e.g. ",
              tags$code("otu_rare_relative"), " (relative-abundance matrix), ",
              tags$code("tax_tab"), " (node annotation), ",
              tags$code("Envdf_4st"), " / ", tags$code("Spedf"), "."),
      tags$li(tags$b("Upload file:"),
              " accepts CSV / TSV / TXT / RDS / RData. ",
              "Toggle whether the first row is a header and choose which ",
              "column to use as row names."),
      tags$li(tags$b("Optional node annotation:"),
              " upload a separate annotation table (taxonomy, pathway, ",
              "module, etc.) and it will be aligned to node IDs automatically.")
    ),
    tags$blockquote(
      tags$b("Tip:"), " when uploading a wide matrix (rows = ASVs / samples), ",
      "make sure ", tags$code("Column # used as row names"),
      " is set to 1, otherwise rows will be indexed by integers."
    ),
    tags$p("Click ", tags$kbd("Load"),
           " and the right panel will show the detected object kind ",
           "(matrix / df / adj / graph), its dimensions and an 8 × 8 preview."),

    # ---------------- Step 2 ----------------
    tags$h2(tags$span(class = "gg-step-num", "2"),
            "Build Network — construct the graph"),
    tags$p("Pick an Input type that matches the object loaded in the Data tab:"),
    tags$ul(
      tags$li(tags$code("Numeric matrix"),
              " → calls ", tags$code("build_graph_from_mat()"),
              " with your choice of ", tags$code("WGCNA"), " / ",
              tags$code("SpiecEasi"), " / ", tags$code("SPARCC"), " / ",
              tags$code("cor"), " / ", tags$code("Hmisc"),
              " inference methods."),
      tags$li(tags$code("Adjacency matrix"),
              " → feed a symmetric adjacency matrix directly and skip ",
              "correlation estimation."),
      tags$li(tags$code("Edge list data.frame"),
              " → use a pre-built ",
              tags$code("from / to / weight"), " table.")
    ),
    tags$p("Key parameters at a glance:"),
    tags$pre(
      "r threshold       <- correlation cut-off (default 0.7)\n",
      "p threshold       <- p-value cut-off (default 0.05), with BH/BY/fdr/...\n",
      "Module method     <- community detection: Fast_greedy / Walktrap /\n",
      "                     Edge_betweenness / Spinglass\n",
      "Top N modules     <- keep the largest N modules, others -> 'Others'\n",
      "Random seed       <- 1115 by default, fully reproducible"
    ),
    tags$p("Click ", tags$kbd("Build network"),
           " and the right panel will show node / edge counts and a ",
           "module-size table. Use ", tags$kbd("Download graph (.rds)"),
           " to save the resulting ", tags$code("tbl_graph"),
           " object to disk."),

    # ---------------- Step 3 ----------------
    tags$h2(tags$span(class = "gg-step-num", "3"),
            "Visualize — render the network"),
    tags$p("This tab is a full-parameter front-end for ",
           tags$code("ggNetView()"),
           ". Each control on the left maps directly to a function argument. ",
           "Common combinations:"),
    tags$ul(
      tags$li(tags$b("Layout:"), " ",
              tags$code("gephi"), " (most popular, separates modules cleanly), ",
              tags$code("circular_modules_*"),
              " family (modules arranged on a circle / petals / stars), ",
              tags$code("fr"), " / ", tags$code("kk"),
              " (force-directed)."),
      tags$li(tags$b("group.by / fill.by / color.by:"),
              " color points by ", tags$code("Modularity"),
              " (modules) or by an annotation column such as ",
              tags$code("Phylum"), "."),
      tags$li(tags$b("Add outer ring + label:"),
              " draws a contour around each module and labels its ID — ",
              "the typical look for publication figures."),
      tags$li(tags$b("pointlabel:"), " set to ",
              tags$code("top1"), " / ", tags$code("top3"),
              " etc. to label the top hub nodes per module by degree.")
    ),
    tags$p("Once parameters look right, click ", tags$kbd("Render plot"),
           " to draw the network. Use the ", tags$kbd("PDF"),
           " / ", tags$kbd("PNG"),
           " buttons at the bottom to export at the chosen width and height."),
    tags$blockquote(
      tags$b("Tip:"), " if Render does nothing, double-check the ",
      tags$b("Build Network"), " tab — the graph object must exist first."
    ),

    # ---------------- Step 4 ----------------
    tags$h2(tags$span(class = "gg-step-num", "4"),
            "Topology / zi-pi — metrics & node roles"),
    tags$p("This tab offers two analyses:"),
    tags$ul(
      tags$li(tags$b("Global topology:"), " runs ",
              tags$code("get_network_topology()"),
              " to compute node / edge counts, density, average degree, ",
              "modularity and more. Results can be downloaded as CSV."),
      tags$li(tags$b("zi-pi node-role analysis:"), " runs ",
              tags$code("ggnetview_zipi()"),
              " and classifies each node by ", tags$code("zi"),
              " (within-module degree) and ", tags$code("pi"),
              " (among-module participation) into ",
              tags$em("Peripherals / Connectors / Module hubs / Network hubs"),
              ".")
    ),
    tags$p("Default thresholds are zi = 2.5 and pi = 0.62 ",
           "(the classical Guimerà & Amaral 2005 cut-offs); ",
           "tweak them as needed for your network."),

    # ---------------- Step 5 ----------------
    tags$h2(tags$span(class = "gg-step-num", "5"),
            "Env-Spec Linkage — environment / species heatmap"),
    tags$p("If you have an environment matrix (Envdf) and a species / ",
           "function matrix (Spedf), this tab draws a ",
           tags$code("gglink_heatmaps()"),
           "-style linked heatmap:"),
    tags$ul(
      tags$li("Use ", tags$code("env_select"),
              " to slice environment columns into blocks, e.g. ",
              tags$code("list(Env01 = 1:14, Env02 = 15:28)"), "."),
      tags$li("Use ", tags$code("spec_select"),
              " to choose the species columns that form the central network."),
      tags$li("Relation method: pick ", tags$code("correlation"),
              " or ", tags$code("mantel"), "."),
      tags$li("Tick ", tags$code("drop non-significant"),
              " to keep only significant links — the figure becomes much cleaner.")
    ),

    tags$hr(),
    tags$h3("FAQ"),
    tags$ul(
      tags$li(tags$b("Q:"), " I see a ",
              tags$code("No shared levels..."),
              " warning while plotting. Is something wrong?",
              tags$br(),
              tags$b("A:"), " It's a harmless ggplot warning telling you ",
              "that the fill scale wasn't used at all levels. Safe to ignore."),
      tags$li(tags$b("Q:"), " The network is huge and takes forever to render.",
              tags$br(),
              tags$b("A:"), " Increase ", tags$code("r threshold"),
              " to 0.8 / 0.85 or lower ", tags$code("Top N modules"),
              " to thin the graph before plotting."),
      tags$li(tags$b("Q:"), " The annotation I uploaded doesn't match my nodes.",
              tags$br(),
              tags$b("A:"), " The first column of the annotation table must ",
              "be node IDs (matching matrix row names) and \"First row is header\" ",
              "should be ticked. Verify in the Data tab preview before building.")
    ),

    tags$hr(),
    tags$p(
      style = "text-align:right; color:#888;",
      tags$small(
        "Looking for function-level docs? See the ",
        tags$a(href = "https://jiawang1209.github.io/ggNetView-manual/",
               target = "_blank", "ggNetView Manual"),
        " or visit ",
        tags$a(href = "https://github.com/Jiawang1209/ggNetView",
               target = "_blank", "GitHub"), "."
      )
    )
  )
)

# ----------------------------- Data tab --------------------------------------
tab_data <- shinydashboard::tabItem(
  tabName = "tab_data",
  fluidRow(
    shinydashboard::box(
      title = "1. Data source", status = "primary", solidHeader = TRUE,
      width = 6,
      radioButtons("data_source", "Source",
                   choices = c("Built-in ggNetView dataset" = "builtin",
                               "Upload file (CSV / TSV / RDS)" = "upload"),
                   selected = "builtin"),
      conditionalPanel(
        "input.data_source == 'builtin'",
        selectInput("builtin_dataset", "Dataset",
                    choices  = GGN_DATASETS,
                    selected = if ("otu_rare_relative" %in% GGN_DATASETS)
                      "otu_rare_relative" else GGN_DATASETS[1])
      ),
      conditionalPanel(
        "input.data_source == 'upload'",
        fileInput("upload_file",
                  "Choose file (CSV / TSV / TXT / RDS / RData)",
                  accept = c(".csv", ".tsv", ".txt", ".rds", ".rda", ".RData")),
        checkboxInput("upload_header", "First row is header", TRUE),
        numericInput("upload_rownames_col",
                     "Column # used as row names (NA = none)",
                     value = 1, min = 0, step = 1)
      ),
      hr(),
      h4("Optional node annotation"),
      fileInput("upload_anno",
                "Upload node annotation (optional, CSV/TSV)",
                accept = c(".csv", ".tsv", ".txt", ".rds")),
      checkboxInput("anno_header", "First row is header", TRUE),
      hr(),
      actionButton("load_data", "Load",
                   icon = icon("upload"),
                   class = "btn-primary")
    ),
    shinydashboard::box(
      title = "2. Loaded object summary", status = "info", solidHeader = TRUE,
      width = 6,
      verbatimTextOutput("data_summary"),
      hr(),
      h4("Preview (first 8 rows / 8 cols)"),
      DT::DTOutput("data_preview")
    )
  )
)

# --------------------------- Build network tab -------------------------------
tab_build <- shinydashboard::tabItem(
  tabName = "tab_build",
  fluidRow(
    shinydashboard::box(
      title = "Build options", status = "primary", solidHeader = TRUE,
      width = 4,
      radioButtons("build_kind", "Input type",
                   choices = c("Numeric matrix (build_graph_from_mat)" = "mat",
                               "Adjacency matrix"                       = "adj",
                               "Edge list data.frame"                   = "df"),
                   selected = "mat"),

      conditionalPanel(
        "input.build_kind == 'mat'",
        selectInput("bg_method",     "Network method",     NETWORK_METHODS,    "WGCNA"),
        selectInput("bg_cor_method", "Correlation method", CORR_METHODS,        "pearson"),
        selectInput("bg_proc",       "p-value adjustment", PADJ_METHODS,        "BH"),
        selectInput("bg_trans",      "Transform method",   TRANSFORM_METHODS,   "none"),
        numericInput("bg_r",         "r threshold",  value = 0.7, min = 0, max = 1, step = 0.05),
        numericInput("bg_p",         "p threshold",  value = 0.05, min = 0, max = 1, step = 0.005),
        selectInput("bg_module",     "Module method",      MODULE_METHODS,      "Fast_greedy"),
        numericInput("bg_top_modules", "Top N modules", value = 15, min = 1, step = 1),
        conditionalPanel(
          "input.bg_method == 'SpiecEasi'",
          selectInput("bg_se_method", "SpiecEasi method", c("mb", "glasso"), "mb")
        ),
        conditionalPanel(
          "input.bg_method == 'SPARCC'",
          numericInput("bg_sparcc_R", "SparCC bootstraps", value = 20, min = 1, step = 1)
        ),
        numericInput("bg_seed", "Random seed", value = 1115, step = 1)
      ),

      conditionalPanel(
        "input.build_kind == 'adj'",
        selectInput("bg_adj_module", "Module method", MODULE_METHODS, "Fast_greedy"),
        numericInput("bg_adj_top",   "Top N modules", value = 15, min = 1, step = 1),
        numericInput("bg_adj_seed",  "Random seed",   value = 1115, step = 1)
      ),

      conditionalPanel(
        "input.build_kind == 'df'",
        checkboxInput("bg_df_directed", "Directed graph", FALSE),
        selectInput("bg_df_module", "Module method", MODULE_METHODS, "Fast_greedy"),
        numericInput("bg_df_top",   "Top N modules", value = 15, min = 1, step = 1),
        numericInput("bg_df_seed",  "Random seed",   value = 1115, step = 1)
      ),

      hr(),
      actionButton("build_go", "Build network",
                   icon = icon("play"),
                   class = "btn-success")
    ),
    shinydashboard::box(
      title = "Network summary", status = "info", solidHeader = TRUE,
      width = 8,
      shinycssloaders::withSpinner(verbatimTextOutput("graph_summary")),
      hr(),
      h4("Module size table"),
      DT::DTOutput("graph_modules"),
      hr(),
      downloadButton("download_graph", "Download graph (.rds)",
                     class = "btn-info")
    )
  )
)

# ---------------------------- Visualize tab ----------------------------------
tab_vis <- shinydashboard::tabItem(
  tabName = "tab_vis",
  fluidRow(
    shinydashboard::box(
      title = "Plot parameters", status = "primary", solidHeader = TRUE,
      width = 4,
      selectInput("vis_layout",        "Layout", LAYOUTS, "gephi"),
      selectInput("vis_layout_module", "Layout module", LAYOUT_MODULES, "adjacent"),
      textInput  ("vis_group_by",      "group.by",  "Modularity"),
      textInput  ("vis_fill_by",       "fill.by",   "Modularity"),
      textInput  ("vis_color_by",      "color.by (optional)", ""),
      sliderInput("vis_pointsize",     "Point size range",
                  min = 0.5, max = 12, value = c(1, 5), step = 0.25),
      checkboxInput("vis_center",      "center",       FALSE),
      checkboxInput("vis_jitter",      "jitter nodes", TRUE),
      numericInput ("vis_jitter_sd",   "jitter sd",   value = 0.15, step = 0.05),
      checkboxInput("vis_mapping_line","mapping_line", TRUE),
      checkboxInput("vis_curve",       "curved edges", FALSE),
      numericInput ("vis_curvature",   "curvature",   value = 0.25, step = 0.05),
      numericInput ("vis_shrink",      "shrink",      value = 0.9, step = 0.05),
      numericInput ("vis_linealpha",   "line alpha",  value = 0.2, step = 0.05),
      textInput   ("vis_linecolor",    "line color", "#d9d9d9"),
      checkboxInput("vis_add_outer",   "add outer ring", TRUE),
      numericInput ("vis_outerwidth",  "outer width", value = 1.25, step = 0.25),
      checkboxInput("vis_label",       "label modules", TRUE),
      textInput   ("vis_pointlabel",   "pointlabel (e.g. top1, NULL)", ""),
      numericInput ("vis_labelsize",   "label size",  value = 10, step = 1),
      numericInput ("vis_seed",        "seed",        value = 1115, step = 1),
      hr(),
      actionButton("vis_render", "Render plot",
                   icon = icon("paint-brush"),
                   class = "btn-success"),
      hr(),
      fluidRow(
        column(6, numericInput("vis_export_w", "Export width (in)",  value = 10)),
        column(6, numericInput("vis_export_h", "Export height (in)", value = 10))
      ),
      downloadButton("vis_download_pdf", "PDF",  class = "btn-info"),
      downloadButton("vis_download_png", "PNG",  class = "btn-info")
    ),
    shinydashboard::box(
      title = "Network plot", status = "info", solidHeader = TRUE,
      width = 8,
      shinycssloaders::withSpinner(
        plotOutput("vis_plot", height = "720px")
      )
    )
  )
)

# ---------------------------- Topology tab -----------------------------------
tab_topo <- shinydashboard::tabItem(
  tabName = "tab_topo",
  fluidRow(
    shinydashboard::box(
      title = "Topology controls", status = "primary", solidHeader = TRUE,
      width = 4,
      h4("Global topology"),
      actionButton("topo_run", "Compute topology",
                   icon = icon("calculator"),
                   class = "btn-success"),
      hr(),
      h4("zi-pi node-role analysis"),
      textInput   ("zipi_modularity_col", "modularity column", "Modularity"),
      textInput   ("zipi_degree_col",     "degree column",      "Degree"),
      numericInput("zipi_zi_thr", "zi threshold", value = 2.5, step = 0.1),
      numericInput("zipi_pi_thr", "pi threshold", value = 0.62, step = 0.01),
      checkboxInput("zipi_na_rm", "drop NA",  FALSE),
      actionButton("zipi_run", "Compute zi-pi",
                   icon = icon("scatter-chart"),
                   class = "btn-success"),
      hr(),
      downloadButton("topo_download", "Download topology (.csv)", class = "btn-info"),
      downloadButton("zipi_download", "Download zi-pi table (.csv)", class = "btn-info")
    ),
    shinydashboard::box(
      title = "Topology output", status = "info", solidHeader = TRUE,
      width = 8,
      h4("Global topology metrics"),
      shinycssloaders::withSpinner(DT::DTOutput("topo_table")),
      hr(),
      h4("zi-pi scatter plot"),
      shinycssloaders::withSpinner(plotOutput("zipi_plot", height = "520px")),
      hr(),
      h4("zi-pi table"),
      DT::DTOutput("zipi_table")
    )
  )
)

# --------------------------- Env-Spec tab ------------------------------------
tab_envspec <- shinydashboard::tabItem(
  tabName = "tab_envspec",
  fluidRow(
    shinydashboard::box(
      title = "Inputs", status = "primary", solidHeader = TRUE, width = 4,
      h4("Environment matrix"),
      radioButtons("env_src", "Source",
                   c("Built-in" = "builtin", "Upload" = "upload"),
                   selected = "builtin"),
      conditionalPanel("input.env_src == 'builtin'",
                       selectInput("env_builtin", "Dataset",
                                   choices  = GGN_DATASETS,
                                   selected = if ("Envdf_4st" %in% GGN_DATASETS)
                                     "Envdf_4st" else GGN_DATASETS[1])),
      conditionalPanel("input.env_src == 'upload'",
                       fileInput("env_upload", "Upload env",
                                 accept = c(".csv", ".tsv", ".rds"))),
      hr(),
      h4("Species matrix"),
      radioButtons("spec_src", "Source",
                   c("Built-in" = "builtin", "Upload" = "upload"),
                   selected = "builtin"),
      conditionalPanel("input.spec_src == 'builtin'",
                       selectInput("spec_builtin", "Dataset",
                                   choices  = GGN_DATASETS,
                                   selected = if ("Spedf" %in% GGN_DATASETS)
                                     "Spedf" else GGN_DATASETS[1])),
      conditionalPanel("input.spec_src == 'upload'",
                       fileInput("spec_upload", "Upload spec",
                                 accept = c(".csv", ".tsv", ".rds"))),
      hr(),
      h4("Block selectors"),
      helpText("Use R-list syntax. Empty means use all columns."),
      textAreaInput("env_select",
                    "env_select (R list)",
                    value = "list(Env01 = 1:14, Env02 = 15:28, Env03 = 29:42, Env04 = 43:56)",
                    rows = 3),
      textAreaInput("spec_select",
                    "spec_select (R list)",
                    value = "list(Spec01 = 1:8)",
                    rows = 2),
      hr(),
      selectInput("link_relation", "Relation method",
                  c("correlation", "mantel"), "correlation"),
      selectInput("link_corm", "Correlation method", CORR_METHODS, "pearson"),
      selectInput("link_layout", "spec layout",
                  c("circle", "circle_outline"), "circle_outline"),
      checkboxInput("link_drop_nonsig", "drop non-significant", FALSE),
      numericInput ("link_r", "r (radius)", value = 6, step = 0.5),
      numericInput ("link_distance", "distance", value = 1, step = 0.25),
      textInput   ("link_orient",
                   "orientation (comma-separated)",
                   "top_right, bottom_right, top_left, bottom_left"),
      hr(),
      actionButton("link_run", "Render heatmap-link",
                   icon = icon("project-diagram"),
                   class = "btn-success"),
      hr(),
      downloadButton("link_download_pdf", "PDF", class = "btn-info")
    ),
    shinydashboard::box(
      title = "Heatmap link plot", status = "info", solidHeader = TRUE, width = 8,
      shinycssloaders::withSpinner(plotOutput("link_plot", height = "720px"))
    )
  )
)

# ------------------------------ About tab ------------------------------------
tab_about <- shinydashboard::tabItem(
  tabName = "tab_about",
  fluidRow(
    shinydashboard::box(
      title = "About", status = "primary", solidHeader = TRUE, width = 12,
      HTML(paste0(
        "<h3>ggNetView Shiny GUI</h3>",
        "<p>Interactive front-end for the <b>ggNetView</b> R package — ",
        "reproducible and deterministic network analysis & visualization.</p>",
        "<ul>",
        "<li><b>Data</b>: load built-in datasets (e.g. <code>otu_rare_relative</code>, ",
        "<code>tax_tab</code>, <code>Envdf_4st</code>, <code>Spedf</code>) or upload your own.</li>",
        "<li><b>Build Network</b>: WGCNA / SparCC / SpiecEasi / cor / Hmisc, ",
        "or directly from adjacency matrix / edge list.</li>",
        "<li><b>Visualize</b>: full parameter coverage of <code>ggNetView()</code>.</li>",
        "<li><b>Topology / zi-pi</b>: <code>get_network_topology()</code> + ",
        "<code>ggnetview_zipi()</code>.</li>",
        "<li><b>Env-Spec Linkage</b>: <code>gglink_heatmaps()</code> with ",
        "multi-block selectors.</li>",
        "</ul>",
        "<p>Manual: <a href='https://jiawang1209.github.io/ggNetView-manual/' target='_blank'>",
        "https://jiawang1209.github.io/ggNetView-manual/</a></p>",
        "<p>Author: Yue Liu &lt;yueliu@iae.ac.cn&gt;</p>"
      ))
    )
  )
)

body <- shinydashboard::dashboardBody(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    tags$link(rel = "icon", type = "image/png", href = "logo.png")
  ),
  shinydashboard::tabItems(
    tab_home, tab_usage,
    tab_data, tab_build, tab_vis, tab_topo, tab_envspec,
    tab_about
  )
)

ui <- shinydashboard::dashboardPage(
  skin   = "blue",     # default skin; only header + sidebar are overridden to pink
  header = header,
  sidebar = sidebar,
  body   = body
)
