source("R/global.R")

ui <- bs4DashPage(
  title  = "ggNetView: An R package for network analysis and visualization",
  header = bs4DashNavbar(
    skin = "light",
    brand = bs4DashBrand(
      title = "ggNetView",
      color = "pink",
      href  = "https://github.com/Jiawang1209/ggNetView",
      image = "logo.png"
    )
  ),
  sidebar = bs4DashSidebar(
    width = 220,
    bs4SidebarUserPanel(
      image = "logo.png",
      name  = "Welcome to ggNetView!"
    ),
    bs4SidebarMenu(
      id = "sidebarmenu",
      bs4SidebarMenuItem("Home", tabName = "home_page", icon = icon("house")),
      bs4SidebarMenuItem("Introduction", tabName = "introduction", icon = icon("book")),
      bs4SidebarMenuItem("Microbial Network", tabName = "MNA", icon = icon("bacteria")),
      bs4SidebarMenuItem("Protein Network", tabName = "PPI", icon = icon("dna")),
      bs4SidebarMenuItem("WGCNA", tabName = "WGCNA", icon = icon("circle-nodes")),
      bs4SidebarMenuItem("Env-Link", tabName = "EnvLink", icon = icon("earth-americas")),
      bs4SidebarMenuItem("Developer", tabName = "Developer", icon = icon("users")),
      bs4SidebarMenuItem("Citation", tabName = "Citation", icon = icon("copyright"))
    )
  ),
  body = bs4DashBody(
    bs4TabItems(
      # --- Home Page ---
      bs4TabItem(
        tabName = "home_page",
        fluidRow(
          column(
            width = 12, align = "center",
            tags$br(),
            img(src = "logo.png", width = "400px", height = "450px"),
            tags$br(), tags$br(),
            h1("ggNetView: An R package for network analysis and visualization"),
            
            h3("Providing flexible and publication-ready tools for exploring complex biological and ecological networks.")
          )
        ),
        tags$br(), tags$hr()
      ),
      
      # --- Introduction ---
      bs4TabItem(
        tabName = "introduction",
        fluidRow(
          column(
            12,
            tags$iframe(
              src   = "README.html",
              width = "100%",
              height = "1000px",
              style = "border:none;"
            )
          )
        )
      ),
      
      # --- Microbial Network (MNA) ---
      bs4TabItem(
        tabName = "MNA",
        fluidRow(
          uploadBoxUI(
            "up_mna",
            title = "Microbial Network Data Upload",
            help_text = HTML("<ul>
              <li>Abundance table (Samples × ASVs/OTUs)</li>
              <li>Optional: taxonomy annotation table</li>
            </ul>")
          )
        ),
        fluidRow(
          bs4Card(
            title = "Parameter Settings (build_graph_from_mat + ggNetView)", status = "primary",
            width = 12,
            checkboxInput("mna_transpose", "Transpose data (samples in rows, variables in columns)", FALSE),
            
            selectInput("mna_transfrom", "Data transformation method",
                        choices = c("none","scale","center","log2","log10","ln","rrarefy","rrarefy_relative"),
                        selected = "log10"),
            selectInput("mna_method", "Network construction method",
                        choices = c("cor","WGCNA","SpiecEasi","SPARCC"),
                        selected = "cor"),
            selectInput("mna_cor_method", "Correlation method",
                        choices = c("spearman","pearson","kendall"),
                        selected = "spearman"),
            selectInput("mna_proc", "Multiple testing correction",
                        choices = c("BH","Bonferroni","Holm","Hochberg","SidakSS","SidakSD","BY","ABH","TSBH"),
                        selected = "BH"),
            selectInput("mna_module", "Module detection method",
                        choices = c("Walktrap","Fast_greedy","Edge_betweenness","Spinglass"),
                        selected = "Walktrap"),
            selectInput("mna_spieceasi", "SpiecEasi method",
                        choices = c("mb","glasso"), selected = "mb"),
            numericInput("mna_r", "r.threshold (|r| ≥)", value = 0.6, min = 0, max = 1, step = 0.01),
            numericInput("mna_p", "p.threshold (p <)",  value = 0.05, min = 0, max = 1, step = 0.001),
            numericInput("mna_topk", "Top modules", value = 15, min = 1, step = 1),
            
            hr(),
            actionButton("mna_build", "Build Network", class = "btn btn-success"),
            div(class="text-muted", style="margin-top:8px;", textOutput("mna_status", inline = TRUE))
          )
        ),
        
        fluidRow(
          bs4Card(
            title = "Visualization (ggNetView)", width = 12, status = "primary",
            selectInput("mna_layout", "Layout",
                        choices = c("star_concentric","star","petal","diamond","square","square2",
                                    "heart_centered","rectangle","rightiso_layers","gephi"),
                        selected = "star_concentric"),
            selectInput("mna_layout_module", "Module layout mode",
                        choices = c("random","adjacent"), selected = "random"),
            selectInput("mna_orientation", "Orientation",
                        choices = c("up","down","left","right"), selected = "up"),
            sliderInput("mna_angle", "Angle (radian adjustment)", min = -pi, max = pi, value = 0, step = 0.05),
            checkboxInput("mna_label", "Show module labels", FALSE),
            checkboxInput("mna_add_outer", "Add outer module boundary", FALSE),
            checkboxInput("mna_remove_others", "Remove 'Others' module", FALSE),
            checkboxInput("mna_mapping_line", "Color edges by correlation sign", TRUE),
            sliderInput("mna_pointsize", "Node size range", min = 0.5, max = 15, value = c(2,9), step = 0.5),
            hr(),
            div(style = "display:flex;gap:10px;align-items:center;",
                actionButton("mna_do_plot", "Plot", class = "btn btn-primary"),
                span(class="text-muted", textOutput("mna_plot_status", inline = TRUE))
            ),
            hr(),
            plotOutput("mna_plot", height = "640px"),
            br(),
            fluidRow(
              column(3, downloadButton("mna_dl_nodes", "Download Nodes")),
              column(3, downloadButton("mna_dl_edges", "Download Edges")),
              column(3, downloadButton("mna_dl_png",   "Download PNG")),
              column(3, downloadButton("mna_dl_pdf",   "Download PDF"))
            )
          )
        )
      ),
      
      # --- Protein Network (PPI) ---
      bs4TabItem(
        tabName = "PPI",
        fluidRow(
          uploadBoxUI("up_ppi",
                      title = "Protein/Gene Interaction Data Upload",
                      help_text = HTML("<ul>
                        <li>Edge list: two columns (ProteinA, ProteinB)</li>
                        <li>Optional: node annotation table (expression/function/pathway)</li>
                      </ul>")
          )
        )
      ),
      
      # --- WGCNA ---
      bs4TabItem(
        tabName = "WGCNA",
        fluidRow(
          uploadBoxUI("up_wgcna",
                      title = "WGCNA Data Upload",
                      help_text = HTML("<ul>
                        <li>Expression matrix (genes × samples)</li>
                        <li>Optional: trait table</li>
                      </ul>")
          )
        )
      ),
      
      # --- Env-Link ---
      bs4TabItem(
        tabName = "EnvLink",
        fluidRow(
          uploadBoxUI("up_env",
                      title = "Env-Link Data Upload",
                      help_text = HTML("<ul>
                        <li>Environmental data (samples × variables)</li>
                        <li>Optional: species or functional abundance table for correlation/Mantel link</li>
                      </ul>")
          )
        )
      )
    ),
    bs4TabItem(
      tabName = "Developer",
      fluidRow()
    )
  ),
  controlbar = bs4DashControlbar(
    collapsed = TRUE,
    skinSelector(),
    pinned = TRUE
  ),
  footer = bs4DashFooter(
    left  = tags$a(
      href   = "https://github.com/Jiawang1209/ggNetView",
      target = "_blank",
      "© 2025 Jiawang1209 / ggNetView"
    ),
    right = "Version 2025",
    fixed = TRUE
  )
)
