source("R/global.R")

ui <- bs4DashPage(
  title  = "ggNetView : an R package for network analysis and visualization.",
  header = bs4DashNavbar(
    skin = "light",
    brand = bs4DashBrand(
      title = "ggNetView",
      color = "pink",
      href  = "https://github.com/Jiawang1209/ggNetView",
      image = "logo.png"     # 放在 www/logo.png
    )
  ),
  sidebar = bs4DashSidebar(
    width = 220,
    bs4SidebarUserPanel(
      image = "logo.png",
      name  = "Welcome ggNetView!"
    ),
    bs4SidebarMenu(
      id = "sidebarmenu",
      bs4SidebarMenuItem("Home Page", tabName = "home_page", icon = icon("house")),
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
      bs4TabItem(
        tabName = "home_page",
        # 把你在 server 中写的所有 UI 元素粘回这里
        fluidRow(
          column(
            width = 12, align = "center",
            tags$br(),
            img(src = "logo.png", width = "400px", height = "450px"),
            tags$br(), tags$br(),
            h1("ggNetView : an R package for network analysis and visualization."),
            h3("It provides flexible and publication-ready tools for exploring complex biological and ecological networks.")
          )
        ),
        tags$br(), tags$hr()
        # ... 后面所有的 UI 内容 ...
      ),
      bs4TabItem(
        tabName = "introduction",
        fluidRow(
          # column(1),
          column(
            12,
            tags$iframe(
              src   = "README.html",     # 注意：文件需放在 www/ 目录
              width = "100%",
              height = "1000px",
              style = "border:none;"
            )
          ),
          # column(1)
        )
      ),
      # --- Microbial Network (MNA) ---
      bs4TabItem(
        tabName = "MNA",
        fluidRow(
          uploadBoxUI("up_mna",
                      title = "Microbial Network 数据上传",
                      help_text = HTML("常见输入：<ul>
          <li>丰度表（样本 × ASV/OTU）</li>
          <li>可选：taxonomy 注释表</li>
        </ul>")
                      )
          ),
        # create graph obj UI
        fluidRow(
          bs4Card(
            title = "参数设置（直连 build_graph_from_mat + ggNetView）", status = "primary",
            width = 12,
            checkboxInput("mna_transpose", "将数据转置（样本在行、变量在列）", FALSE),
            
            # —— build_graph_from_mat 的关键参数 —— 
            selectInput("mna_transfrom", "transfrom.method",
                        choices = c("none","scale","center","log2","log10","ln","rrarefy","rrarefy_relative"),
                        selected = "log10"),
            selectInput("mna_method", "method",
                        choices = c("cor","WGCNA","SpiecEasi","SPARCC"),  # 注意：你的函数内部分支需支持 "SPARCC"
                        selected = "cor"),
            selectInput("mna_cor_method", "cor.method",
                        choices = c("spearman","pearson","kendall"),
                        selected = "spearman"),
            selectInput("mna_proc", "多重校正（proc）",
                        choices = c("BH","Bonferroni","Holm","Hochberg","SidakSS","SidakSD","BY","ABH","TSBH"),
                        selected = "BH"),
            selectInput("mna_module", "module.method",
                        choices = c("Walktrap","Fast_greedy","Edge_betweenness","Spinglass"),
                        selected = "Walktrap"),
            selectInput("mna_spieceasi", "SpiecEasi.method",
                        choices = c("mb","glasso"), selected = "mb"),
            numericInput("mna_r", "r.threshold（|r| ≥）", value = 0.6, min = 0, max = 1, step = 0.01),
            numericInput("mna_p", "p.threshold（p <）",  value = 0.05, min = 0, max = 1, step = 0.001),
            numericInput("mna_topk", "top_modules", value = 15, min = 1, step = 1),
            
            hr(),
            actionButton("mna_build", "构建网络", class = "btn btn-success"),
            div(class="text-muted", style="margin-top:8px;", textOutput("mna_status", inline = TRUE))
          )
        ),
        
        fluidRow(
          bs4Card(
            title = "可视化（ggNetView）", width = 12, status = "primary",
            # —— ggNetView 绘图参数（挑常用）——
            selectInput("mna_layout", "layout",
                        choices = c("star_concentric","star","petal","diamond","square","square2",
                                    "heart_centered","rectangle","rightiso_layers","gephi"),
                        selected = "star_concentric"),
            selectInput("mna_layout_module", "layout.module",
                        choices = c("random","adjacent"), selected = "random"),
            selectInput("mna_orientation", "orientation",
                        choices = c("up","down","left","right"), selected = "up"),
            sliderInput("mna_angle", "angle（弧度微调）", min = -pi, max = pi, value = 0, step = 0.05),
            checkboxInput("mna_label", "显示模块标签（label）", FALSE),
            checkboxInput("mna_add_outer", "显示模块外圈（add_outer）", FALSE),
            checkboxInput("mna_remove_others", "去除 Others 模块", FALSE),
            checkboxInput("mna_mapping_line", "边按相关正/负着色（mapping_line）", TRUE),
            sliderInput("mna_pointsize", "节点大小范围（ggplot2 size）", min = 0.5, max = 15, value = c(2,9), step = 0.5),
            
            hr(),
            plotOutput("mna_plot", height = "640px"),
            br(),
            fluidRow(
              column(3, downloadButton("mna_dl_nodes", "下载节点表")),
              column(3, downloadButton("mna_dl_edges", "下载边表")),
              column(3, downloadButton("mna_dl_png",   "下载PNG")),
              column(3, downloadButton("mna_dl_pdf",   "下载PDF"))
            )
          )
        )
      ),
      
      # --- Protein Network (PPI) ---
      bs4TabItem(
        tabName = "PPI",
        fluidRow(
          uploadBoxUI("up_ppi",
                      title = "Protein/Gene Interaction 数据上传",
                      help_text = HTML("常见输入：<ul>
          <li>边列表：两列 (ProteinA, ProteinB)</li>
          <li>可选：节点注释表（表达量/功能/通路等）</li>
        </ul>")
          )
          # 这里继续加你的其他 UI ...
        )
      ),
      
      # --- WGCNA ---
      bs4TabItem(
        tabName = "WGCNA",
        fluidRow(
          uploadBoxUI("up_wgcna",
                      title = "WGCNA 数据上传",
                      help_text = HTML("常见输入：<ul>
          <li>表达矩阵（基因 × 样本）</li>
          <li>可选：样本性状表（traits）</li>
        </ul>")
          )
          # 这里继续加你的其他 UI ...
        )
      ),
      
      # --- Env-Link ---
      bs4TabItem(
        tabName = "EnvLink",
        fluidRow(
          uploadBoxUI("up_env",
                      title = "Env-Link 数据上传",
                      help_text = HTML("常见输入：<ul>
          <li>环境数据表（样本 × 环境变量）</li>
          <li>可选：物种/功能丰度表，便于相关/Mantel 链接</li>
        </ul>")
          )
          # 这里继续加你的其他 UI ...
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
    skinSelector(),   # 这是一个“会返回标签”的函数，必须以函数调用形式出现
    pinned = TRUE
  ),
  footer = bs4DashFooter(
    left  = tags$a(
      href   = "https://github.com/Jiawang1209/ggNetView",
      target = "_blank",
      "版权所有 © Jiawang1209/ggNetView"
    ),
    right = "2025",
    fixed = TRUE
  ) 
)