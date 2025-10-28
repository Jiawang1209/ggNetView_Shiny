source("R/global.R")

ui <- bs4DashPage(
  title  = "ggNetView : an R package for network analysis and visualization.",
  header = bs4DashNavbar(
    skin = "light",
    brand = bs4DashBrand(
      title = "ggNetView",
      color = "pink",
      href  = "https://github.com/Jiawang1209/ggNetView",
      image = "https://github.com/Jiawang1209/ggNetView/tree/main/man/figures/logo.png"     # 放在 www/logo.png
    )
  ),
  sidebar = bs4DashSidebar(
    width = 220,
    bs4SidebarUserPanel(
      image = "https://github.com/Jiawang1209/ggNetView/tree/main/man/figures/logo.png",
      name  = "Welcome ggNetView!"
    ),
    bs4SidebarMenu(
      id = "sidebarmenu",
      bs4SidebarMenuItem("Home Page",   tabName = "home_page",   icon = icon("house")),
      bs4SidebarMenuItem("Introduction", tabName = "introduction", icon = icon("question")),
      bs4SidebarMenuItem("Microbial Network Analysis", tabName = "MNA", icon = icon("bacteria")),
      bs4SidebarMenuItem("Protein Network Analysis", tabName = "PPI", icon = icon("dna")),
      bs4SidebarMenuItem("WGCNA", tabName = "WGCNA", icon = icon("circle-nodes"))
    )
  ),
  body = bs4DashBody(
    bs4TabItems(
      bs4TabItem(
        tabName = "home_page",
        h3("Home works.")
      ),
      bs4TabItem(
        tabName = "introduction",
        fluidRow(
          column(1),
          column(10, includeMarkdown("README.md")),  # 确保有这个文件
          column(1)
        )
      )
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