# Server function
server <-  function(input, output, session) {
  tabItem(
    tabName = "home_page",
    fluidRow(
      column(
        width = 12,
        align = "center",
        tags$br(),
        img(src = "./www/logo.png", 
            width = "400px", 
            height = "450px"
        ),
        tags$br(),
        tags$br(),
        h1("ggNetView is an R package for network analysis and visualization."),
        h3("It provides flexible and publication-ready tools for exploring complex biological and ecological networks.")
      ),
    ),
    tags$br(),
    tags$hr(),
    fluidRow(
      column(
        width = 12,
        align = "center",
        # slickROutput("myCarousel", width = "50%", height = "250px")
        tags$br(),
        h2("Plot Preview"),
        tags$br(),
        # myCarousel_UI("myCarousel")
      )
    ),
    tags$br(),
    tags$hr(),
    fluidRow(
      column(
        width = 12,
        align = "center",
        box(
          title = "Function Preview",
          status = "info",
          width = 8,
          #background = "gray",
          solidHeader = TRUE,
          fluidRow(
            infoBox(
              title = "Basic Plot", value = 15, icon = icon("bar-chart"),
              color = "orange", fill = FALSE, width = 4
            ),
            infoBox(
              title = "Advance Plot", value = 16, icon = icon("area-chart"),
              color = "orange", fill = FALSE, width = 4
            ),
            infoBox(
              title = "Statistical Analysis", value = 7, icon = icon("calculator"),
              color = "orange", fill = FALSE, width = 4
            )
          ),
          fluidRow(
            infoBox(
              title = "Time-Series", value = 2, icon = icon("line-chart"),
              color = "info", fill = FALSE,width = 4
            ),
            infoBox(
              title = "Map", value = 2, icon = icon("globe"),
              color = "info", fill = FALSE, width = 4
            ),
            infoBox(
              title = "Bioinformatics", value = 6, icon = icon("dna"),
              color = "info", fill = FALSE, width = 4
            )
          ),
          fluidRow(
            infoBox(
              title = "Microbiome", value = 3, icon = icon("bacteria"),
              color = "success", fill = FALSE, width = 4
            ),
            infoBox(
              title = "isotope", value = 3, icon = icon("atom"),
              color = "success", fill = FALSE, width = 4
            ),
            infoBox(
              title = "Agriculture", value = 3, icon = icon("pagelines"),
              color = "success", fill = FALSE, width = 4
            )
          ),
          fluidRow(
            infoBox(
              title = "Highcharter", value = 7, icon = icon("uncharted"),
              color = "success", fill = FALSE, width = 4
            ),
            infoBox(
              title = "Data Transformation", value = 7, icon = icon("table"),
              color = "success", fill = FALSE, width = 4
            ),
            infoBox(
              title = "Developer", value = 5, icon = icon("user"),
              color = "success", fill = FALSE, width = 4
            )
          )
        )
      )
    ),
    tags$br(),
    tags$hr(),

    
    fluidRow(
      column(
        width = 12,
        align = "center",
        h2("User Guide")
      )
    ),
    tags$br(),
    tags$hr(),
    fluidRow(
      align = "center",
      column(
        width = 12,
        h2("Contact")
      )
    ),
    
    fluidRow(
      width = 12,
      tags$head(
        tags$style(
          HTML("
        .scrolling-text-container {
          display: flex;
          justify-content: center;
          align-items: center;
          overflow: hidden;
          height: 50px;
          padding: 10px;
          background-color: #fcc5c0;
        }
        
        .scrolling-text {
          white-space: nowrap;
          animation: scrolling-text 30s linear infinite;
        }
        
        @keyframes scrolling-text {
          0% { transform: translateX(100%); }
          100% { transform: translateX(-100%); }
        }
      ")
        )
      ),
      fluidRow(
        column(
          width = 12,
          align = "center",
          
          div(
            class = "scrolling-text-container",
            div(
              class = "scrolling-text",
              "目前 BGCViewer 仍然处于持续开发和维护阶段，各位用户可以添加开发团队的联系方式，与开发者进行实时的交流和互动，以便对BGCViewer进行共同的维护和新功能的开发。
               您的支持和鼓励是BGCViewer团队开发的动力，祝您科研顺利，一切如意！
              "
            )
          )
        )
      )
      
    ),
    tags$br(),
    tags$br(),
    fluidRow(
      width = 12,
      align = "center",
      column(
        width = 4,
        align = "center",
        # imageOutput("BGCViewer_wechat")
        img(src = "BGCViewer_wechat.jpeg", 
            width = "300px", 
            height = "400px"
        ),
      ),
      column(
        width = 4,
        align = "center",
        # imageOutput("wechat_liuyue")
        img(src = "liuyue_wechat.jpeg", 
            width = "300px", 
            height = "400px"
        ),
      ),
      column(
        width = 4,
        align = "center",
        # imageOutput("wechat_gongzhonghao")
        img(src = "wechat_gongzhonghao.jpeg", 
            width = "300px", 
            height = "400px"
        ),
      )
    ),
    tags$br(),
    tags$hr(),
    fluidPage(
      fluidRow(
        column(
          width = 3
        ),
        column(
          width = 6,
          align = "center",
          h2("Live Statistics"),
          tags$br(),
          HTML('<script type="text/javascript" src="//rf.revolvermaps.com/0/0/7.js?i=5sbhdlkfthp&amp;m=0&amp;c=ff0000&amp;cr1=ffffff&amp;sx=0" async="async"></script>')
        ),
        column(
          width = 3
        )
      )
    )
  )
  
}