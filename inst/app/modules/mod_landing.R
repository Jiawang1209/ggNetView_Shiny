mod_landing_ui <- function(id) {
  ns <- shiny::NS(id)
  app_root <- getOption("ggnetview.app_root", getwd())
  readme_path <- file.path(app_root, "README.md")

  step_card <- function(n, title, body) {
    shiny::div(
      class = "ggnv-step-card",
      shiny::span(class = "ggnv-step-num", n),
      shiny::div(
        shiny::span(class = "ggnv-step-title", title),
        shiny::span(class = "ggnv-step-body", body)
      )
    )
  }

  bslib::card(
    class = "ggnv-landing",
    shiny::div(
      class = "ggnv-hero",
      shiny::img(src = "logo.png", class = "ggnv-hero-logo", alt = "ggNetView"),
      shiny::h1("Welcome to ggNetView", class = "ggnv-hero-title"),
      shiny::p(
        class = "ggnv-hero-tagline",
        "Build, analyze & visualize association networks — reproducibly."
      ),
      shiny::actionButton(
        ns("start_example"),
        label = shiny::tagList(bsicons::bs_icon("play-fill"), "Load example data & go to Builder"),
        class = "btn btn-primary btn-lg ggnv-hero-cta"
      )
    ),
    shiny::div(
      class = "ggnv-quickstart",
      shiny::div(class = "ggnv-quickstart-label", "Quick start · 3 steps"),
      step_card("1", "Load data", "Upload a matrix / edge table, or pick a bundled example."),
      step_card("2", "Build graph", "Correlation, RMT-assisted, or WGCNA / TOM."),
      step_card("3", "Visualize & analyze", "Layouts, topology, Zi-Pi, perturbation.")
    ),
    bslib::accordion(
      open = FALSE,
      bslib::accordion_panel(
        "Learn more about ggNetView",
        if (file.exists(readme_path)) {
          shiny::div(class = "ggnv-introduction", shiny::includeMarkdown(readme_path))
        } else {
          shiny::p("README not found.")
        }
      )
    )
  )
}

mod_landing_server <- function(id, registry) {
  # Returns a reactive for the start_example button click.
  # The caller (server.R) observes this reactive and calls
  # register_gallery_examples(registry) to load bundled examples.
  shiny::moduleServer(id, function(input, output, session) {
    shiny::reactive(input$start_example)
  })
}
