#' Brand bslib theme for the ggNetView Shiny app
#'
#' Custom Bootstrap 5 theme using the ggNetView magenta brand color.
#' @return A `bs_theme` object.
#' @keywords internal
app_bs_theme <- function() {
  bslib::bs_theme(
    version = 5,
    primary = "#AE017E",
    base_font = bslib::font_collection(
      bslib::font_google("Inter", local = FALSE),
      "system-ui", "-apple-system", "Segoe UI", "Roboto", "sans-serif"
    ),
    heading_font = bslib::font_collection(
      bslib::font_google("Inter", local = FALSE),
      "system-ui", "sans-serif"
    ),
    "link-color" = "#AE017E",
    "navbar-light-active-color" = "#7D0159"
  )
}

#' Categorical module color palette (from the ggNetView logo)
#'
#' @return A named character vector of 8 hex colors.
#' @keywords internal
app_module_palette <- function() {
  c(
    red    = "#F08D8D",
    yellow = "#F2D24D",
    orange = "#F2A65A",
    green  = "#9FD18B",
    teal   = "#6FC4C0",
    blue   = "#7FB3E0",
    purple = "#C9A0DC",
    pink   = "#E89BC4"
  )
}
