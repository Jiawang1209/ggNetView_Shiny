#' Empty-state guidance card
#'
#' Shown when a panel has no object/data selected yet.
#' @param icon bsicons icon name.
#' @param title Short headline.
#' @param hint One-line guidance text.
#' @param action Optional UI (e.g. an actionButton) shown below the hint.
#' @keywords internal
ui_empty_state <- function(icon = "inbox", title = "Nothing here yet",
                           hint = "", action = NULL) {
  shiny::div(
    class = "ggnv-empty-state",
    shiny::div(class = "ggnv-empty-state-icon", bsicons::bs_icon(icon)),
    shiny::div(class = "ggnv-empty-state-title", title),
    if (nzchar(hint)) shiny::div(class = "ggnv-empty-state-hint", hint),
    if (!is.null(action)) shiny::div(class = "ggnv-empty-state-action", action)
  )
}

#' Brand value box metric card
#'
#' @param title Metric label.
#' @param value Metric value (string or number).
#' @param icon bsicons icon name.
#' @param theme Background theme passed to bslib::value_box (default white card).
#' @keywords internal
ggnv_value_box <- function(title, value, icon = "graph-up",
                           theme = bslib::value_box_theme(bg = "#FFFFFF", fg = "#7D0159")) {
  bslib::value_box(
    title = title,
    value = value,
    showcase = bsicons::bs_icon(icon),
    theme = theme,
    class = "ggnv-value-box"
  )
}

#' Engineering-grade DT::datatable wrapper
#'
#' Adds copy/csv/excel buttons, horizontal scroll, sensible paging, and
#' rounds numeric columns to 3 digits.
#' @param df A data.frame.
#' @param page_length Rows per page.
#' @param digits Rounding for numeric columns.
#' @param ... Passed to DT::datatable.
#' @keywords internal
dt_table <- function(df, page_length = 10, digits = 3, ...) {
  df <- as.data.frame(df, check.names = FALSE)
  dt <- DT::datatable(
    df,
    extensions = "Buttons",
    rownames = FALSE,
    options = list(
      dom = "Bfrtip",
      buttons = c("copy", "csv", "excel"),
      scrollX = TRUE,
      pageLength = page_length,
      lengthMenu = c(10, 25, 50, 100)
    ),
    ...
  )
  num_cols <- names(df)[vapply(df, is.numeric, logical(1))]
  if (length(num_cols)) {
    dt <- DT::formatRound(dt, columns = num_cols, digits = digits)
  }
  dt
}

#' Unified toast notification
#'
#' @param message Text to show.
#' @param type One of "default","message","warning","error".
#' @param duration Seconds (NULL = sticky).
#' @keywords internal
notify <- function(message, type = "message", duration = 5) {
  shiny::showNotification(message, type = type, duration = duration)
}
