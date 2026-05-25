write_registry_table <- function(data, path) {
  utils::write.csv(data, path, row.names = FALSE)
  invisible(path)
}

write_registry_object <- function(data, path) {
  saveRDS(data, path)
  invisible(path)
}

write_registry_params <- function(params, path) {
  jsonlite::write_json(params, path, pretty = TRUE, auto_unbox = TRUE, null = "null")
  invisible(path)
}

write_plot_png <- function(plot, path, width = 8, height = 6, dpi = 300) {
  ggplot2::ggsave(filename = path, plot = plot, width = width, height = height, dpi = dpi)
  invisible(path)
}

write_plot_pdf <- function(plot, path, width = 8, height = 6) {
  ggplot2::ggsave(filename = path, plot = plot, width = width, height = height, device = grDevices::cairo_pdf)
  invisible(path)
}
