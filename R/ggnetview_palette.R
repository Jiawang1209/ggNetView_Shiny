# Canonical ordering for discrete classes.
# For factors, return levels that actually occur (preserving the factor's own
# order). For non-factors, fall back to a lexicographic sort of the unique
# values. Used by point / label / mask scales so a given column always produces
# the same palette regardless of which site first observed it.
.ggnv_class_order <- function(x) {
  if (length(x) == 0L) return(character())
  if (is.factor(x)) {
    lv <- levels(x)
    present <- unique(as.character(stats::na.omit(x)))
    return(lv[lv %in% present])
  }
  sort(unique(stats::na.omit(as.character(x))))
}

#' Generate a named color palette for discrete classes
#'
#' @param classes Character string.
#' The discrete class names or factor levels to map to colors.
#' @param others_label Character, (default = "Others").
#'
#' @returns A named character vector whose names are the discrete classes
#' and whose values are hex colors. If `others_label` appears in `classes`,
#' it is always mapped to a neutral grey so "Others" has a stable color.
#' @export
#'
#' @examples
#' get_palette(c("Module1", "Module2", "Module3"))
#' get_palette(c("Module1", "Module2", "Others"))
#'
get_palette <- function(classes, others_label = "Others") {
  base_colors <- c(
    '#8dd3c7','#ffffb3','#bebada','#fb8072','#80b1d3',
    '#fdb462','#b3de69','#fccde5','#cab2d6','#bc80bd',
    '#ccebc5','#ffed6f','#a6cee3','#b2df8a','#fb9a99',
    '#bdbdbd','#1f78b4','#33a02c','#e31a1c','#fdbf6f',
    '#ff7f00','#6a3d9a','#ffff99','#b15928'
  )

  classes <- as.character(classes)
  uniq_classes <- unique(stats::na.omit(classes))

  has_others <- others_label %in% uniq_classes
  uniq_main  <- setdiff(uniq_classes, others_label)

  color_map <- setNames(
    rep(base_colors, length.out = length(uniq_main)),
    uniq_main
  )

  if (has_others) {
    color_map <- c(color_map, setNames("#bdbdbd", others_label))
  }
  color_map
}


#' Custom discrete color scale for ggNetView
#'
#' @param classes Character string.
#' The discrete class names or factor levels to map to colors.
#' @param ... Additional arguments passed to `ggplot2::scale_color_manual()`.
#' @param others_label Character, (default = "Others").
#' @param na_value Color for missing values. Default `"#e0e0e0"`.
#' @param drop Logical, passed to `ggplot2::scale_color_manual()`.
#'
#' @returns A `ggplot2` scale object.
#' @export
#'
#' @examples
#' library(ggplot2)
#' df <- data.frame(
#'   x = 1:4, y = 1:4,
#'   group = c("Module1", "Module2", "Module3", "Others")
#' )
#' ggplot(df, aes(x, y, color = group)) +
#'   geom_point(size = 6) +
#'   scale_color_ggnetview(df$group)
#'
scale_color_ggnetview <- function(classes,
                                  ...,
                                  others_label = "Others",
                                  na_value = "#e0e0e0",
                                  drop = FALSE) {
  pal <- get_palette(classes, others_label = others_label)
  ggplot2::scale_color_manual(values = pal, na.value = na_value, drop = drop, ...)
}

#' Custom discrete fill scale for ggNetView
#'
#' @param classes Character string.
#' The discrete class names or factor levels to map to colors.
#' @param ... Additional arguments passed to `ggplot2::scale_color_manual()`.
#' @param others_label Character, (default = "Others").
#' @param na_value Color for missing values. Default `"#e0e0e0"`.
#' @param drop Logical, passed to `ggplot2::scale_color_manual()`.
#'
#' @returns A `ggplot2` scale object.
#' @export
#'
#' @examples
#' library(ggplot2)
#' df <- data.frame(
#'   x = 1:4, y = 1:4,
#'   group = c("Module1", "Module2", "Module3", "Others")
#' )
#' ggplot(df, aes(x, y, fill = group)) +
#'   geom_point(shape = 21, size = 8) +
#'   scale_fill_ggnetview(df$group)
scale_fill_ggnetview <- function(classes,
                                 ...,
                                 others_label = "Others",
                                 na_value = "#e0e0e0",
                                 drop = FALSE) {
  pal <- get_palette(classes, others_label = others_label)
  ggplot2::scale_fill_manual(values = pal, na.value = na_value, drop = drop, ...)
}
