#' Update module names in an existing graph object
#'
#' @param graph_obj A graph object returned by \code{build_graph_*()}.
#' @param modules A named vector or a data frame used to rename modules.
#'   The mapping is \code{old_module -> new_module}.
#' @param old_col Character. Old-module column name in \code{modules} when
#'   \code{modules} is a data frame. Default \code{NULL} means auto-detect.
#' @param new_col Character. New-module column name in \code{modules} when
#'   \code{modules} is a data frame. Default \code{NULL} means auto-detect.
#' @param levels Character vector. Optional explicit module order.
#'   If \code{NULL}, order by module size (descending) and place
#'   \code{"Others"} at the end.
#' @param allow_partial Logical (default = \code{TRUE}).
#'   If \code{TRUE}, modules without rename rules keep original names.
#'   If \code{FALSE}, all existing modules must be covered by \code{modules}.
#'
#' @returns A new graph object with updated module-related node attributes:
#'   \code{Modularity}, \code{modularity2}, and \code{modularity3}
#'   (and \code{modularity} if that column exists).
#' @export
#'
#' @examples
#' data(ppi_example)
#' obj <- build_graph_from_df(
#'   df              = ppi_example$ppi,
#'   node_annotation = ppi_example$annotation
#' )
#' obj2 <- update_graph_modules(
#'   graph_obj = obj,
#'   modules   = c("1" = "ModuleA", "2" = "ModuleB")
#' )
#' levels(get_graph_nodes(obj2)$Modularity)
update_graph_modules <- function(graph_obj,
                                 modules,
                                 old_col = NULL,
                                 new_col = NULL,
                                 levels = NULL,
                                 allow_partial = TRUE) {
  if (!inherits(graph_obj, "tbl_graph")) {
    stop("`graph_obj` must be a `tbl_graph` object.", call. = FALSE)
  }

  # Track whether the user has explicitly customised module order;
  # downstream layout helpers (e.g. create_layout_multirings) read this marker
  # to decide whether to respect the factor levels or fall back to default
  # size-based ordering.
  user_supplied_levels <- !is.null(levels)

  node_df <- graph_obj %>%
    tidygraph::activate(nodes) %>%
    tidygraph::as_tibble()

  current_mod <- if ("Modularity" %in% colnames(node_df)) {
    as.character(node_df$Modularity)
  } else if ("modularity3" %in% colnames(node_df)) {
    as.character(node_df$modularity3)
  } else if ("modularity2" %in% colnames(node_df)) {
    as.character(node_df$modularity2)
  } else {
    stop("`graph_obj` must contain one of: `Modularity`, `modularity3`, `modularity2`.", call. = FALSE)
  }

  current_levels <- unique(current_mod)

  if (is.data.frame(modules)) {
    if (ncol(modules) < 2) {
      stop("`modules` data.frame must contain at least two columns.", call. = FALSE)
    }
    if (is.null(old_col)) {
      old_candidates <- c("old", "from", "old_module", "Modularity", "modularity2", "modularity3")
      hit_old <- old_candidates[old_candidates %in% colnames(modules)]
      old_col <- if (length(hit_old) > 0) hit_old[1] else colnames(modules)[1]
    }
    if (is.null(new_col)) {
      new_candidates <- c("new", "to", "new_module", "module", "Modularity_new")
      hit_new <- new_candidates[new_candidates %in% colnames(modules)]
      if (length(hit_new) > 0) {
        new_col <- hit_new[1]
      } else {
        new_col <- setdiff(colnames(modules), old_col)[1]
      }
    }
    if (!old_col %in% colnames(modules) || !new_col %in% colnames(modules)) {
      stop("`old_col`/`new_col` not found in `modules`.", call. = FALSE)
    }

    map_df <- modules %>%
      dplyr::select(all_of(c(old_col, new_col))) %>%
      purrr::set_names(c("old_modularity", "new_modularity")) %>%
      dplyr::mutate(
        old_modularity = as.character(old_modularity),
        new_modularity = as.character(new_modularity)
      )
  } else if (is.atomic(modules) && !is.null(names(modules))) {
    map_df <- data.frame(
      old_modularity = as.character(names(modules)),
      new_modularity = as.character(modules),
      stringsAsFactors = FALSE
    )
  } else {
    stop("`modules` must be a data.frame or a named vector.", call. = FALSE)
  }

  if (any(is.na(map_df$old_modularity)) || any(map_df$old_modularity == "")) {
    stop("Old module labels in `modules` must be non-empty and not NA.", call. = FALSE)
  }
  if (any(is.na(map_df$new_modularity)) || any(map_df$new_modularity == "")) {
    stop("New module labels in `modules` must be non-empty and not NA.", call. = FALSE)
  }
  if (anyDuplicated(map_df$old_modularity)) {
    dup <- unique(map_df$old_modularity[duplicated(map_df$old_modularity)])
    stop(sprintf("Duplicated old module labels in `modules`: %s", paste(dup, collapse = ", ")), call. = FALSE)
  }

  unknown_old <- setdiff(map_df$old_modularity, current_levels)
  if (length(unknown_old) > 0) {
    stop(sprintf("Old module labels not found in `graph_obj`: %s", paste(unknown_old, collapse = ", ")), call. = FALSE)
  }

  map_key <- map_df$old_modularity
  map_val <- map_df$new_modularity
  idx <- match(current_mod, map_key)
  renamed <- ifelse(is.na(idx), NA_character_, map_val[idx])

  if (isTRUE(allow_partial)) {
    new_mod <- ifelse(is.na(renamed), current_mod, renamed)
  } else {
    if (any(is.na(renamed))) {
      miss_mod <- unique(current_mod[is.na(renamed)])
      stop(sprintf("Missing rename rules for module(s): %s", paste(miss_mod, collapse = ", ")), call. = FALSE)
    }
    new_mod <- renamed
  }

  if (is.null(levels)) {
    level_tab <- sort(table(new_mod), decreasing = TRUE)
    levels_final <- names(level_tab)
  } else {
    levels_final <- unique(as.character(levels))
    missing_levels <- setdiff(unique(new_mod), levels_final)
    levels_final <- c(levels_final, missing_levels)
  }
  levels_final <- c(setdiff(levels_final, "Others"), intersect("Others", levels_final))

  new_mod_factor <- factor(new_mod, levels = levels_final, ordered = TRUE)

  graph_obj_new <- graph_obj %>%
    tidygraph::activate(nodes) %>%
    tidygraph::mutate(
      Modularity = new_mod_factor,
      modularity2 = new_mod_factor,
      modularity3 = as.character(new_mod_factor)
    )

  if ("modularity" %in% colnames(node_df)) {
    graph_obj_new <- graph_obj_new %>%
      tidygraph::activate(nodes) %>%
      tidygraph::mutate(modularity = new_mod_factor)
  }

  if ("Degree" %in% colnames(node_df)) {
    graph_obj_new <- graph_obj_new %>%
      tidygraph::activate(nodes) %>%
      tidygraph::arrange(Modularity, dplyr::desc(Degree))
  } else {
    graph_obj_new <- graph_obj_new %>%
      tidygraph::activate(nodes) %>%
      tidygraph::arrange(Modularity)
  }

  # Mark whether the user supplied an explicit `levels` argument so that
  # downstream layout helpers can decide whether to honour the order.
  graph_obj_new <- igraph::set_graph_attr(
    graph_obj_new, ".modularity_user_ordered", user_supplied_levels
  )

  return(graph_obj_new)
}


#' Update module assignments from an existing column in graph object
#'
#' Uses the values of an existing node column as the new module assignment,
#' updating \code{Modularity}, \code{modularity2}, and \code{modularity3}
#' for seamless integration with \code{ggNetView()}. All other node columns
#' are preserved.
#'
#' @param graph_obj A graph object returned by \code{build_graph_*()}.
#' @param modules_new Character. Name of an existing column in \code{graph_obj}
#'   nodes whose values define the new module assignment. The column may or may
#'   not contain \code{"Others"}.
#' @param levels Character vector. Optional explicit module order.
#'   If \code{NULL}, order by module size (descending) and place
#'   \code{"Others"} at the end when present.
#'
#' @returns A new graph object with updated \code{Modularity}, \code{modularity2},
#'   and \code{modularity3} (and \code{modularity} if that column exists).
#'   All other node columns are preserved.
#' @export
#'
#' @examples
#' data(ppi_example)
#' obj <- build_graph_from_df(
#'   df              = ppi_example$ppi,
#'   node_annotation = ppi_example$annotation
#' )
#' # Re-assign modularity from the existing `group` node column.
#' obj2 <- update_graph_modules2(graph_obj = obj, modules_new = "group")
#' levels(get_graph_nodes(obj2)$Modularity)
update_graph_modules2 <- function(graph_obj,
                                 modules_new,
                                 levels = NULL) {
  # Track whether the user has explicitly customised module order;
  # downstream layout helpers (e.g. create_layout_multirings) read this marker
  # to decide whether to respect the factor levels or fall back to default
  # size-based ordering.
  user_supplied_levels <- !is.null(levels)
  if (!inherits(graph_obj, "tbl_graph")) {
    stop("`graph_obj` must be a `tbl_graph` object.", call. = FALSE)
  }

  node_df <- graph_obj %>%
    tidygraph::activate(nodes) %>%
    tidygraph::as_tibble()

  if (!is.character(modules_new) || length(modules_new) != 1 || is.na(modules_new)) {
    stop("`modules_new` must be a single character string (column name).", call. = FALSE)
  }
  if (!modules_new %in% colnames(node_df)) {
    stop(sprintf("Column `%s` not found in `graph_obj` nodes.", modules_new), call. = FALSE)
  }

  new_mod <- as.character(node_df[[modules_new]])
  na_idx <- is.na(new_mod) | new_mod == ""
  if (any(na_idx)) {
    new_mod[na_idx] <- "Others"
  }

  if (is.null(levels)) {
    level_tab <- sort(table(new_mod), decreasing = TRUE)
    levels_final <- names(level_tab)
  } else {
    levels_final <- unique(as.character(levels))
    missing_levels <- setdiff(unique(new_mod), levels_final)
    levels_final <- c(levels_final, missing_levels)
  }
  levels_final <- c(setdiff(levels_final, "Others"), intersect("Others", levels_final))

  new_mod_factor <- factor(new_mod, levels = levels_final, ordered = TRUE)

  graph_obj_new <- graph_obj %>%
    tidygraph::activate(nodes) %>%
    tidygraph::mutate(
      Modularity = new_mod_factor,
      modularity2 = new_mod_factor,
      modularity3 = as.character(new_mod_factor)
    )

  if ("modularity" %in% colnames(node_df)) {
    graph_obj_new <- graph_obj_new %>%
      tidygraph::activate(nodes) %>%
      tidygraph::mutate(modularity = new_mod_factor)
  }

  if ("Degree" %in% colnames(node_df)) {
    graph_obj_new <- graph_obj_new %>%
      tidygraph::activate(nodes) %>%
      tidygraph::arrange(Modularity, dplyr::desc(Degree))
  } else {
    graph_obj_new <- graph_obj_new %>%
      tidygraph::activate(nodes) %>%
      tidygraph::arrange(Modularity)
  }

  # Mark whether the user supplied an explicit `levels` argument so that
  # downstream layout helpers can decide whether to honour the order.
  graph_obj_new <- igraph::set_graph_attr(
    graph_obj_new, ".modularity_user_ordered", user_supplied_levels
  )

  return(graph_obj_new)
}
