registry_new <- function() {
  shiny::reactiveValues(items = list(), counter = 0L, log = list())
}

registry_next_id <- function(registry) {
  counter <- shiny::isolate(registry$counter) + 1L
  registry$counter <- counter
  sprintf("obj_%04d", counter)
}

registry_summarize <- function(data, type) {
  if (is.matrix(data) || is.data.frame(data)) {
    return(list(
      rows = nrow(data),
      cols = ncol(data),
      colnames = head(colnames(data), 20),
      rownames = head(rownames(data), 20)
    ))
  }

  if (inherits(data, "igraph")) {
    return(list(
      nodes = igraph::vcount(data),
      edges = igraph::ecount(data),
      directed = igraph::is_directed(data)
    ))
  }

  list(class = class(data), type = type)
}

registry_add <- function(registry, name, type, data, source = NULL, params = list(), warnings = character()) {
  id <- registry_next_id(registry)
  item <- list(
    id = id,
    name = name,
    type = type,
    data = data,
    summary = registry_summarize(data, type),
    created_at = Sys.time(),
    source = source,
    params = params,
    warnings = warnings
  )

  items <- shiny::isolate(registry$items)
  items[[id]] <- item
  registry$items <- items
  item
}

registry_get <- function(registry, id) {
  registry$items[[id]]
}

registry_delete <- function(registry, id) {
  items <- shiny::isolate(registry$items)
  items[[id]] <- NULL
  registry$items <- items
  invisible(TRUE)
}

registry_count <- function(registry) {
  length(registry$items)
}

registry_list <- function(registry, type = NULL) {
  items <- registry$items
  if (!is.null(type)) {
    items <- Filter(function(x) identical(x$type, type), items)
  }

  if (!length(items)) {
    return(data.frame(
      id = character(),
      name = character(),
      type = character(),
      created_at = as.POSIXct(character()),
      stringsAsFactors = FALSE
    ))
  }

  do.call(rbind, lapply(items, function(x) {
    data.frame(
      id = x$id,
      name = x$name,
      type = x$type,
      created_at = x$created_at,
      stringsAsFactors = FALSE
    )
  }))
}

registry_choices <- function(registry, type = NULL) {
  listed <- registry_list(registry, type = type)
  if (!nrow(listed)) {
    return(stats::setNames(character(), character()))
  }
  stats::setNames(listed$id, paste0(listed$name, " [", listed$type, "]"))
}

registry_log_error <- function(registry, context, error) {
  entry <- list(context = context, message = conditionMessage(error), created_at = Sys.time())
  log <- shiny::isolate(registry$log)
  log[[length(log) + 1L]] <- entry
  registry$log <- log
  entry
}
