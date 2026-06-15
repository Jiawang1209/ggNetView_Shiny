parse_comparison_pairs <- function(text, available_groups) {
  if (is.null(text) || length(text) == 0L || !nzchar(trimws(text[[1]]))) {
    return(list(pairs = NULL, warnings = character()))
  }

  available_groups <- as.character(available_groups)
  lines <- unlist(strsplit(as.character(text[[1]]), "[\n;]+"))
  lines <- trimws(lines)
  lines <- lines[nzchar(lines)]

  pairs <- list()
  warnings <- character()
  seen <- character()
  for (line in lines) {
    parts <- trimws(unlist(strsplit(line, "\\s*(,|->|\\|)\\s*")))
    parts <- parts[nzchar(parts)]
    if (length(parts) != 2L) {
      warnings <- c(warnings, paste0("Skipping malformed comparison pair: ", line))
      next
    }
    if (!all(parts %in% available_groups)) {
      warnings <- c(
        warnings,
        paste0(
          "Skipping comparison pair ",
          paste(parts, collapse = ","),
          ": group not available."
        )
      )
      next
    }
    key <- paste(sort(parts), collapse = "\r")
    if (key %in% seen) {
      warnings <- c(warnings, paste0("Skipping duplicate comparison pair: ", paste(parts, collapse = ",")))
      next
    }
    seen <- c(seen, key)
    pairs[[length(pairs) + 1L]] <- parts
  }

  list(pairs = if (length(pairs)) pairs else NULL, warnings = warnings)
}

safe_multi_network_compare <- function(graphs, params = list()) {
  if (!is.list(graphs) || length(graphs) < 2L) {
    return(app_failure("Multi-network comparison requires at least two graph objects."))
  }
  if (!all(vapply(graphs, function(g) inherits(g, "igraph") || inherits(g, "tbl_graph"), logical(1)))) {
    return(app_failure("All comparison inputs must be graph objects (igraph or tbl_graph)."))
  }

  # L12 fix: coerce plain igraph objects to tbl_graph so ggNetView_multi_link
  # receives the class it requires (tbl_graph with Modularity/Degree/Strength).
  # tbl_graph objects pass through unchanged; igraph objects are enriched via
  # build_graph_from_igraph which adds the required node columns.
  coerce_fn <- tryCatch(
    utils::getFromNamespace("build_graph_from_igraph", "ggNetView"),
    error = function(e) NULL
  )
  graphs <- lapply(graphs, function(g) {
    if (inherits(g, "tbl_graph")) {
      return(g)
    }
    if (!is.null(coerce_fn)) {
      tryCatch(coerce_fn(g), error = function(e) tidygraph::as_tbl_graph(g))
    } else {
      tidygraph::as_tbl_graph(g)
    }
  })

  fn <- resolve_ggnetview_function("ggNetView_multi_link")
  if (is.null(fn)) {
    return(app_failure("Cannot find ggNetView function: ggNetView_multi_link"))
  }

  include_topology_summary <- isTRUE(params$include_topology_summary)
  topology_params <- params$topology_params
  comparison_pairs_input <- params$comparison_pairs
  params$include_topology_summary <- NULL
  params$topology_params <- NULL
  params$comparison_pairs <- NULL

  parsed_pairs <- parse_comparison_pairs(comparison_pairs_input, names(graphs))
  if (!is.null(parsed_pairs$pairs)) {
    params$comparisons_groups <- parsed_pairs$pairs
  }
  if (!is.null(comparison_pairs_input) && nzchar(trimws(as.character(comparison_pairs_input[[1]]))) && is.null(parsed_pairs$pairs)) {
    return(app_failure(paste(
      c("No valid comparison pairs were provided.", parsed_pairs$warnings),
      collapse = "\n"
    )))
  }

  defaults <- list(
    graph_obj_list = graphs,
    layout = "fr",
    layout.module = "adjacent",
    comparisons = TRUE,
    k_nn = 2
  )
  call_args <- utils::modifyList(defaults, params, keep.null = TRUE)
  call_args <- filter_function_call_args(fn, call_args)

  result <- safe_call(
    do.call(fn, call_args),
    "Failed to compare multiple networks."
  )
  if (!result$ok) {
    return(result)
  }

  value <- result$value
  plot <- if (is.list(value) && !is.null(value$p)) value$p else value
  info <- if (is.list(value)) value$info else NULL
  link_info <- if (is.list(value)) value$link_info else NULL
  link_interpretation <- interpret_multi_network_links(link_info)
  link_table <- link_interpretation$details
  link_summary <- link_interpretation$summary
  topology_table <- if (include_topology_summary) {
    summarize_multi_network_topology(graphs, params = topology_params %||% list(bootstrap = 0L))
  } else {
    data.frame()
  }

  app_success(list(
    plot = plot,
    info = info,
    link_info = link_info,
    link_table = link_table,
    link_summary = link_summary,
    link_report = link_interpretation$report,
    topology_table = topology_table,
    comparison_pairs = parsed_pairs$pairs,
    comparison_warnings = parsed_pairs$warnings,
    raw = value
  ))
}

indexed_name <- function(x, i, default) {
  names_x <- names(x)
  if (is.null(names_x) || length(names_x) < i || !nzchar(names_x[[i]])) {
    return(default)
  }
  names_x[[i]]
}

normalize_multi_network_link_table <- function(link_info) {
  if (is.null(link_info)) {
    return(data.frame())
  }
  if (is.data.frame(link_info)) {
    return(as.data.frame(link_info, check.names = FALSE))
  }
  if (is.list(link_info)) {
    flattened <- tryCatch(
      do.call(rbind, lapply(seq_along(link_info), function(i) {
        item <- link_info[[i]]
        part_name <- indexed_name(link_info, i, as.character(i))
        if (is.data.frame(item)) {
          item$comparison_part <- part_name
          return(item)
        }
        data.frame(
          comparison_part = part_name,
          value = paste(utils::capture.output(utils::str(item)), collapse = "\n"),
          stringsAsFactors = FALSE
        )
      })),
      error = function(e) NULL
    )
    if (is.data.frame(flattened)) {
      return(flattened)
    }
  }
  data.frame(value = utils::capture.output(utils::str(link_info)), stringsAsFactors = FALSE)
}

empty_multi_network_link_interpretation <- function() {
  list(
    details = data.frame(),
    summary = data.frame(),
    report = data.frame()
  )
}

report_signal_level <- function(primary_count, total_count, effect = NA_real_) {
  primary_count <- suppressWarnings(as.numeric(primary_count))
  total_count <- suppressWarnings(as.numeric(total_count))
  effect <- suppressWarnings(as.numeric(effect))
  ratio <- ifelse(is.na(total_count) | total_count <= 0, 0, primary_count / total_count)
  ifelse(
    primary_count >= 3 | ratio >= 0.5 | (!is.na(effect) & effect >= 0.7),
    "strong",
    ifelse(primary_count > 0 | (!is.na(effect) & effect >= 0.4), "moderate", "screening")
  )
}

report_text_value <- function(x, digits = 3) {
  x <- suppressWarnings(as.numeric(x))
  ifelse(is.na(x), "NA", format(round(x, digits), nsmall = digits))
}

multi_network_report_presets <- function(summary, workflow = "multi_network_compare") {
  columns <- c(
    "workflow",
    "pair",
    "group_a",
    "group_b",
    "link_level",
    "signal_scope",
    "evidence_label",
    "report_text",
    "domain_label",
    "interpretation_level",
    "narrative_text",
    "caveat_text"
  )
  if (is.null(summary) || !is.data.frame(summary) || !nrow(summary)) {
    return(stats::setNames(data.frame(matrix(ncol = length(columns), nrow = 0)), columns))
  }

  summary <- as.data.frame(summary, check.names = FALSE)
  defaults <- list(
    pair = "",
    group_a = "",
    group_b = "",
    link_level = "unknown",
    link_count = 0,
    unique_sources = 0,
    unique_targets = 0,
    mean_distance = NA_real_
  )
  for (name in names(defaults)) {
    if (!name %in% names(summary)) {
      summary[[name]] <- defaults[[name]]
    }
  }

  as_number <- function(x) suppressWarnings(as.numeric(x))
  link_count <- as_number(summary$link_count)
  unique_sources <- as_number(summary$unique_sources)
  unique_targets <- as_number(summary$unique_targets)
  mean_distance <- as_number(summary$mean_distance)

  link_level <- as.character(summary$link_level)
  signal_scope <- ifelse(
    identical(length(link_level), 0L),
    character(),
    ifelse(
      link_level == "module",
      "shared module",
      ifelse(link_level == "node", "shared node", paste("shared", link_level))
    )
  )
  evidence_label <- sprintf(
    "%s link(s); %s source(s); %s target(s); mean distance=%s",
    ifelse(is.na(link_count), 0, link_count),
    ifelse(is.na(unique_sources), 0, unique_sources),
    ifelse(is.na(unique_targets), 0, unique_targets),
    ifelse(is.na(mean_distance), "NA", format(round(mean_distance, 3), nsmall = 3))
  )
  report_text <- sprintf(
    "%s comparison contains %s %s connection(s): %s.",
    as.character(summary$pair),
    ifelse(is.na(link_count), 0, link_count),
    signal_scope,
    evidence_label
  )
  interpretation_level <- report_signal_level(link_count, link_count, mean_distance)
  domain_label <- sprintf(
    "%s %s-level shared network structure",
    as.character(summary$pair),
    link_level
  )
  narrative_text <- sprintf(
    "%s shows %s evidence for shared network structure at the %s level, with %s connection(s) across %s source(s) and %s target(s).",
    as.character(summary$pair),
    interpretation_level,
    link_level,
    ifelse(is.na(link_count), 0, link_count),
    ifelse(is.na(unique_sources), 0, unique_sources),
    ifelse(is.na(unique_targets), 0, unique_targets)
  )
  caveat_text <- sprintf(
    "Confirm %s-level shared links against sample metadata, module labels, and biological context before treating them as conserved mechanisms.",
    link_level
  )

  data.frame(
    workflow = rep(workflow, nrow(summary)),
    pair = as.character(summary$pair),
    group_a = as.character(summary$group_a),
    group_b = as.character(summary$group_b),
    link_level = link_level,
    signal_scope = signal_scope,
    evidence_label = evidence_label,
    report_text = report_text,
    domain_label = domain_label,
    interpretation_level = interpretation_level,
    narrative_text = narrative_text,
    caveat_text = caveat_text,
    stringsAsFactors = FALSE
  )
}

interpret_multi_network_links <- function(link_info) {
  details <- normalize_multi_network_link_table(link_info)
  if (!is.data.frame(details) || !nrow(details)) {
    return(empty_multi_network_link_interpretation())
  }

  details <- as.data.frame(details, check.names = FALSE)
  required <- c("group_a", "group_b", "link_level", "source", "target")
  missing <- setdiff(required, names(details))
  if (length(missing)) {
    details[missing] <- NA_character_
  }
  details$group_a <- as.character(details$group_a)
  details$group_b <- as.character(details$group_b)
  details$link_level <- as.character(details$link_level)
  details$source <- as.character(details$source)
  details$target <- as.character(details$target)
  details$pair <- paste(details$group_a, details$group_b, sep = " vs ")
  details$link_label <- paste(details$source, details$target, sep = " -> ")

  coord_cols <- c("x", "y", "xend", "yend")
  if (all(coord_cols %in% names(details))) {
    coords <- lapply(details[coord_cols], function(x) suppressWarnings(as.numeric(x)))
    details$distance <- sqrt((coords$xend - coords$x)^2 + (coords$yend - coords$y)^2)
  } else {
    details$distance <- NA_real_
  }

  split_key <- interaction(details$pair, details$group_a, details$group_b, details$link_level, drop = TRUE, lex.order = TRUE)
  summary_rows <- lapply(split(details, split_key), function(df) {
    data.frame(
      pair = df$pair[[1]],
      group_a = df$group_a[[1]],
      group_b = df$group_b[[1]],
      link_level = df$link_level[[1]],
      link_count = nrow(df),
      unique_sources = length(unique(stats::na.omit(df$source))),
      unique_targets = length(unique(stats::na.omit(df$target))),
      mean_distance = if (all(is.na(df$distance))) NA_real_ else mean(df$distance, na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  })
  summary <- do.call(rbind, summary_rows)
  rownames(summary) <- NULL
  summary <- summary[order(summary$pair, summary$link_level), , drop = FALSE]

  list(
    details = details,
    summary = summary,
    report = multi_network_report_presets(summary)
  )
}

empty_environment_link_interpretation <- function() {
  list(
    details = data.frame(),
    summary = data.frame(),
    report = data.frame()
  )
}

environment_report_presets <- function(summary, workflow = "environment_link") {
  columns <- c(
    "workflow",
    "env_block",
    "spec_block",
    "method",
    "block_pair",
    "signal_direction",
    "evidence_label",
    "report_text",
    "domain_label",
    "interpretation_level",
    "narrative_text",
    "caveat_text"
  )
  if (is.null(summary) || !is.data.frame(summary) || !nrow(summary)) {
    return(stats::setNames(data.frame(matrix(ncol = length(columns), nrow = 0)), columns))
  }

  summary <- as.data.frame(summary, check.names = FALSE)
  defaults <- list(
    env_block = "All",
    spec_block = "All",
    method = "unknown",
    link_count = 0L,
    significant_count = 0L,
    positive_count = 0L,
    negative_count = 0L,
    strongest_link = "unknown",
    strongest_correlation = NA_real_,
    strongest_pvalue = NA_real_,
    mean_abs_correlation = NA_real_
  )
  for (name in names(defaults)) {
    if (!name %in% names(summary)) {
      summary[[name]] <- defaults[[name]]
    }
  }

  as_number <- function(x) suppressWarnings(as.numeric(x))
  summary$link_count <- as_number(summary$link_count)
  summary$significant_count <- as_number(summary$significant_count)
  summary$positive_count <- as_number(summary$positive_count)
  summary$negative_count <- as_number(summary$negative_count)
  summary$strongest_correlation <- as_number(summary$strongest_correlation)
  summary$strongest_pvalue <- as_number(summary$strongest_pvalue)
  summary$mean_abs_correlation <- as_number(summary$mean_abs_correlation)

  signal_direction <- ifelse(
    summary$positive_count > 0 & summary$negative_count > 0,
    "mixed",
    ifelse(
      summary$positive_count > 0,
      "positive",
      ifelse(summary$negative_count > 0, "negative", "unknown")
    )
  )
  block_pair <- paste(summary$env_block, summary$spec_block, sep = " -> ")
  evidence_label <- sprintf(
    "%s/%s significant; mean |r|=%s",
    ifelse(is.na(summary$significant_count), 0, summary$significant_count),
    ifelse(is.na(summary$link_count), 0, summary$link_count),
    ifelse(is.na(summary$mean_abs_correlation), "NA", format(round(summary$mean_abs_correlation, 3), nsmall = 3))
  )
  report_text <- sprintf(
    "%s (%s, %s) shows a %s signal: %s; strongest link %s (r=%s, p=%s).",
    block_pair,
    summary$method,
    workflow,
    signal_direction,
    evidence_label,
    summary$strongest_link,
    ifelse(is.na(summary$strongest_correlation), "NA", format(round(summary$strongest_correlation, 3), nsmall = 3)),
    ifelse(is.na(summary$strongest_pvalue), "NA", format(signif(summary$strongest_pvalue, 3), scientific = FALSE))
  )
  interpretation_level <- report_signal_level(
    summary$significant_count,
    summary$link_count,
    summary$mean_abs_correlation
  )
  interpretation_level <- ifelse(
    summary$significant_count <= 0,
    "screening",
    interpretation_level
  )
  domain_label <- paste(summary$env_block, summary$spec_block, "association", sep = "-")
  narrative_text <- sprintf(
    "%s has %s %s evidence in the %s workflow: %s of %s tested links are statistically supported, the dominant direction is %s, and the strongest link is %s (r=%s, p=%s).",
    block_pair,
    interpretation_level,
    summary$method,
    workflow,
    ifelse(is.na(summary$significant_count), 0, summary$significant_count),
    ifelse(is.na(summary$link_count), 0, summary$link_count),
    signal_direction,
    summary$strongest_link,
    report_text_value(summary$strongest_correlation),
    ifelse(is.na(summary$strongest_pvalue), "NA", format(signif(summary$strongest_pvalue, 3), scientific = FALSE))
  )
  caveat_text <- ifelse(
    grepl("mantel", summary$method, ignore.case = TRUE),
    "Mantel-style interpretation depends on distance choices and permutation count; verify robustness with domain-specific distance metrics.",
    "Correlation-style interpretation is associative; confirm directionality with study design, covariates, and independent validation."
  )

  data.frame(
    workflow = rep(workflow, nrow(summary)),
    env_block = as.character(summary$env_block),
    spec_block = as.character(summary$spec_block),
    method = as.character(summary$method),
    block_pair = block_pair,
    signal_direction = signal_direction,
    evidence_label = evidence_label,
    report_text = report_text,
    domain_label = domain_label,
    interpretation_level = interpretation_level,
    narrative_text = narrative_text,
    caveat_text = caveat_text,
    stringsAsFactors = FALSE
  )
}

interpret_environment_links <- function(stats) {
  if (is.null(stats) || !is.data.frame(stats) || !nrow(stats)) {
    return(empty_environment_link_interpretation())
  }

  details <- as.data.frame(stats, check.names = FALSE)
  if (!"ID" %in% names(details)) {
    details$ID <- NA_character_
  }
  if (!"Type" %in% names(details)) {
    details$Type <- NA_character_
  }
  if (!"Correlation" %in% names(details)) {
    details$Correlation <- NA_real_
  }
  if (!"Pvalue" %in% names(details)) {
    details$Pvalue <- NA_real_
  }
  if (!"spec_block" %in% names(details)) {
    details$spec_block <- "All"
  }
  if (!"env_block" %in% names(details)) {
    details$env_block <- "All"
  }
  if (!"method" %in% names(details)) {
    details$method <- "unknown"
  }

  details$ID <- as.character(details$ID)
  details$Type <- as.character(details$Type)
  details$spec_block <- as.character(details$spec_block)
  details$env_block <- as.character(details$env_block)
  details$method <- as.character(details$method)
  details$Correlation <- suppressWarnings(as.numeric(details$Correlation))
  details$Pvalue <- suppressWarnings(as.numeric(details$Pvalue))
  details$abs_correlation <- abs(details$Correlation)
  details$direction <- ifelse(
    is.na(details$Correlation),
    "unknown",
    ifelse(details$Correlation > 0, "positive", ifelse(details$Correlation < 0, "negative", "zero"))
  )
  details$significant <- if ("p_signif" %in% names(details)) {
    nzchar(as.character(details$p_signif)) | (!is.na(details$Pvalue) & details$Pvalue < 0.05)
  } else {
    !is.na(details$Pvalue) & details$Pvalue < 0.05
  }
  details$link_label <- paste(details$ID, details$Type, sep = " ~ ")

  split_key <- interaction(details$env_block, details$spec_block, details$method, drop = TRUE, lex.order = TRUE)
  summary_rows <- lapply(split(details, split_key), function(df) {
    strongest_index <- if (all(is.na(df$abs_correlation))) {
      1L
    } else {
      which.max(replace(df$abs_correlation, is.na(df$abs_correlation), -Inf))
    }
    data.frame(
      env_block = df$env_block[[1]],
      spec_block = df$spec_block[[1]],
      method = df$method[[1]],
      link_count = nrow(df),
      significant_count = sum(isTRUE(df$significant) | df$significant, na.rm = TRUE),
      positive_count = sum(df$direction == "positive", na.rm = TRUE),
      negative_count = sum(df$direction == "negative", na.rm = TRUE),
      strongest_link = df$link_label[[strongest_index]],
      strongest_correlation = df$Correlation[[strongest_index]],
      strongest_pvalue = df$Pvalue[[strongest_index]],
      mean_abs_correlation = if (all(is.na(df$abs_correlation))) NA_real_ else mean(df$abs_correlation, na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  })
  summary <- do.call(rbind, summary_rows)
  rownames(summary) <- NULL
  summary <- summary[order(summary$env_block, summary$spec_block, summary$method), , drop = FALSE]

  list(details = details, summary = summary, report = environment_report_presets(summary))
}

summarize_multi_network_topology <- function(graphs, params = list()) {
  if (!length(graphs)) {
    return(data.frame())
  }

  rows <- lapply(seq_along(graphs), function(i) {
    graph_name <- indexed_name(graphs, i, paste0("Graph_", i))

    result <- safe_topology(graphs[[i]], params = params)
    if (!isTRUE(result$ok)) {
      return(data.frame(
        graph = graph_name,
        Topology = "error",
        Value = NA_real_,
        Detail = result$message,
        stringsAsFactors = FALSE
      ))
    }

    topology <- result$value$topology
    if (is.null(topology) || !is.data.frame(topology) || !nrow(topology)) {
      return(data.frame(
        graph = graph_name,
        Topology = "empty",
        Value = NA_real_,
        Detail = "No topology rows returned.",
        stringsAsFactors = FALSE
      ))
    }

    topology <- as.data.frame(topology, check.names = FALSE)
    if (!"Topology" %in% names(topology)) {
      topology$Topology <- rownames(topology) %||% seq_len(nrow(topology))
    }
    value_col <- intersect(c("Value", "value", "Real", "real", "Network", "network"), names(topology))
    if (!length(value_col)) {
      numeric_cols <- names(topology)[vapply(topology, is.numeric, logical(1))]
      value_col <- if (length(numeric_cols)) numeric_cols[[1]] else NA_character_
    }
    value <- if (!is.na(value_col)) topology[[value_col]] else NA_real_
    detail_cols <- setdiff(names(topology), c("Topology", value_col))
    detail <- if (length(detail_cols)) {
      apply(topology[, detail_cols, drop = FALSE], 1, function(x) {
        paste(names(x), x, sep = "=", collapse = "; ")
      })
    } else {
      rep("", nrow(topology))
    }

    data.frame(
      graph = graph_name,
      Topology = as.character(topology$Topology),
      Value = suppressWarnings(as.numeric(value)),
      Detail = detail,
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, rows)
}

default_group_info_for_matrix <- function(mat, split = c("halves", "alternating")) {
  split <- match.arg(split)
  mat <- as.data.frame(mat, check.names = FALSE)
  samples <- colnames(mat)
  if (is.null(samples) || length(samples) < 2L) {
    stop("Grouped network workflow requires a matrix with at least two sample columns.", call. = FALSE)
  }

  if (split == "alternating") {
    groups <- rep(c("Group_A", "Group_B"), length.out = length(samples))
  } else {
    midpoint <- ceiling(length(samples) / 2)
    groups <- ifelse(seq_along(samples) <= midpoint, "Group_A", "Group_B")
  }

  data.frame(Sample = samples, Group = groups, stringsAsFactors = FALSE)
}

align_group_info_for_matrix <- function(mat, group_info) {
  mat <- as.data.frame(mat, check.names = FALSE)
  samples <- colnames(mat)
  if (is.null(samples) || length(samples) < 2L) {
    stop("Grouped network workflow requires a matrix with at least two sample columns.", call. = FALSE)
  }

  group_info <- as.data.frame(group_info, check.names = FALSE)
  if (!all(c("Sample", "Group") %in% names(group_info))) {
    stop("Group metadata must contain Sample and Group columns.", call. = FALSE)
  }
  group_info$Sample <- as.character(group_info$Sample)
  group_info$Group <- as.character(group_info$Group)

  matching <- group_info[group_info$Sample %in% samples, , drop = FALSE]
  if (anyDuplicated(matching$Sample)) {
    stop("Group metadata contains duplicate Sample values for selected matrix columns.", call. = FALSE)
  }

  missing_samples <- setdiff(samples, matching$Sample)
  if (length(missing_samples)) {
    stop(
      "Group metadata is missing samples: ",
      paste(missing_samples, collapse = ", "),
      call. = FALSE
    )
  }

  aligned <- matching[match(samples, matching$Sample), , drop = FALSE]
  aligned[, c("Sample", "Group"), drop = FALSE]
}

safe_multi_group_network <- function(mat, group_info = NULL, params = list()) {
  fn <- resolve_ggnetview_function("ggNetView_multi")
  if (is.null(fn)) {
    return(app_failure("Cannot find ggNetView function: ggNetView_multi"))
  }

  mat <- as.data.frame(mat, check.names = FALSE)
  if (is.null(group_info)) {
    group_info <- default_group_info_for_matrix(mat)
  } else {
    group_info <- tryCatch(align_group_info_for_matrix(mat, group_info), error = function(e) e)
    if (inherits(group_info, "error")) {
      return(app_failure(conditionMessage(group_info)))
    }
  }

  defaults <- list(
    mat = mat,
    group_info = group_info,
    method = "cor",
    cor.method = "pearson",
    r.threshold = 0.2,
    p.threshold = 1,
    layout = "circle",
    layout.module = "adjacent"
  )
  call_args <- utils::modifyList(defaults, params, keep.null = TRUE)
  call_args <- filter_function_call_args(fn, call_args)

  result <- safe_call(
    do.call(fn, call_args),
    "Failed to build grouped multi-network plot."
  )
  if (!result$ok) {
    return(result)
  }

  app_success(list(
    plot = result$value,
    group_info = group_info,
    raw = result$value
  ))
}

parse_table_blocks <- function(text, available_columns, default_name = "Block") {
  available_columns <- as.character(available_columns)
  if (is.null(text) || length(text) == 0L || !nzchar(trimws(text[[1]]))) {
    return(list(
      blocks = stats::setNames(list(available_columns), default_name),
      warnings = character()
    ))
  }

  lines <- unlist(strsplit(as.character(text[[1]]), "\n+"))
  lines <- trimws(lines)
  lines <- lines[nzchar(lines)]

  blocks <- list()
  warnings <- character()
  for (line in lines) {
    pieces <- strsplit(line, ":", fixed = TRUE)[[1]]
    if (length(pieces) < 2L) {
      warnings <- c(warnings, paste0("Skipping malformed block definition: ", line))
      next
    }
    block_name <- trimws(pieces[[1]])
    raw_cols <- paste(pieces[-1], collapse = ":")
    if (!nzchar(block_name)) {
      block_name <- paste0(default_name, "_", length(blocks) + 1L)
    }
    cols <- trimws(unlist(strsplit(raw_cols, "\\s*,\\s*")))
    cols <- cols[nzchar(cols)]
    if (!length(cols)) {
      warnings <- c(warnings, paste0("Skipping empty block: ", block_name))
      next
    }

    resolved <- unlist(lapply(cols, function(col) {
      if (col %in% available_columns) {
        return(col)
      }
      numeric_index <- suppressWarnings(as.integer(col))
      if (!is.na(numeric_index) && numeric_index >= 1L && numeric_index <= length(available_columns)) {
        return(available_columns[[numeric_index]])
      }
      warnings <<- c(warnings, paste0("Skipping unavailable column '", col, "' in block ", block_name, "."))
      character()
    }))
    resolved <- unique(resolved)
    if (!length(resolved)) {
      warnings <- c(warnings, paste0("Skipping block with no valid columns: ", block_name))
      next
    }

    name <- make.unique(c(names(blocks), block_name), sep = "_")
    blocks[[name[[length(name)]]]] <- resolved
  }

  if (!length(blocks)) {
    return(list(blocks = NULL, warnings = warnings))
  }
  list(blocks = blocks, warnings = warnings)
}

environment_block_selectors <- function(env, spec, env_blocks = NULL, spec_blocks = NULL) {
  env_parsed <- parse_table_blocks(env_blocks, colnames(env), "Environment")
  spec_parsed <- parse_table_blocks(spec_blocks, colnames(spec), "Species")
  list(
    env_select = env_parsed$blocks,
    spec_select = spec_parsed$blocks,
    warnings = c(env_parsed$warnings, spec_parsed$warnings)
  )
}

parse_environment_block_pairs <- function(text, env_names, spec_names) {
  if (is.null(text) || length(text) == 0L || !nzchar(trimws(text[[1]]))) {
    return(list(pairs = NULL, warnings = character()))
  }

  env_names <- as.character(env_names)
  spec_names <- as.character(spec_names)
  lines <- unlist(strsplit(as.character(text[[1]]), "[\n;]+"))
  lines <- trimws(lines)
  lines <- lines[nzchar(lines)]

  pairs <- list()
  warnings <- character()
  seen <- character()
  for (line in lines) {
    parts <- trimws(unlist(strsplit(line, "\\s*(,|->|\\|)\\s*")))
    parts <- parts[nzchar(parts)]
    if (length(parts) != 2L) {
      warnings <- c(warnings, paste0("Skipping malformed environment block pair: ", line))
      next
    }
    if (!parts[[1]] %in% env_names) {
      warnings <- c(warnings, paste0("Skipping environment block pair ", paste(parts, collapse = ","), ": env block not available."))
      next
    }
    if (!parts[[2]] %in% spec_names) {
      warnings <- c(warnings, paste0("Skipping environment block pair ", paste(parts, collapse = ","), ": spec block not available."))
      next
    }
    key <- paste(parts, collapse = "\r")
    if (key %in% seen) {
      warnings <- c(warnings, paste0("Skipping duplicate environment block pair: ", paste(parts, collapse = ",")))
      next
    }
    seen <- c(seen, key)
    pairs[[length(pairs) + 1L]] <- parts
  }

  list(pairs = if (length(pairs)) pairs else NULL, warnings = warnings)
}

parse_environment_option_list <- function(text, allowed = NULL) {
  if (is.null(text) || length(text) == 0L || !nzchar(trimws(as.character(text[[1]])))) {
    return(character())
  }
  values <- trimws(unlist(strsplit(as.character(text[[1]]), "\\s*(?:,|;|\\||\\r?\\n)+\\s*", perl = TRUE)))
  values <- values[nzchar(values)]
  if (!is.null(allowed)) {
    values <- values[values %in% allowed]
  }
  unique(values)
}

optional_positive_numeric <- function(x) {
  if (is.null(x) || length(x) == 0L || is.na(x[[1]])) {
    return(NULL)
  }
  value <- suppressWarnings(as.numeric(x[[1]]))
  if (!is.finite(value) || value <= 0) {
    return(NULL)
  }
  value
}

optional_numeric <- function(x) {
  if (is.null(x) || length(x) == 0L || is.na(x[[1]])) {
    return(NULL)
  }
  value <- suppressWarnings(as.numeric(x[[1]]))
  if (!is.finite(value)) {
    return(NULL)
  }
  value
}

optional_nonnegative_numeric <- function(x) {
  if (is.null(x) || length(x) == 0L || is.na(x[[1]])) {
    return(NULL)
  }
  value <- suppressWarnings(as.numeric(x[[1]]))
  if (!is.finite(value) || value < 0) {
    return(NULL)
  }
  value
}

optional_positive_integer <- function(x) {
  value <- optional_positive_numeric(x)
  if (is.null(value)) {
    return(NULL)
  }
  as.integer(round(value))
}

optional_color <- function(x) {
  if (is.null(x) || length(x) == 0L || is.na(x[[1]])) {
    return(NULL)
  }
  value <- trimws(as.character(x[[1]]))
  if (!nzchar(value)) {
    return(NULL)
  }
  value
}

environment_geometry_params <- function(
  orientation_text = NULL,
  spec_layout_text = NULL,
  group_layout = NULL,
  group_angle = NULL,
  group_arc_angle = NULL,
  anchor_dist = NULL,
  distance = NULL,
  nrow = NULL,
  ncol = NULL,
  scale_networks = NULL,
  core_point_size = NULL,
  heatmap_label_size = NULL,
  heatmap_sig_size = NULL,
  heatmap_point_size = NULL,
  sig_line_width_min = NULL,
  sig_line_width_max = NULL,
  sig_line_color_low = NULL,
  sig_line_color_high = NULL,
  sig_line_alpha = NULL
) {
  allowed_orientations <- c("top_right", "bottom_right", "top_left", "bottom_left")
  allowed_spec_layouts <- c("circle_outline", "diamond_outline", "rectangle_outline", "square_outline")
  allowed_group_layouts <- c("circle", "row", "column", "square", "diamond", "triangle", "triangle_down", "snake", "arc")

  params <- list()
  orientation <- parse_environment_option_list(orientation_text, allowed_orientations)
  if (length(orientation)) {
    params$orientation <- orientation
  }
  spec_layout <- parse_environment_option_list(spec_layout_text, allowed_spec_layouts)
  if (length(spec_layout)) {
    params$spec_layout <- spec_layout
  }
  if (!is.null(group_layout) && length(group_layout) && group_layout[[1]] %in% allowed_group_layouts) {
    params$group_layout <- group_layout[[1]]
  }
  group_angle <- optional_numeric(group_angle)
  if (!is.null(group_angle)) {
    params$group_angle <- group_angle
  }
  group_arc_angle <- optional_numeric(group_arc_angle)
  if (!is.null(group_arc_angle)) {
    params$group_arc_angle <- group_arc_angle
  }
  anchor_dist <- optional_positive_numeric(anchor_dist)
  if (!is.null(anchor_dist)) {
    params$anchor_dist <- anchor_dist
  }
  distance <- optional_numeric(distance)
  if (!is.null(distance)) {
    params$distance <- distance
  }
  nrow <- optional_positive_integer(nrow)
  if (!is.null(nrow)) {
    params$nrow <- nrow
  }
  ncol <- optional_positive_integer(ncol)
  if (!is.null(ncol)) {
    params$ncol <- ncol
  }
  if (!is.null(scale_networks) && length(scale_networks) && !is.na(scale_networks[[1]])) {
    params$scale_networks <- isTRUE(scale_networks[[1]])
  }
  core_point_size <- optional_positive_numeric(core_point_size)
  if (!is.null(core_point_size)) {
    params$CorePointSize <- core_point_size
  }
  heatmap_label_size <- optional_positive_numeric(heatmap_label_size)
  if (!is.null(heatmap_label_size)) {
    params$HeatmapLabelSize <- heatmap_label_size
  }
  heatmap_sig_size <- optional_positive_numeric(heatmap_sig_size)
  if (!is.null(heatmap_sig_size)) {
    params$HeatmapSigSize <- heatmap_sig_size
  }
  heatmap_point_size <- optional_positive_numeric(heatmap_point_size)
  if (!is.null(heatmap_point_size)) {
    params$HeatmapPointSize <- heatmap_point_size
  }
  sig_line_width_min <- optional_positive_numeric(sig_line_width_min)
  sig_line_width_max <- optional_positive_numeric(sig_line_width_max)
  if (!is.null(sig_line_width_min) && !is.null(sig_line_width_max)) {
    params$SigLineWidth <- sort(c(sig_line_width_min, sig_line_width_max))
  }
  sig_line_color_low <- optional_color(sig_line_color_low)
  sig_line_color_high <- optional_color(sig_line_color_high)
  if (!is.null(sig_line_color_low) && !is.null(sig_line_color_high)) {
    params$SigLineColor <- c(sig_line_color_low, sig_line_color_high)
  }
  sig_line_alpha <- optional_nonnegative_numeric(sig_line_alpha)
  if (!is.null(sig_line_alpha)) {
    params$SigLineAlpha <- min(sig_line_alpha, 1)
  }

  params
}

environment_mantel_params <- function(
  method = "pearson",
  alternative = "two.sided",
  spec_dist_method = "bray",
  env_dist_method = "euclidean",
  permutations = 99L
) {
  method_choices <- c("pearson", "spearman", "kendall")
  alternative_choices <- c("two.sided", "less", "greater")
  method <- as.character(method %||% "pearson")[[1]]
  alternative <- as.character(alternative %||% "two.sided")[[1]]
  if (!method %in% method_choices) {
    method <- "pearson"
  }
  if (!alternative %in% alternative_choices) {
    alternative <- "two.sided"
  }

  spec_dist_method <- trimws(as.character(spec_dist_method %||% "bray")[[1]])
  env_dist_method <- trimws(as.character(env_dist_method %||% "euclidean")[[1]])
  if (!nzchar(spec_dist_method)) {
    spec_dist_method <- "bray"
  }
  if (!nzchar(env_dist_method)) {
    env_dist_method <- "euclidean"
  }

  permutations <- optional_positive_integer(permutations)
  if (is.null(permutations)) {
    permutations <- 99L
  }

  list(
    method = method,
    mantel.method2 = method,
    mantel.alternative = alternative,
    spec_dist_method = spec_dist_method,
    env_dist_method = env_dist_method,
    permutations = permutations
  )
}

apply_environment_pair_params <- function(params, env_select, spec_select) {
  env_spec_pairs <- params$env_spec_pairs
  params$env_spec_pairs <- NULL
  parsed_pairs <- parse_environment_block_pairs(
    env_spec_pairs,
    env_names = names(env_select),
    spec_names = names(spec_select)
  )
  if (!is.null(parsed_pairs$pairs)) {
    params$comparisons <- TRUE
    params$comparisons_groups <- parsed_pairs$pairs
  }
  if (!is.null(env_spec_pairs) && nzchar(trimws(as.character(env_spec_pairs[[1]]))) && is.null(parsed_pairs$pairs)) {
    return(list(
      ok = FALSE,
      message = paste(c("No valid environment block pairs were provided.", parsed_pairs$warnings), collapse = "\n")
    ))
  }
  list(ok = TRUE, params = params, pairs = parsed_pairs$pairs, warnings = parsed_pairs$warnings)
}

filter_function_call_args <- function(fn, call_args) {
  fn_formals <- formals(fn)
  allowed_names <- names(fn_formals)
  call_args <- call_args[names(call_args) %in% allowed_names]

  if ("group_layout" %in% names(call_args) && "group_layout" %in% allowed_names) {
    group_choices <- tryCatch(
      eval(fn_formals$group_layout, envir = baseenv()),
      error = function(e) NULL
    )
    if (is.character(group_choices) && length(group_choices) && !call_args$group_layout %in% group_choices) {
      call_args$group_layout <- group_choices[[1]]
    }
  }

  call_args
}

safe_environment_link <- function(env, spec, env_select = NULL, spec_select = NULL, env_blocks = NULL, spec_blocks = NULL, env_spec_pairs = NULL, params = list()) {
  fn <- resolve_ggnetview_function("gglink_heatmaps")
  if (is.null(fn)) {
    return(app_failure("Cannot find ggNetView function: gglink_heatmaps"))
  }

  env <- as.data.frame(env, check.names = FALSE)
  spec <- as.data.frame(spec, check.names = FALSE)
  env_blocks <- env_blocks %||% params$env_blocks
  spec_blocks <- spec_blocks %||% params$spec_blocks
  block_selectors <- environment_block_selectors(
    env,
    spec,
    env_blocks = env_blocks,
    spec_blocks = spec_blocks
  )
  params$env_blocks <- NULL
  params$spec_blocks <- NULL
  env_spec_pairs <- env_spec_pairs %||% params$env_spec_pairs
  params$env_spec_pairs <- env_spec_pairs
  if (is.null(env_select)) {
    env_select <- block_selectors$env_select
  }
  if (is.null(spec_select)) {
    spec_select <- block_selectors$spec_select
  }
  if (is.null(env_select) || is.null(spec_select)) {
    return(app_failure(paste(
      c("No valid environment/species blocks were provided.", block_selectors$warnings),
      collapse = "\n"
    )))
  }
  pair_params <- apply_environment_pair_params(params, env_select, spec_select)
  if (!isTRUE(pair_params$ok)) {
    return(app_failure(pair_params$message))
  }
  params <- pair_params$params

  defaults <- list(
    env = env,
    spec = spec,
    env_select = env_select,
    spec_select = spec_select,
    relation_method = "correlation",
    cor.method = "pearson",
    orientation = "top_right",
    group_layout = "circle",
    seed = 1115L
  )
  call_args <- utils::modifyList(defaults, params, keep.null = TRUE)
  call_args <- filter_function_call_args(fn, call_args)

  result <- safe_call(
    do.call(fn, call_args),
    "Failed to calculate environment links."
  )
  if (!result$ok) {
    return(result)
  }

  value <- result$value
  app_success(list(
    plot = value[[1]],
    curved_plot = value[[2]],
    stats = value[[3]],
    env_select = env_select,
    spec_select = spec_select,
    comparison_pairs = pair_params$pairs,
    comparison_warnings = pair_params$warnings,
    block_warnings = c(block_selectors$warnings, pair_params$warnings),
    call_params = call_args,
    raw = value
  ))
}

safe_environment_heatmap <- function(env, spec, env_select = NULL, spec_select = NULL, env_blocks = NULL, spec_blocks = NULL, env_spec_pairs = NULL, params = list()) {
  fn <- resolve_ggnetview_function("gglink_heatmaps")
  if (is.null(fn)) {
    return(app_failure("Cannot find ggNetView function: gglink_heatmaps"))
  }

  env <- as.data.frame(env, check.names = FALSE)
  spec <- as.data.frame(spec, check.names = FALSE)
  env_blocks <- env_blocks %||% params$env_blocks
  spec_blocks <- spec_blocks %||% params$spec_blocks
  block_selectors <- environment_block_selectors(
    env,
    spec,
    env_blocks = env_blocks,
    spec_blocks = spec_blocks
  )
  params$env_blocks <- NULL
  params$spec_blocks <- NULL
  env_spec_pairs <- env_spec_pairs %||% params$env_spec_pairs
  params$env_spec_pairs <- env_spec_pairs
  if (is.null(env_select)) {
    env_select <- block_selectors$env_select
  }
  if (is.null(spec_select)) {
    spec_select <- block_selectors$spec_select
  }
  if (is.null(env_select) || is.null(spec_select)) {
    return(app_failure(paste(
      c("No valid environment/species blocks were provided.", block_selectors$warnings),
      collapse = "\n"
    )))
  }
  pair_params <- apply_environment_pair_params(params, env_select, spec_select)
  if (!isTRUE(pair_params$ok)) {
    return(app_failure(pair_params$message))
  }
  params <- pair_params$params

  defaults <- list(
    env = env,
    spec = spec,
    env_select = env_select,
    spec_select = spec_select,
    relation_method = "correlation",
    cor.method = "pearson",
    cor.use = "pairwise",
    mantel_kind = "block_vs_col",
    permutations = 99L,
    seed = 1115L,
    spec_collapse = FALSE,
    drop_nonsig = FALSE,
    orientation = "top_right",
    group_layout = "circle",
    spec_layout = "circle_outline"
  )
  call_args <- utils::modifyList(defaults, params, keep.null = TRUE)
  call_args <- filter_function_call_args(fn, call_args)

  result <- safe_call(
    do.call(fn, call_args),
    "Failed to calculate manual environment heatmap."
  )
  if (!result$ok) {
    return(result)
  }

  value <- result$value
  plot <- if (is.list(value) && length(value) >= 1L) value[[1]] else value
  curved_plot <- if (is.list(value) && length(value) >= 2L) value[[2]] else NULL
  stats <- if (is.list(value) && length(value) >= 3L) value[[3]] else data.frame()

  app_success(list(
    plot = plot,
    curved_plot = curved_plot,
    stats = stats,
    env_select = env_select,
    spec_select = spec_select,
    comparison_pairs = pair_params$pairs,
    comparison_warnings = pair_params$warnings,
    block_warnings = c(block_selectors$warnings, pair_params$warnings),
    call_params = call_args,
    raw = value
  ))
}

safe_module_environment_heatmap <- function(graph, env, otu_mat, env_blocks = NULL, params = list()) {
  fn <- resolve_ggnetview_function("ggnetview_modularity_heatmaps")
  if (is.null(fn)) {
    return(app_failure("Cannot find ggNetView function: ggnetview_modularity_heatmaps"))
  }
  if (!inherits(graph, "igraph")) {
    return(app_failure("Module environment heatmap requires a graph object."))
  }

  env <- as.data.frame(env, check.names = FALSE)
  otu_mat <- as.matrix(otu_mat)
  if (is.null(rownames(otu_mat)) || is.null(colnames(otu_mat))) {
    return(app_failure("Module environment heatmap requires an OTU matrix with row and column names."))
  }
  common_samples <- if (!is.null(rownames(env))) {
    intersect(rownames(env), colnames(otu_mat))
  } else {
    character()
  }
  if (is.null(rownames(env)) || length(common_samples) < 3L) {
    if (ncol(env) == length(colnames(otu_mat))) {
      env <- as.data.frame(t(as.matrix(env)), check.names = FALSE)
    }
  }

  orientation <- params$orientation
  if (is.null(orientation) || !length(orientation)) {
    orientation <- "top_right"
  }
  orientation <- as.character(orientation)
  params$orientation <- NULL

  env_parsed <- parse_table_blocks(env_blocks %||% params$env_blocks, colnames(env), "Environment")
  params$env_blocks <- NULL
  env_select <- env_parsed$blocks
  if (is.null(env_select)) {
    return(app_failure(paste(
      c("No valid environment blocks were provided.", env_parsed$warnings),
      collapse = "\n"
    )))
  }
  if (length(env_select) != length(orientation)) {
    return(app_failure("Module environment heatmap requires the same number of environment blocks and orientations."))
  }

  if (!is.null(params$distance) && is.finite(params$distance) && params$distance < 0) {
    return(app_failure("Module environment heatmap requires a non-negative heatmap distance."))
  }

  defaults <- list(
    graph_obj = graph,
    env = env,
    otu_mat = otu_mat,
    env_select = env_select,
    module_index = "eigengene",
    abundance_type = "sum",
    relation_method = "correlation",
    cor.method = "pearson",
    cor.use = "pairwise",
    mantel_kind = "block_vs_col",
    permutations = 99L,
    seed = 1115L,
    drop_nonsig = FALSE,
    layout = "circle",
    layout.module = "adjacent",
    orientation = orientation,
    distance = 3
  )
  call_args <- utils::modifyList(defaults, params, keep.null = TRUE)
  call_args <- filter_function_call_args(fn, call_args)

  result <- safe_call(
    do.call(fn, call_args),
    "Failed to calculate module environment heatmap."
  )
  if (!result$ok) {
    return(result)
  }

  value <- result$value
  plot <- if (is.list(value) && length(value) >= 1L) value[[1]] else value
  curved_plot <- if (is.list(value) && length(value) >= 2L) value[[2]] else NULL
  stats <- if (is.list(value) && length(value) >= 3L) value[[3]] else data.frame()

  app_success(list(
    plot = plot,
    curved_plot = curved_plot,
    stats = stats,
    env_select = env_select,
    block_warnings = env_parsed$warnings,
    call_params = call_args,
    raw = value
  ))
}

graph_to_triple_tables <- function(graph) {
  if (!inherits(graph, "igraph")) {
    stop("Triple heatmap requires a graph object for edge/node tables.", call. = FALSE)
  }

  edges <- igraph::as_data_frame(graph, what = "edges")
  if (!all(c("from", "to") %in% names(edges))) {
    stop("Graph edge table must contain from/to columns.", call. = FALSE)
  }
  if (!"weight" %in% names(edges)) {
    edges$weight <- 1
  }
  edges <- edges[, c("from", "to", "weight"), drop = FALSE]

  nodes <- igraph::as_data_frame(graph, what = "vertices")
  if (!"name" %in% names(nodes)) {
    vertex_names <- igraph::V(graph)$name
    if (is.null(vertex_names)) {
      vertex_names <- as.character(seq_len(igraph::vcount(graph)))
    }
    nodes$name <- vertex_names
  }
  annotation_col <- intersect(c("Modularity", "modularity2", "modularity", "module", "annotation"), names(nodes))
  annotation <- if (length(annotation_col)) {
    as.character(nodes[[annotation_col[[1]]]])
  } else {
    rep("Feature", nrow(nodes))
  }
  node_table <- data.frame(
    node = as.character(nodes$name),
    annotation = annotation,
    stringsAsFactors = FALSE
  )

  list(edges = edges, nodes = node_table)
}

sample_table_for_triple <- function(x, sample_col = "Sample") {
  x <- as.data.frame(x, check.names = FALSE)
  if (sample_col %in% names(x)) {
    return(x)
  }
  samples <- rownames(x)
  if (is.null(samples) || any(!nzchar(samples))) {
    samples <- paste0("S", seq_len(nrow(x)))
  }
  data.frame(Sample = samples, x, check.names = FALSE)
}

safe_environment_triple_heatmap <- function(env, experiment, graph, params = list()) {
  fn <- resolve_ggnetview_function("gglink_heatmap_triple")
  if (is.null(fn)) {
    return(app_failure("Cannot find ggNetView function: gglink_heatmap_triple"))
  }

  tables <- tryCatch(graph_to_triple_tables(graph), error = function(e) e)
  if (inherits(tables, "error")) {
    return(app_failure(conditionMessage(tables)))
  }

  env <- as.data.frame(env, check.names = FALSE)
  experiment <- as.data.frame(experiment, check.names = FALSE)
  common_samples <- intersect(rownames(env), rownames(experiment))
  if (length(common_samples) >= 3L) {
    env <- env[common_samples, , drop = FALSE]
    experiment <- experiment[common_samples, , drop = FALSE]
  }

  graph_nodes <- tables$nodes$node
  feature_count_param <- params$feature_count
  if (is.null(feature_count_param)) {
    feature_count_param <- min(3L, ncol(experiment))
  }
  feature_count <- as.integer(feature_count_param)
  feature_count <- max(1L, min(feature_count, ncol(experiment), max(1L, length(graph_nodes) - 1L)))
  preferred_features <- intersect(graph_nodes, colnames(experiment))
  if (length(preferred_features) >= feature_count) {
    feature_names <- preferred_features[seq_len(feature_count)]
  } else {
    feature_names <- unique(c(preferred_features, colnames(experiment)))[seq_len(feature_count)]
  }
  experiment <- experiment[, feature_names, drop = FALSE]

  defaults <- list(
    Environment = sample_table_for_triple(env),
    Experiment = sample_table_for_triple(experiment),
    edge = tables$edges,
    node = tables$nodes,
    sample_col = "Sample",
    hub_n = ncol(experiment),
    r = 6
  )
  call_args <- utils::modifyList(defaults, params[names(params) != "feature_count"], keep.null = TRUE)

  result <- safe_call(
    do.call(fn, call_args),
    "Failed to calculate triple environment heatmap."
  )
  if (!result$ok) {
    return(result)
  }

  app_success(list(
    plot = result$value,
    nodes = tables$nodes,
    edges = tables$edges,
    experiment = experiment,
    raw = result$value
  ))
}

safe_mantel_pairwise <- function(spec, env, params = list()) {
  fn <- resolve_ggnetview_function("mantel_pairwise")
  if (is.null(fn)) {
    return(app_failure("Cannot find ggNetView function: mantel_pairwise"))
  }

  defaults <- list(
    spec_df = as.data.frame(spec, check.names = FALSE),
    env_df = as.data.frame(env, check.names = FALSE),
    method = "pearson",
    permutations = 99L,
    seed = 1115L
  )
  call_args <- utils::modifyList(defaults, params, keep.null = TRUE)
  call_args <- filter_function_call_args(fn, call_args)

  safe_call(
    do.call(fn, call_args),
    "Failed to run Mantel pairwise test."
  )
}

safe_mantel_table <- function(spec, env, params = list()) {
  kind <- as.character(params$mantel_kind %||% "col_vs_col")[[1]]
  if (!kind %in% c("block_vs_col", "col_vs_col")) {
    kind <- "col_vs_col"
  }
  params$mantel_kind <- NULL

  method <- params$method %||% params$mantel.method2 %||% "pearson"
  params$method <- method
  params$mantel.method2 <- NULL

  if (identical(kind, "col_vs_col")) {
    return(safe_mantel_pairwise(spec, env, params = params))
  }

  fn <- resolve_ggnetview_function("mantel_block_vs_col")
  if (is.null(fn)) {
    return(app_failure("Cannot find ggNetView function: mantel_block_vs_col"))
  }

  params$mantel.alternative <- NULL
  defaults <- list(
    spec_df = as.data.frame(spec, check.names = FALSE),
    env_df = as.data.frame(env, check.names = FALSE),
    block_name = "Species",
    method = "pearson",
    spec_dist_method = "bray",
    env_dist_method = "euclidean",
    permutations = 99L,
    seed = 1115L
  )
  call_args <- utils::modifyList(defaults, params, keep.null = TRUE)
  call_args <- filter_function_call_args(fn, call_args)

  safe_call(
    do.call(fn, call_args),
    "Failed to run block-vs-column Mantel test."
  )
}
