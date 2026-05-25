library(shiny)
library(bslib)
library(DT)
library(ggplot2)
library(igraph)

app_root <- normalizePath(file.path("..", ".."), mustWork = FALSE)

app_helper_env <- new.env(parent = .GlobalEnv)
app_helper_files <- file.path(app_root, "R", c("app_validation.R", "app_registry.R", "app_adapters.R"))

load_app_helper <- function(name) {
  if (exists(name, envir = .GlobalEnv, inherits = FALSE)) {
    return(invisible(TRUE))
  }

  ns <- tryCatch(asNamespace("ggNetView"), error = function(e) NULL)
  if (!is.null(ns) && exists(name, envir = ns, inherits = FALSE)) {
    assign(name, get(name, envir = ns), envir = .GlobalEnv)
    return(invisible(TRUE))
  }

  for (helper_file in app_helper_files) {
    if (file.exists(helper_file)) {
      sys.source(helper_file, envir = app_helper_env)
    }
  }

  if (exists(name, envir = app_helper_env, inherits = FALSE)) {
    assign(name, get(name, envir = app_helper_env), envir = .GlobalEnv)
    return(invisible(TRUE))
  }

  invisible(FALSE)
}

invisible(lapply(c(
  "app_result", "app_success", "app_failure",
  "read_user_table", "detect_upload_type", "validate_matrix_like",
  "safe_call", "safe_build_graph", "safe_plot_ggnetview", "safe_topology",
  "registry_new", "registry_next_id", "registry_summarize", "registry_add",
  "registry_get", "registry_delete", "registry_count", "registry_list",
  "registry_log_error"
), load_app_helper))
