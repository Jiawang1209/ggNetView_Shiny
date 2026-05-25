# ggNetView Shiny Redesign Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Rebuild this repository into the primary ggNetView Shiny application, using `package/ggNetView/R/` as the authoritative API and delivering the first core workflow from upload to network, plot, topology, and export.

**Architecture:** Keep the root project as an R package-style Shiny app. Copy the new ggNetView API into root `R/`, add a small app service layer for registry, validation, adapters, and exports, then rebuild `inst/app/` as focused Shiny modules that exchange data only through the registry.

**Tech Stack:** R, Shiny, bslib, DT, ggplot2, igraph, ggNetView core functions, testthat.

---

## Scope

This plan implements the first milestone only:

```text
Upload data
  -> validate and register object
  -> build graph
  -> inspect graph
  -> draw ggNetView plot
  -> calculate topology
  -> export plot, tables, graph object, and parameters
```

It does not add consensus networks, STRINGDB import, Mantel heatmaps, IVI, Zi-Pi, sample subgraphs, or multi-network comparison to the app UI. Those functions can exist in root `R/` after API migration, but they are not first-milestone Shiny modules.

## File Structure Map

Create or replace these app files:

- `app.R`: root direct app entry point.
- `R/launch_ggNetView.R`: formal package-style launcher.
- `R/app_registry.R`: session object registry helpers.
- `R/app_validation.R`: upload reading, type detection, object summaries, app result constructors.
- `R/app_adapters.R`: safe wrappers around ggNetView API calls.
- `R/app_exports.R`: CSV, RDS, JSON, PNG, and PDF export helpers.
- `inst/app/app.R`: direct Shiny app runner.
- `inst/app/global.R`: app-level package loading and source setup.
- `inst/app/ui.R`: top-level UI shell.
- `inst/app/server.R`: top-level server wiring.
- `inst/app/modules/mod_data_hub.R`: upload and object list module.
- `inst/app/modules/mod_graph_builder.R`: graph construction module.
- `inst/app/modules/mod_graph_explorer.R`: graph inspection module.
- `inst/app/modules/mod_visual_lab.R`: `ggNetView()` plotting module.
- `inst/app/modules/mod_topology_results.R`: topology table module.
- `inst/app/modules/mod_export_center.R`: export module.
- `inst/app/www/styles.css`: app-specific styling.
- `inst/extdata/example_matrix.csv`: small example matrix for smoke tests.

Create or replace these tests:

- `tests/testthat/test-app-registry.R`
- `tests/testthat/test-app-validation.R`
- `tests/testthat/test-app-adapters.R`
- `tests/testthat/test-launch.R`
- `tests/testthat/test-shiny-files.R`

Modify generated package metadata through roxygen where possible:

- `DESCRIPTION`
- `NAMESPACE`

Migration input:

- `package/ggNetView/R/*.R`
- `package/ggNetView/DESCRIPTION`
- `package/ggNetView/NAMESPACE`
- `ggNetView.shiny/inst/app/*`
- `ggNetView.shiny/R/launch_ggNetView.R`

## Task 1: Prepare Migration Branch And Baseline Checks

**Files:**
- Read: `docs/superpowers/specs/2026-05-25-ggnetview-shiny-redesign.md`
- Read: `DESCRIPTION`
- Read: `NAMESPACE`
- Read: `package/ggNetView/DESCRIPTION`
- Read: `package/ggNetView/NAMESPACE`

- [ ] **Step 1: Confirm worktree state**

Run:

```bash
git status --short --branch
```

Expected: the branch may be ahead by the spec commit and may show untracked `package/` and audit docs. Do not stage unrelated untracked docs unless a later step names them.

- [ ] **Step 2: Create an implementation branch**

Run:

```bash
git switch -c codex/ggnetview-shiny-redesign
```

Expected: Git reports the new branch name.

- [ ] **Step 3: Run baseline parse check on the new API source**

Run:

```bash
R_PROFILE_USER=/dev/null Rscript -e 'files <- list.files("package/ggNetView/R", pattern = "\\\\.R$", full.names = TRUE); invisible(lapply(files, parse)); cat("parsed", length(files), "R files\\n")'
```

Expected: prints `parsed <number> R files` with no parse errors.

- [ ] **Step 4: Run baseline root package check enough to capture current dependency state**

Run:

```bash
R_PROFILE_USER=/dev/null Rscript -e 'cat("R can start without project .Rprofile\\n")'
```

Expected: prints `R can start without project .Rprofile`.

- [ ] **Step 5: Commit only the branch marker if no files changed**

No commit is needed if no files changed.

## Task 2: Replace Root API With New ggNetView API

**Files:**
- Modify: `R/*.R`
- Source: `package/ggNetView/R/*.R`
- Modify: `DESCRIPTION`
- Modify: `NAMESPACE`
- Test: `tests/testthat/test-api-migration.R`

- [ ] **Step 1: Snapshot old root `R/` file names**

Run:

```bash
find R -maxdepth 1 -type f -name '*.R' -print | sort > /tmp/ggnetview-old-root-r-files.txt
find package/ggNetView/R -maxdepth 1 -type f -name '*.R' -print | sort > /tmp/ggnetview-new-api-r-files.txt
```

Expected: both files are created under `/tmp`.

- [ ] **Step 2: Replace root API files from the new source**

Run:

```bash
rm -f R/*.R
cp package/ggNetView/R/*.R R/
rm -f R/.DS_Store
```

Expected: root `R/` contains the same `.R` files as `package/ggNetView/R/`, excluding `.DS_Store`.

- [ ] **Step 3: Copy package metadata from the new source**

Run:

```bash
cp package/ggNetView/DESCRIPTION DESCRIPTION
cp package/ggNetView/NAMESPACE NAMESPACE
```

Expected: root package metadata matches the new package source.

- [ ] **Step 4: Add app dependencies to `DESCRIPTION`**

Edit `DESCRIPTION` so `Imports:` includes these app packages if absent:

```text
    bslib,
    DT,
    jsonlite,
    shiny,
```

Expected: each dependency appears once in `Imports:`.

- [ ] **Step 5: Add a migration test**

Create `tests/testthat/test-api-migration.R`:

```r
test_that("new ggNetView API files are present in root R directory", {
  expect_true(file.exists(test_path("../../R/build_graph_from_consensus.R")))
  expect_true(file.exists(test_path("../../R/get_node_centrality.R")))
  expect_true(file.exists(test_path("../../R/get_node_ivi.R")))
  expect_true(file.exists(test_path("../../R/get_sample_subgraph.R")))
})

test_that("first milestone API functions are available", {
  expect_true(exists("build_graph_from_mat", mode = "function"))
  expect_true(exists("build_graph_from_adj_mat", mode = "function"))
  expect_true(exists("build_graph_from_df", mode = "function"))
  expect_true(exists("ggNetView", mode = "function"))
  expect_true(exists("get_network_topology", mode = "function"))
})
```

- [ ] **Step 6: Run migration tests**

Run:

```bash
R_PROFILE_USER=/dev/null Rscript -e 'testthat::test_file("tests/testthat/test-api-migration.R")'
```

Expected: tests pass. If dependencies are missing locally, capture the missing package names and install or document them before continuing.

- [ ] **Step 7: Commit API migration**

Run:

```bash
git add R DESCRIPTION NAMESPACE tests/testthat/test-api-migration.R
git commit -m "refactor: replace root API with new ggNetView source"
```

Expected: one commit containing only API and metadata migration plus the migration test.

## Task 3: Add Launch Entrypoints

**Files:**
- Create: `app.R`
- Create: `R/launch_ggNetView.R`
- Create: `inst/app/app.R`
- Create: `inst/app/global.R`
- Test: `tests/testthat/test-launch.R`

- [ ] **Step 1: Write launch tests first**

Create `tests/testthat/test-launch.R`:

```r
test_that("launch_ggNetView exists and points at bundled app", {
  expect_true(exists("launch_ggNetView", mode = "function"))
  app_dir <- system.file("app", package = "ggNetView")
  expect_true(nzchar(app_dir))
})

test_that("root and bundled app files exist", {
  expect_true(file.exists(test_path("../../app.R")))
  expect_true(file.exists(test_path("../../inst/app/app.R")))
  expect_true(file.exists(test_path("../../inst/app/global.R")))
})
```

- [ ] **Step 2: Run launch tests and verify they fail before implementation**

Run:

```bash
R_PROFILE_USER=/dev/null Rscript -e 'testthat::test_file("tests/testthat/test-launch.R")'
```

Expected: fails because `launch_ggNetView()` or app files are not implemented in the root project.

- [ ] **Step 3: Create root direct app entry**

Create `app.R`:

```r
app_dir <- file.path("inst", "app")

if (!dir.exists(app_dir)) {
  stop("Cannot find Shiny app directory: ", normalizePath(app_dir, mustWork = FALSE), call. = FALSE)
}

shiny::runApp(app_dir, launch.browser = TRUE)
```

- [ ] **Step 4: Create formal launcher**

Create `R/launch_ggNetView.R`:

```r
#' Launch ggNetView Shiny
#'
#' Opens the bundled ggNetView Shiny application.
#'
#' @param launch.browser Logical. Open the app in a browser.
#' @param ... Additional arguments passed to [shiny::runApp()].
#'
#' @return The return value from [shiny::runApp()].
#' @export
launch_ggNetView <- function(launch.browser = TRUE, ...) {
  app_dir <- system.file("app", package = "ggNetView")

  if (!nzchar(app_dir)) {
    app_dir <- file.path(getwd(), "inst", "app")
  }

  if (!dir.exists(app_dir)) {
    stop("Cannot find ggNetView Shiny app directory.", call. = FALSE)
  }

  shiny::runApp(app_dir, launch.browser = launch.browser, ...)
}
```

- [ ] **Step 5: Create bundled app runner**

Create `inst/app/app.R`:

```r
source("global.R", local = TRUE)
source("ui.R", local = TRUE)
source("server.R", local = TRUE)

shiny::shinyApp(ui = ui, server = server)
```

- [ ] **Step 6: Create initial global file**

Create `inst/app/global.R`:

```r
library(shiny)
library(bslib)
library(DT)
library(ggplot2)
library(igraph)

app_root <- normalizePath(file.path("..", ".."), mustWork = FALSE)
```

- [ ] **Step 7: Ensure `launch_ggNetView()` is exported**

If roxygen is available, run:

```bash
R_PROFILE_USER=/dev/null Rscript -e 'roxygen2::roxygenise()'
```

If roxygen is not available, edit `NAMESPACE` and add:

```r
export(launch_ggNetView)
```

- [ ] **Step 8: Run launch tests**

Run:

```bash
R_PROFILE_USER=/dev/null Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-launch.R")'
```

Expected: tests pass.

- [ ] **Step 9: Commit launch entrypoints**

Run:

```bash
git add app.R R/launch_ggNetView.R inst/app/app.R inst/app/global.R NAMESPACE tests/testthat/test-launch.R
git commit -m "feat: add ggNetView Shiny launch entrypoints"
```

Expected: one commit containing launch entrypoints and tests.

## Task 4: Implement App Result And Object Registry

**Files:**
- Create: `R/app_validation.R`
- Create: `R/app_registry.R`
- Test: `tests/testthat/test-app-registry.R`

- [ ] **Step 1: Write registry tests**

Create `tests/testthat/test-app-registry.R`:

```r
test_that("registry can add, list, get, and delete objects", {
  registry <- registry_new()
  item <- registry_add(
    registry,
    name = "example matrix",
    type = "matrix",
    data = matrix(1:4, nrow = 2),
    source = "unit-test",
    params = list(alpha = 0.05),
    warnings = "small example"
  )

  expect_match(item$id, "^obj_")
  expect_equal(item$name, "example matrix")
  expect_equal(registry_count(registry), 1L)
  expect_equal(registry_get(registry, item$id)$type, "matrix")
  expect_equal(nrow(registry_list(registry)), 1L)

  registry_delete(registry, item$id)
  expect_equal(registry_count(registry), 0L)
})

test_that("registry summary records matrix dimensions", {
  registry <- registry_new()
  item <- registry_add(
    registry,
    name = "m",
    type = "matrix",
    data = matrix(1:9, nrow = 3)
  )

  expect_equal(item$summary$rows, 3L)
  expect_equal(item$summary$cols, 3L)
})
```

- [ ] **Step 2: Run registry tests and verify they fail before implementation**

Run:

```bash
R_PROFILE_USER=/dev/null Rscript -e 'testthat::test_file("tests/testthat/test-app-registry.R")'
```

Expected: fails because registry functions do not exist.

- [ ] **Step 3: Create app result constructors**

Create `R/app_validation.R` with this initial content:

```r
app_result <- function(ok, value = NULL, message = NULL, warnings = character(), trace = NULL) {
  structure(
    list(
      ok = isTRUE(ok),
      value = value,
      message = message,
      warnings = warnings,
      trace = trace
    ),
    class = "ggnetview_app_result"
  )
}

app_success <- function(value = NULL, message = NULL, warnings = character()) {
  app_result(TRUE, value = value, message = message, warnings = warnings)
}

app_failure <- function(message, trace = NULL, warnings = character()) {
  app_result(FALSE, value = NULL, message = message, warnings = warnings, trace = trace)
}
```

- [ ] **Step 4: Create registry implementation**

Create `R/app_registry.R`:

```r
registry_new <- function() {
  shiny::reactiveValues(items = list(), counter = 0L, log = list())
}

registry_next_id <- function(registry) {
  registry$counter <- registry$counter + 1L
  sprintf("obj_%04d", registry$counter)
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

  registry$items[[id]] <- item
  item
}

registry_get <- function(registry, id) {
  registry$items[[id]]
}

registry_delete <- function(registry, id) {
  registry$items[[id]] <- NULL
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

registry_log_error <- function(registry, context, error) {
  entry <- list(context = context, message = conditionMessage(error), created_at = Sys.time())
  registry$log[[length(registry$log) + 1L]] <- entry
  entry
}
```

- [ ] **Step 5: Run registry tests**

Run:

```bash
R_PROFILE_USER=/dev/null Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-app-registry.R")'
```

Expected: tests pass.

- [ ] **Step 6: Commit registry layer**

Run:

```bash
git add R/app_validation.R R/app_registry.R tests/testthat/test-app-registry.R
git commit -m "feat: add Shiny object registry"
```

Expected: one commit containing registry and result constructors.

## Task 5: Implement Upload Reading And Validation

**Files:**
- Modify: `R/app_validation.R`
- Create: `inst/extdata/example_matrix.csv`
- Test: `tests/testthat/test-app-validation.R`

- [ ] **Step 1: Create example matrix fixture**

Create `inst/extdata/example_matrix.csv`:

```csv
taxon,S1,S2,S3,S4,S5
Taxon_A,10,11,13,12,15
Taxon_B,5,7,6,8,7
Taxon_C,20,18,21,19,22
Taxon_D,3,4,5,3,6
Taxon_E,9,8,10,11,9
```

- [ ] **Step 2: Write validation tests**

Create `tests/testthat/test-app-validation.R`:

```r
test_that("read_user_table reads first column as row names", {
  path <- test_path("../../inst/extdata/example_matrix.csv")
  tbl <- read_user_table(path)

  expect_true(is.data.frame(tbl))
  expect_equal(rownames(tbl)[1], "Taxon_A")
  expect_equal(ncol(tbl), 5L)
})

test_that("detect_upload_type identifies numeric matrix", {
  mat <- data.frame(S1 = c(1, 2), S2 = c(3, 4), row.names = c("A", "B"))
  expect_equal(detect_upload_type(mat), "matrix")
})

test_that("validate_matrix_like rejects non-numeric cells", {
  mat <- data.frame(S1 = c("x", "y"), S2 = c("1", "2"), row.names = c("A", "B"))
  result <- validate_matrix_like(mat)

  expect_false(result$ok)
  expect_match(result$message, "numeric")
})

test_that("validate_matrix_like converts numeric data frames to matrix", {
  mat <- data.frame(S1 = c(1, 2), S2 = c(3, 4), row.names = c("A", "B"))
  result <- validate_matrix_like(mat)

  expect_true(result$ok)
  expect_true(is.matrix(result$value))
  expect_equal(storage.mode(result$value), "double")
})
```

- [ ] **Step 3: Run validation tests and verify they fail before implementation**

Run:

```bash
R_PROFILE_USER=/dev/null Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-app-validation.R")'
```

Expected: fails because upload helpers do not exist.

- [ ] **Step 4: Append upload helpers to `R/app_validation.R`**

Append:

```r
read_user_table <- function(path) {
  ext <- tolower(tools::file_ext(path))
  if (!ext %in% c("csv", "tsv", "txt")) {
    stop("Unsupported file type. Please upload a CSV, TSV, or TXT file.", call. = FALSE)
  }

  delim <- if (identical(ext, "csv")) "," else "\t"
  data <- utils::read.table(
    path,
    header = TRUE,
    sep = delim,
    check.names = FALSE,
    stringsAsFactors = FALSE,
    comment.char = "",
    quote = "\""
  )

  if (ncol(data) < 2L) {
    stop("Uploaded table must contain an ID column and at least one data column.", call. = FALSE)
  }

  ids <- data[[1]]
  if (anyDuplicated(ids)) {
    stop("The first column contains duplicate IDs. Please make row IDs unique.", call. = FALSE)
  }

  rownames(data) <- ids
  data[[1]] <- NULL
  data
}

detect_upload_type <- function(data) {
  if (is.matrix(data) || is.data.frame(data)) {
    numeric_cols <- vapply(data, function(x) all(!is.na(suppressWarnings(as.numeric(x)))), logical(1))
    if (all(numeric_cols)) {
      return("matrix")
    }
  }

  if (is.data.frame(data) && all(c("from", "to") %in% names(data))) {
    return("edge_table")
  }

  "table"
}

validate_matrix_like <- function(data) {
  if (!is.matrix(data) && !is.data.frame(data)) {
    return(app_failure("Input must be a matrix or data frame."))
  }

  converted <- suppressWarnings(as.matrix(data))
  storage.mode(converted) <- "double"

  if (anyNA(converted)) {
    return(app_failure("Matrix input must be numeric and cannot contain non-numeric cells."))
  }

  if (is.null(rownames(converted)) || anyDuplicated(rownames(converted))) {
    return(app_failure("Matrix input must have unique row names."))
  }

  app_success(converted)
}
```

- [ ] **Step 5: Run validation tests**

Run:

```bash
R_PROFILE_USER=/dev/null Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-app-validation.R")'
```

Expected: tests pass.

- [ ] **Step 6: Commit validation layer**

Run:

```bash
git add R/app_validation.R inst/extdata/example_matrix.csv tests/testthat/test-app-validation.R
git commit -m "feat: add upload validation helpers"
```

Expected: one commit containing validation helpers and example data.

## Task 6: Implement Safe API Adapters

**Files:**
- Create: `R/app_adapters.R`
- Test: `tests/testthat/test-app-adapters.R`

- [ ] **Step 1: Write adapter tests**

Create `tests/testthat/test-app-adapters.R`:

```r
test_that("safe_build_graph returns failure for unknown builder", {
  result <- safe_build_graph(
    data = matrix(1:4, nrow = 2, dimnames = list(c("A", "B"), c("S1", "S2"))),
    builder = "missing_builder",
    params = list()
  )

  expect_false(result$ok)
  expect_match(result$message, "Unsupported graph builder")
})

test_that("safe_plot_ggnetview rejects non-graph input", {
  result <- safe_plot_ggnetview(graph = data.frame(x = 1), params = list())

  expect_false(result$ok)
  expect_match(result$message, "graph")
})

test_that("safe_topology rejects non-graph input", {
  result <- safe_topology(graph = data.frame(x = 1))

  expect_false(result$ok)
  expect_match(result$message, "graph")
})
```

- [ ] **Step 2: Run adapter tests and verify they fail before implementation**

Run:

```bash
R_PROFILE_USER=/dev/null Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-app-adapters.R")'
```

Expected: fails because adapter functions do not exist.

- [ ] **Step 3: Create adapter implementation**

Create `R/app_adapters.R`:

```r
safe_call <- function(expr, user_message) {
  tryCatch(
    app_success(force(expr)),
    error = function(e) app_failure(user_message, trace = conditionMessage(e))
  )
}

safe_build_graph <- function(data, builder, params = list()) {
  builder_map <- list(
    matrix = "build_graph_from_mat",
    adjacency = "build_graph_from_adj_mat",
    edge_table = "build_graph_from_df"
  )

  fn_name <- builder_map[[builder]]
  if (is.null(fn_name) || !exists(fn_name, mode = "function")) {
    return(app_failure(paste("Unsupported graph builder:", builder)))
  }

  fn <- get(fn_name, mode = "function")
  safe_call(
    do.call(fn, c(list(data), params)),
    paste("Failed to build graph with", fn_name)
  )
}

safe_plot_ggnetview <- function(graph, params = list()) {
  if (!inherits(graph, "igraph")) {
    return(app_failure("Visual Lab requires an igraph graph object."))
  }

  safe_call(
    do.call(ggNetView, c(list(graph), params)),
    "Failed to generate ggNetView plot."
  )
}

safe_topology <- function(graph, params = list()) {
  if (!inherits(graph, "igraph")) {
    return(app_failure("Topology Results requires an igraph graph object."))
  }

  safe_call(
    do.call(get_network_topology, c(list(graph), params)),
    "Failed to calculate network topology."
  )
}
```

- [ ] **Step 4: Run adapter tests**

Run:

```bash
R_PROFILE_USER=/dev/null Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-app-adapters.R")'
```

Expected: tests pass.

- [ ] **Step 5: Commit adapters**

Run:

```bash
git add R/app_adapters.R tests/testthat/test-app-adapters.R
git commit -m "feat: add safe ggNetView app adapters"
```

Expected: one commit containing safe wrappers and tests.

## Task 7: Implement Export Helpers

**Files:**
- Create: `R/app_exports.R`
- Test: `tests/testthat/test-app-exports.R`

- [ ] **Step 1: Write export tests**

Create `tests/testthat/test-app-exports.R`:

```r
test_that("write_registry_table writes CSV", {
  path <- tempfile(fileext = ".csv")
  write_registry_table(data.frame(a = 1, b = 2), path)

  expect_true(file.exists(path))
  expect_equal(nrow(utils::read.csv(path)), 1L)
})

test_that("write_registry_params writes JSON", {
  path <- tempfile(fileext = ".json")
  write_registry_params(list(alpha = 0.05, method = "spearman"), path)

  expect_true(file.exists(path))
  txt <- readLines(path, warn = FALSE)
  expect_true(any(grepl("alpha", txt)))
})
```

- [ ] **Step 2: Run export tests and verify they fail before implementation**

Run:

```bash
R_PROFILE_USER=/dev/null Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-app-exports.R")'
```

Expected: fails because export helpers do not exist.

- [ ] **Step 3: Create export helper implementation**

Create `R/app_exports.R`:

```r
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
```

- [ ] **Step 4: Run export tests**

Run:

```bash
R_PROFILE_USER=/dev/null Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-app-exports.R")'
```

Expected: tests pass.

- [ ] **Step 5: Commit export helpers**

Run:

```bash
git add R/app_exports.R tests/testthat/test-app-exports.R
git commit -m "feat: add Shiny export helpers"
```

Expected: one commit containing export helpers and tests.

## Task 8: Build Shiny Module Skeleton And UI Shell

**Files:**
- Create: `inst/app/modules/mod_data_hub.R`
- Create: `inst/app/modules/mod_graph_builder.R`
- Create: `inst/app/modules/mod_graph_explorer.R`
- Create: `inst/app/modules/mod_visual_lab.R`
- Create: `inst/app/modules/mod_topology_results.R`
- Create: `inst/app/modules/mod_export_center.R`
- Replace: `inst/app/ui.R`
- Replace: `inst/app/server.R`
- Modify: `inst/app/global.R`
- Create: `inst/app/www/styles.css`
- Test: `tests/testthat/test-shiny-files.R`

- [ ] **Step 1: Write file existence tests**

Create `tests/testthat/test-shiny-files.R`:

```r
test_that("first milestone Shiny module files exist", {
  files <- c(
    "inst/app/modules/mod_data_hub.R",
    "inst/app/modules/mod_graph_builder.R",
    "inst/app/modules/mod_graph_explorer.R",
    "inst/app/modules/mod_visual_lab.R",
    "inst/app/modules/mod_topology_results.R",
    "inst/app/modules/mod_export_center.R",
    "inst/app/ui.R",
    "inst/app/server.R",
    "inst/app/www/styles.css"
  )

  expect_true(all(file.exists(test_path("../../", files))))
})
```

- [ ] **Step 2: Run Shiny file tests and verify they fail before implementation**

Run:

```bash
R_PROFILE_USER=/dev/null Rscript -e 'testthat::test_file("tests/testthat/test-shiny-files.R")'
```

Expected: fails because new module files do not exist.

- [ ] **Step 3: Update `inst/app/global.R` to source modules**

Replace `inst/app/global.R`:

```r
library(shiny)
library(bslib)
library(DT)
library(ggplot2)
library(igraph)

module_files <- list.files("modules", pattern = "\\\\.R$", full.names = TRUE)
invisible(lapply(module_files, source, local = FALSE))
```

- [ ] **Step 4: Create top-level UI shell**

Create `inst/app/ui.R`:

```r
ui <- bslib::page_navbar(
  title = "ggNetView",
  theme = bslib::bs_theme(version = 5, bootswatch = "flatly"),
  header = tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
  bslib::nav_panel("Data Hub", mod_data_hub_ui("data_hub")),
  bslib::nav_panel("Graph Builder", mod_graph_builder_ui("graph_builder")),
  bslib::nav_panel("Graph Explorer", mod_graph_explorer_ui("graph_explorer")),
  bslib::nav_panel("Visual Lab", mod_visual_lab_ui("visual_lab")),
  bslib::nav_panel("Topology", mod_topology_results_ui("topology_results")),
  bslib::nav_panel("Export", mod_export_center_ui("export_center"))
)
```

- [ ] **Step 5: Create top-level server shell**

Create `inst/app/server.R`:

```r
server <- function(input, output, session) {
  registry <- registry_new()

  mod_data_hub_server("data_hub", registry)
  mod_graph_builder_server("graph_builder", registry)
  mod_graph_explorer_server("graph_explorer", registry)
  mod_visual_lab_server("visual_lab", registry)
  mod_topology_results_server("topology_results", registry)
  mod_export_center_server("export_center", registry)
}
```

- [ ] **Step 6: Create temporary module shells**

Create each module with a clear empty state. For `inst/app/modules/mod_data_hub.R`:

```r
mod_data_hub_ui <- function(id) {
  ns <- NS(id)
  bslib::layout_columns(
    card(
      card_header("Upload"),
      fileInput(ns("file"), "Upload CSV, TSV, or TXT"),
      textInput(ns("object_name"), "Object name", value = "uploaded_matrix"),
      actionButton(ns("register"), "Register object")
    ),
    card(
      card_header("Objects"),
      DT::DTOutput(ns("objects"))
    )
  )
}

mod_data_hub_server <- function(id, registry) {
  moduleServer(id, function(input, output, session) {
    output$objects <- DT::renderDT(registry_list(registry), rownames = FALSE)
  })
}
```

Create the other five modules with the same shell pattern, changing names and IDs:

```r
mod_graph_builder_ui <- function(id) {
  ns <- NS(id)
  card(card_header("Graph Builder"), selectInput(ns("source_id"), "Source object", choices = character()), actionButton(ns("build"), "Build graph"))
}

mod_graph_builder_server <- function(id, registry) {
  moduleServer(id, function(input, output, session) {})
}

mod_graph_explorer_ui <- function(id) {
  ns <- NS(id)
  card(card_header("Graph Explorer"), selectInput(ns("graph_id"), "Graph object", choices = character()), DT::DTOutput(ns("nodes")), DT::DTOutput(ns("edges")))
}

mod_graph_explorer_server <- function(id, registry) {
  moduleServer(id, function(input, output, session) {})
}

mod_visual_lab_ui <- function(id) {
  ns <- NS(id)
  card(card_header("Visual Lab"), selectInput(ns("graph_id"), "Graph object", choices = character()), actionButton(ns("draw"), "Draw"), plotOutput(ns("plot"), height = 650))
}

mod_visual_lab_server <- function(id, registry) {
  moduleServer(id, function(input, output, session) {})
}

mod_topology_results_ui <- function(id) {
  ns <- NS(id)
  card(card_header("Topology Results"), selectInput(ns("graph_id"), "Graph object", choices = character()), actionButton(ns("calculate"), "Calculate"), DT::DTOutput(ns("topology")))
}

mod_topology_results_server <- function(id, registry) {
  moduleServer(id, function(input, output, session) {})
}

mod_export_center_ui <- function(id) {
  ns <- NS(id)
  card(card_header("Export Center"), selectInput(ns("object_id"), "Object", choices = character()), downloadButton(ns("download_rds"), "Download RDS"))
}

mod_export_center_server <- function(id, registry) {
  moduleServer(id, function(input, output, session) {})
}
```

- [ ] **Step 7: Add minimal CSS**

Create `inst/app/www/styles.css`:

```css
body {
  font-size: 15px;
}

.card {
  border-radius: 8px;
}

.form-label {
  font-weight: 600;
}
```

- [ ] **Step 8: Run Shiny file tests**

Run:

```bash
R_PROFILE_USER=/dev/null Rscript -e 'testthat::test_file("tests/testthat/test-shiny-files.R")'
```

Expected: tests pass.

- [ ] **Step 9: Run app parse check**

Run:

```bash
R_PROFILE_USER=/dev/null Rscript -e 'files <- c("inst/app/global.R", "inst/app/ui.R", "inst/app/server.R", list.files("inst/app/modules", pattern = "\\\\.R$", full.names = TRUE)); invisible(lapply(files, parse)); cat("app files parse\\n")'
```

Expected: prints `app files parse`.

- [ ] **Step 10: Commit Shiny module skeleton**

Run:

```bash
git add inst/app tests/testthat/test-shiny-files.R
git commit -m "feat: add ggNetView Shiny module shell"
```

Expected: one commit containing the app shell and module files.

## Task 9: Implement Data Hub

**Files:**
- Modify: `inst/app/modules/mod_data_hub.R`
- Test: `tests/testthat/test-app-validation.R`

- [ ] **Step 1: Replace Data Hub server with upload registration logic**

Modify `inst/app/modules/mod_data_hub.R` so the server is:

```r
mod_data_hub_server <- function(id, registry) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$register, {
      req(input$file)

      result <- tryCatch(
        {
          table <- read_user_table(input$file$datapath)
          type <- detect_upload_type(table)
          validation <- if (identical(type, "matrix")) validate_matrix_like(table) else app_success(table)

          if (!validation$ok) {
            showNotification(validation$message, type = "error")
            return(NULL)
          }

          item <- registry_add(
            registry,
            name = input$object_name,
            type = type,
            data = validation$value,
            source = input$file$name,
            warnings = validation$warnings
          )

          showNotification(paste("Registered", item$name), type = "message")
          item
        },
        error = function(e) {
          showNotification(conditionMessage(e), type = "error")
          NULL
        }
      )

      invisible(result)
    })

    output$objects <- DT::renderDT(registry_list(registry), rownames = FALSE)
  })
}
```

- [ ] **Step 2: Run validation tests**

Run:

```bash
R_PROFILE_USER=/dev/null Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-app-validation.R")'
```

Expected: tests pass.

- [ ] **Step 3: Run app parse check**

Run:

```bash
R_PROFILE_USER=/dev/null Rscript -e 'invisible(lapply(list.files("inst/app/modules", pattern = "\\\\.R$", full.names = TRUE), parse)); cat("modules parse\\n")'
```

Expected: prints `modules parse`.

- [ ] **Step 4: Commit Data Hub**

Run:

```bash
git add inst/app/modules/mod_data_hub.R
git commit -m "feat: wire Data Hub uploads to registry"
```

Expected: one commit containing Data Hub behavior.

## Task 10: Implement Graph Builder And Graph Explorer

**Files:**
- Modify: `inst/app/modules/mod_graph_builder.R`
- Modify: `inst/app/modules/mod_graph_explorer.R`

- [ ] **Step 1: Add registry choice helper to `R/app_registry.R`**

Append:

```r
registry_choices <- function(registry, type = NULL) {
  listed <- registry_list(registry, type = type)
  if (!nrow(listed)) {
    return(stats::setNames(character(), character()))
  }
  stats::setNames(listed$id, paste0(listed$name, " [", listed$type, "]"))
}
```

- [ ] **Step 2: Implement Graph Builder server**

Modify `inst/app/modules/mod_graph_builder.R`:

```r
mod_graph_builder_ui <- function(id) {
  ns <- NS(id)
  bslib::layout_columns(
    card(
      card_header("Build Graph"),
      selectInput(ns("source_id"), "Source object", choices = character()),
      selectInput(
        ns("builder"),
        "Builder",
        choices = c("Matrix" = "matrix", "Adjacency matrix" = "adjacency", "Edge table" = "edge_table")
      ),
      textInput(ns("graph_name"), "Graph name", value = "network_graph"),
      actionButton(ns("build"), "Build graph")
    ),
    card(card_header("Build status"), verbatimTextOutput(ns("status")))
  )
}

mod_graph_builder_server <- function(id, registry) {
  moduleServer(id, function(input, output, session) {
    observe({
      updateSelectInput(session, "source_id", choices = registry_choices(registry))
    })

    status <- reactiveVal("No graph built yet.")

    observeEvent(input$build, {
      req(input$source_id)
      source <- registry_get(registry, input$source_id)
      req(source)

      result <- safe_build_graph(source$data, input$builder, params = list())
      if (!result$ok) {
        status(result$message)
        showNotification(result$message, type = "error")
        return()
      }

      item <- registry_add(
        registry,
        name = input$graph_name,
        type = "graph",
        data = result$value,
        source = source$id,
        params = list(builder = input$builder)
      )
      status(paste("Built graph:", item$name))
      showNotification(paste("Built graph:", item$name), type = "message")
    })

    output$status <- renderText(status())
  })
}
```

- [ ] **Step 3: Implement Graph Explorer server**

Modify `inst/app/modules/mod_graph_explorer.R`:

```r
mod_graph_explorer_ui <- function(id) {
  ns <- NS(id)
  bslib::layout_columns(
    card(card_header("Select Graph"), selectInput(ns("graph_id"), "Graph object", choices = character())),
    card(card_header("Summary"), verbatimTextOutput(ns("summary"))),
    col_widths = c(4, 8)
  )
}

mod_graph_explorer_server <- function(id, registry) {
  moduleServer(id, function(input, output, session) {
    observe({
      updateSelectInput(session, "graph_id", choices = registry_choices(registry, type = "graph"))
    })

    selected_graph <- reactive({
      req(input$graph_id)
      registry_get(registry, input$graph_id)
    })

    output$summary <- renderPrint({
      item <- selected_graph()
      req(item)
      print(item$summary)
    })
  })
}
```

- [ ] **Step 4: Run registry and adapter tests**

Run:

```bash
R_PROFILE_USER=/dev/null Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-app-registry.R"); testthat::test_file("tests/testthat/test-app-adapters.R")'
```

Expected: tests pass.

- [ ] **Step 5: Run app parse check**

Run:

```bash
R_PROFILE_USER=/dev/null Rscript -e 'files <- c("R/app_registry.R", "inst/app/modules/mod_graph_builder.R", "inst/app/modules/mod_graph_explorer.R"); invisible(lapply(files, parse)); cat("graph module files parse\\n")'
```

Expected: prints `graph module files parse`.

- [ ] **Step 6: Commit graph modules**

Run:

```bash
git add R/app_registry.R inst/app/modules/mod_graph_builder.R inst/app/modules/mod_graph_explorer.R
git commit -m "feat: add graph builder and explorer modules"
```

Expected: one commit containing graph choice helpers and graph modules.

## Task 11: Implement Visual Lab And Topology Results

**Files:**
- Modify: `inst/app/modules/mod_visual_lab.R`
- Modify: `inst/app/modules/mod_topology_results.R`

- [ ] **Step 1: Implement Visual Lab**

Modify `inst/app/modules/mod_visual_lab.R`:

```r
mod_visual_lab_ui <- function(id) {
  ns <- NS(id)
  bslib::layout_sidebar(
    sidebar = sidebar(
      selectInput(ns("graph_id"), "Graph object", choices = character()),
      selectInput(ns("layout"), "Layout", choices = c("nicely", "fr", "kk", "circle")),
      selectInput(ns("label_layout"), "Label layout", choices = c("auto", "inside", "outside")),
      numericInput(ns("label_wrap_width"), "Label wrap width", value = 18, min = 4, max = 80),
      numericInput(ns("bandwidth_scale"), "Bandwidth scale", value = 1, min = 0.1, max = 5, step = 0.1),
      actionButton(ns("draw"), "Draw")
    ),
    card(card_header("Preview"), plotOutput(ns("plot"), height = 650))
  )
}

mod_visual_lab_server <- function(id, registry) {
  moduleServer(id, function(input, output, session) {
    plot_obj <- reactiveVal(NULL)

    observe({
      updateSelectInput(session, "graph_id", choices = registry_choices(registry, type = "graph"))
    })

    observeEvent(input$draw, {
      req(input$graph_id)
      graph_item <- registry_get(registry, input$graph_id)
      req(graph_item)

      params <- list(
        layout = input$layout,
        label_layout = input$label_layout,
        label_wrap_width = input$label_wrap_width,
        bandwidth_scale = input$bandwidth_scale
      )

      result <- safe_plot_ggnetview(graph_item$data, params = params)
      if (!result$ok) {
        showNotification(result$message, type = "error")
        return()
      }

      plot_obj(result$value)
      registry_add(
        registry,
        name = paste0(graph_item$name, "_plot"),
        type = "plot",
        data = result$value,
        source = graph_item$id,
        params = params
      )
    })

    output$plot <- renderPlot({
      req(plot_obj())
      plot_obj()
    })
  })
}
```

- [ ] **Step 2: Implement Topology Results**

Modify `inst/app/modules/mod_topology_results.R`:

```r
mod_topology_results_ui <- function(id) {
  ns <- NS(id)
  bslib::layout_columns(
    card(
      card_header("Calculate"),
      selectInput(ns("graph_id"), "Graph object", choices = character()),
      actionButton(ns("calculate"), "Calculate topology")
    ),
    card(card_header("Topology"), DT::DTOutput(ns("topology"))),
    col_widths = c(4, 8)
  )
}

mod_topology_results_server <- function(id, registry) {
  moduleServer(id, function(input, output, session) {
    topology_table <- reactiveVal(data.frame())

    observe({
      updateSelectInput(session, "graph_id", choices = registry_choices(registry, type = "graph"))
    })

    observeEvent(input$calculate, {
      req(input$graph_id)
      graph_item <- registry_get(registry, input$graph_id)
      req(graph_item)

      result <- safe_topology(graph_item$data)
      if (!result$ok) {
        showNotification(result$message, type = "error")
        return()
      }

      table <- as.data.frame(result$value)
      topology_table(table)
      registry_add(
        registry,
        name = paste0(graph_item$name, "_topology"),
        type = "result",
        data = table,
        source = graph_item$id,
        params = list(metric = "network_topology")
      )
    })

    output$topology <- DT::renderDT(topology_table(), rownames = FALSE)
  })
}
```

- [ ] **Step 3: Run app parse check**

Run:

```bash
R_PROFILE_USER=/dev/null Rscript -e 'files <- c("inst/app/modules/mod_visual_lab.R", "inst/app/modules/mod_topology_results.R"); invisible(lapply(files, parse)); cat("visual and topology modules parse\\n")'
```

Expected: prints `visual and topology modules parse`.

- [ ] **Step 4: Run adapter tests**

Run:

```bash
R_PROFILE_USER=/dev/null Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-app-adapters.R")'
```

Expected: tests pass.

- [ ] **Step 5: Commit Visual Lab and Topology Results**

Run:

```bash
git add inst/app/modules/mod_visual_lab.R inst/app/modules/mod_topology_results.R
git commit -m "feat: add visual and topology modules"
```

Expected: one commit containing Visual Lab and Topology Results.

## Task 12: Implement Export Center

**Files:**
- Modify: `inst/app/modules/mod_export_center.R`

- [ ] **Step 1: Replace Export Center module**

Modify `inst/app/modules/mod_export_center.R`:

```r
mod_export_center_ui <- function(id) {
  ns <- NS(id)
  card(
    card_header("Export Center"),
    selectInput(ns("object_id"), "Object", choices = character()),
    downloadButton(ns("download_rds"), "Download RDS"),
    downloadButton(ns("download_csv"), "Download CSV"),
    downloadButton(ns("download_params"), "Download Parameters")
  )
}

mod_export_center_server <- function(id, registry) {
  moduleServer(id, function(input, output, session) {
    observe({
      updateSelectInput(session, "object_id", choices = registry_choices(registry))
    })

    selected_item <- reactive({
      req(input$object_id)
      registry_get(registry, input$object_id)
    })

    output$download_rds <- downloadHandler(
      filename = function() paste0(selected_item()$name, ".rds"),
      content = function(file) write_registry_object(selected_item()$data, file)
    )

    output$download_csv <- downloadHandler(
      filename = function() paste0(selected_item()$name, ".csv"),
      content = function(file) {
        data <- selected_item()$data
        if (is.matrix(data)) {
          data <- as.data.frame(data)
        }
        if (!is.data.frame(data)) {
          data <- data.frame(value = capture.output(str(data)))
        }
        write_registry_table(data, file)
      }
    )

    output$download_params <- downloadHandler(
      filename = function() paste0(selected_item()$name, "_params.json"),
      content = function(file) write_registry_params(selected_item()$params, file)
    )
  })
}
```

- [ ] **Step 2: Run export tests**

Run:

```bash
R_PROFILE_USER=/dev/null Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-app-exports.R")'
```

Expected: tests pass.

- [ ] **Step 3: Run app parse check**

Run:

```bash
R_PROFILE_USER=/dev/null Rscript -e 'parse("inst/app/modules/mod_export_center.R"); cat("export module parses\\n")'
```

Expected: prints `export module parses`.

- [ ] **Step 4: Commit Export Center**

Run:

```bash
git add inst/app/modules/mod_export_center.R
git commit -m "feat: add Export Center module"
```

Expected: one commit containing Export Center wiring.

## Task 13: Add Workflow Smoke Script

**Files:**
- Create: `tests/run_shiny_core_workflow_smoke.R`

- [ ] **Step 1: Create smoke script**

Create `tests/run_shiny_core_workflow_smoke.R`:

```r
Sys.setenv(R_PROFILE_USER = "/dev/null")
devtools::load_all(".")

matrix_path <- file.path("inst", "extdata", "example_matrix.csv")
mat <- read_user_table(matrix_path)
valid <- validate_matrix_like(mat)
stopifnot(valid$ok)

graph_result <- safe_build_graph(valid$value, builder = "matrix", params = list())
stopifnot(graph_result$ok)

topology_result <- safe_topology(graph_result$value)
stopifnot(topology_result$ok)

plot_result <- safe_plot_ggnetview(graph_result$value, params = list())
stopifnot(plot_result$ok)

tmpdir <- tempfile("ggnetview-smoke-")
dir.create(tmpdir)
write_registry_object(graph_result$value, file.path(tmpdir, "graph.rds"))
write_registry_params(list(builder = "matrix"), file.path(tmpdir, "params.json"))

cat("core workflow smoke passed\\n")
```

- [ ] **Step 2: Run smoke script**

Run:

```bash
R_PROFILE_USER=/dev/null Rscript tests/run_shiny_core_workflow_smoke.R
```

Expected: prints `core workflow smoke passed`.

- [ ] **Step 3: Commit smoke script**

Run:

```bash
git add tests/run_shiny_core_workflow_smoke.R
git commit -m "test: add Shiny core workflow smoke"
```

Expected: one commit containing the smoke script.

## Task 14: Verify App Startup

**Files:**
- Read: `app.R`
- Read: `inst/app/app.R`
- Read: `R/launch_ggNetView.R`

- [ ] **Step 1: Run testthat suite**

Run:

```bash
R_PROFILE_USER=/dev/null Rscript -e 'devtools::load_all("."); testthat::test_dir("tests/testthat")'
```

Expected: all available tests pass. If optional scientific dependencies are missing, record exact missing package names and install them before claiming completion.

- [ ] **Step 2: Check direct app object creation without opening a browser**

Run:

```bash
R_PROFILE_USER=/dev/null Rscript -e 'setwd("inst/app"); source("global.R"); source("ui.R"); source("server.R"); app <- shiny::shinyApp(ui, server); stopifnot(inherits(app, "shiny.appobj")); cat("shiny app object created\\n")'
```

Expected: prints `shiny app object created`.

- [ ] **Step 3: Check formal launcher can resolve app path**

Run:

```bash
R_PROFILE_USER=/dev/null Rscript -e 'devtools::load_all("."); app_dir <- system.file("app", package = "ggNetView"); if (!nzchar(app_dir)) app_dir <- file.path(getwd(), "inst", "app"); stopifnot(dir.exists(app_dir)); cat(app_dir, "\\n")'
```

Expected: prints a valid `inst/app` path.

- [ ] **Step 4: Run root package build check**

Run:

```bash
R_PROFILE_USER=/dev/null R CMD build .
```

Expected: source tarball builds. If dependency installation prevents `R CMD check`, report that separately with exact missing dependencies.

- [ ] **Step 5: Commit verification notes if a small README update is needed**

If startup commands have changed, update `README.md` with:

```markdown
## Run the Shiny app

```r
shiny::runApp("inst/app")
```

or:

```r
ggNetView::launch_ggNetView()
```
```

Then run:

```bash
git add README.md
git commit -m "docs: document Shiny launch commands"
```

Expected: README contains both supported launch paths.

## Task 15: Retire Old `ggNetView.shiny/` Structure

**Files:**
- Remove or archive: `ggNetView.shiny/`
- Modify: `.Rbuildignore`
- Modify: `README.md`

- [ ] **Step 1: Confirm new app passes startup checks**

Run:

```bash
R_PROFILE_USER=/dev/null Rscript tests/run_shiny_core_workflow_smoke.R
R_PROFILE_USER=/dev/null Rscript -e 'setwd("inst/app"); source("global.R"); source("ui.R"); source("server.R"); app <- shiny::shinyApp(ui, server); stopifnot(inherits(app, "shiny.appobj")); cat("startup check passed\\n")'
```

Expected: both checks pass.

- [ ] **Step 2: Remove old Shiny package directory**

Run:

```bash
git rm -r ggNetView.shiny
```

Expected: Git stages deletion of the old separate Shiny package.

- [ ] **Step 3: Ensure package build ignores design artifacts if needed**

If `.Rbuildignore` exists, add:

```text
^docs/superpowers$
^package$
```

If `.Rbuildignore` does not exist, create it with:

```text
^docs/superpowers$
^package$
^.*\\.Rproj$
^\\.Rproj\\.user$
```

- [ ] **Step 4: Commit old package retirement**

Run:

```bash
git add .Rbuildignore README.md
git commit -m "chore: retire separate ggNetView.shiny package"
```

Expected: one commit removing `ggNetView.shiny/` and preserving the root app as the Shiny home.

## Task 16: Final Verification And Handoff

**Files:**
- Read: `docs/superpowers/specs/2026-05-25-ggnetview-shiny-redesign.md`
- Read: `docs/superpowers/plans/2026-05-25-ggnetview-shiny-redesign.md`
- Modify if needed: `README.md`

- [ ] **Step 1: Run all planned verification commands**

Run:

```bash
R_PROFILE_USER=/dev/null Rscript -e 'devtools::load_all("."); testthat::test_dir("tests/testthat")'
R_PROFILE_USER=/dev/null Rscript tests/run_shiny_core_workflow_smoke.R
R_PROFILE_USER=/dev/null Rscript -e 'setwd("inst/app"); source("global.R"); source("ui.R"); source("server.R"); app <- shiny::shinyApp(ui, server); stopifnot(inherits(app, "shiny.appobj")); cat("shiny app object created\\n")'
R_PROFILE_USER=/dev/null R CMD build .
```

Expected:

- testthat suite passes;
- smoke script prints `core workflow smoke passed`;
- app object creation prints `shiny app object created`;
- `R CMD build .` creates a tarball.

- [ ] **Step 2: Inspect git status**

Run:

```bash
git status --short --branch
```

Expected: clean working tree except intentionally untracked local artifacts such as generated tarballs. Remove generated tarballs after recording the build result.

- [ ] **Step 3: Produce handoff summary**

Write the final response with:

- branch name;
- commits made;
- changed architecture summary;
- exact verification commands and outcomes;
- any missing dependency blockers;
- next recommended phase.

Expected: user can decide whether to merge, continue with polish, or expand into second-milestone modules.

## Self-Review Checklist

- Spec coverage: The plan covers API migration, dual launch entrypoints, object registry, validation, adapters, modules, exports, smoke testing, startup testing, and retirement of `ggNetView.shiny/`.
- Scope control: First milestone remains the core workflow only.
- Type consistency: Registry item fields use `id`, `name`, `type`, `data`, `summary`, `created_at`, `source`, `params`, and `warnings` throughout.
- Test flow: Each implementation layer has a failing-test step, implementation step, pass step, and commit step.
- Execution safety: Old `ggNetView.shiny/` is removed only after the new app startup and smoke checks pass.
