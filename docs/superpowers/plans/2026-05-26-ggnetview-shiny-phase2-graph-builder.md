# ggNetView Shiny Phase 2 Graph Builder Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Build the Phase 2 input registry and Graph Builder layer so ggNetView Shiny can create graph objects from the manual's main graph-construction inputs.

**Architecture:** Keep the current Shiny module structure and add focused app helpers instead of embedding ggNetView API details inside UI observers. Data Hub classifies uploaded objects into typed registry items. Graph Builder routes a selected mode to a small adapter that validates inputs, calls the correct ggNetView function, and registers graph/result objects with source IDs and params.

**Tech Stack:** R, Shiny, bslib, igraph, tidygraph, ggplot2, jsonlite, testthat, shinytest2 or existing browser smoke scripts, `/usr/local/bin/Rscript`.

---

## Design Inputs

- Spec: `docs/superpowers/specs/2026-05-26-ggnetview-shiny-manual-coverage-design.md`
- Current app modules: `inst/app/modules/`
- Current app helpers: `R/app_adapters.R`, `R/app_registry.R`, `R/app_validation.R`, `R/app_exports.R`
- Current smoke baseline: `docs/ggnetview-shiny-next-todos.md`
- Manual source: `package/ggNetView-manual/*.Rmd`

## File Structure

Modify existing files:

- `R/app_validation.R`: type detection and validation for new input classes.
- `R/app_registry.R`: richer registry summaries and source metadata.
- `R/app_adapters.R`: keep `safe_call()` and source loading; delegate graph building to the new helper file.
- `R/app_exports.R`: graph/result-aware exports.
- `inst/app/modules/mod_data_hub.R`: upload type selector, validation preview, and example input registration.
- `inst/app/modules/mod_graph_builder.R`: builder mode UI and server routing.
- `inst/app/modules/mod_export_center.R`: expose downloads based on object type.
- `README.md`: update Phase 2 supported workflow after implementation.
- `docs/ggnetview-shiny-next-todos.md`: move completed Phase 2 items into done notes and keep later phases visible.

Create focused files:

- `R/app_graph_builders.R`: mode-specific validation, parameter normalization, and calls to ggNetView graph APIs.
- `R/app_input_examples.R`: small fixture creators used by app examples and tests.
- `tests/testthat/test-app-input-types.R`: unit tests for type detection and validation.
- `tests/testthat/test-app-graph-builders.R`: unit tests for graph builder adapters.
- `tests/testthat/test-app-export-types.R`: unit tests for graph/result export behavior.
- `tests/run_shiny_phase2_workflow_smoke.R`: browser-level smoke for the main Phase 2 workflow.
- `inst/extdata/phase2_example_matrix.csv`: tiny matrix fixture.
- `inst/extdata/phase2_example_matrix_b.csv`: second matrix for double/multi-matrix builders.
- `inst/extdata/phase2_example_edges.csv`: edge-table fixture.
- `inst/extdata/phase2_example_modules.csv`: node-to-module fixture.
- `inst/extdata/phase2_example_adjacency.csv`: adjacency fixture.
- `inst/extdata/phase2_example_tom.csv`: TOM-like fixture.

## Verification Commands

Run these from repository root:

```bash
/usr/local/bin/Rscript -e 'source("R/app_validation.R"); source("R/app_registry.R"); source("R/app_adapters.R"); source("R/app_graph_builders.R"); cat("helpers loaded\n")'
/usr/local/bin/Rscript -e 'testthat::test_file("tests/testthat/test-app-input-types.R")'
/usr/local/bin/Rscript -e 'testthat::test_file("tests/testthat/test-app-graph-builders.R")'
/usr/local/bin/Rscript -e 'testthat::test_file("tests/testthat/test-app-export-types.R")'
/usr/local/bin/Rscript tests/run_shiny_startup_smoke.R
/usr/local/bin/Rscript tests/run_shiny_phase2_workflow_smoke.R
```

Expected final output:

- helper loading command prints `helpers loaded`;
- each `testthat::test_file()` command exits with status 0;
- startup smoke exits with status 0;
- Phase 2 workflow smoke exits with status 0 and writes a screenshot/log artifact if the existing smoke pattern does that.

---

### Task 1: Add Phase 2 Fixture Data

**Files:**

- Create: `R/app_input_examples.R`
- Create: `inst/extdata/phase2_example_matrix.csv`
- Create: `inst/extdata/phase2_example_matrix_b.csv`
- Create: `inst/extdata/phase2_example_edges.csv`
- Create: `inst/extdata/phase2_example_modules.csv`
- Create: `inst/extdata/phase2_example_adjacency.csv`
- Create: `inst/extdata/phase2_example_tom.csv`
- Test: `tests/testthat/test-app-input-types.R`

- [ ] **Step 1: Write failing fixture tests**

Create `tests/testthat/test-app-input-types.R` with this initial content:

```r
testthat::test_that("phase2 example files exist and are readable", {
  paths <- file.path("inst", "extdata", c(
    "phase2_example_matrix.csv",
    "phase2_example_matrix_b.csv",
    "phase2_example_edges.csv",
    "phase2_example_modules.csv",
    "phase2_example_adjacency.csv",
    "phase2_example_tom.csv"
  ))

  testthat::expect_true(all(file.exists(paths)))

  tables <- lapply(paths, utils::read.csv, row.names = 1, check.names = FALSE)
  testthat::expect_equal(nrow(tables[[1]]), 6)
  testthat::expect_equal(ncol(tables[[1]]), 5)
  testthat::expect_true(all(c("source", "target", "weight") %in% names(utils::read.csv(paths[[3]], check.names = FALSE))))
})
```

- [ ] **Step 2: Run the test and verify it fails before fixtures exist**

Run:

```bash
/usr/local/bin/Rscript -e 'testthat::test_file("tests/testthat/test-app-input-types.R")'
```

Expected: FAIL because the Phase 2 fixture files do not exist.

- [ ] **Step 3: Create fixture generator**

Create `R/app_input_examples.R`:

```r
phase2_example_data <- function() {
  matrix_a <- data.frame(
    S1 = c(9, 8, 1, 2, 7, 6),
    S2 = c(8, 9, 2, 1, 6, 7),
    S3 = c(1, 2, 9, 8, 3, 4),
    S4 = c(2, 1, 8, 9, 4, 3),
    S5 = c(7, 6, 3, 4, 9, 8),
    row.names = paste0("OTU", 1:6),
    check.names = FALSE
  )

  matrix_b <- data.frame(
    S1 = c(3, 4, 9, 8, 2, 1),
    S2 = c(4, 3, 8, 9, 1, 2),
    S3 = c(9, 8, 3, 4, 7, 6),
    S4 = c(8, 9, 4, 3, 6, 7),
    S5 = c(2, 1, 7, 6, 8, 9),
    row.names = paste0("Gene", 1:6),
    check.names = FALSE
  )

  edges <- data.frame(
    source = c("OTU1", "OTU1", "OTU2", "OTU3", "OTU4", "OTU5"),
    target = c("OTU2", "OTU5", "OTU6", "OTU4", "OTU6", "OTU6"),
    weight = c(0.82, 0.58, -0.41, 0.77, 0.49, 0.66),
    check.names = FALSE
  )

  modules <- data.frame(
    node = paste0("OTU", 1:6),
    module = c("A", "A", "B", "B", "C", "C"),
    check.names = FALSE
  )

  adjacency <- matrix(0, nrow = 6, ncol = 6, dimnames = list(paste0("OTU", 1:6), paste0("OTU", 1:6)))
  adjacency[cbind(c(1, 1, 2, 3, 4, 5), c(2, 5, 6, 4, 6, 6))] <- c(0.82, 0.58, -0.41, 0.77, 0.49, 0.66)
  adjacency <- adjacency + t(adjacency)
  diag(adjacency) <- 0

  tom <- adjacency
  tom[tom < 0] <- abs(tom[tom < 0])
  diag(tom) <- 1

  list(
    matrix_a = matrix_a,
    matrix_b = matrix_b,
    edges = edges,
    modules = modules,
    adjacency = as.data.frame(adjacency, check.names = FALSE),
    tom = as.data.frame(tom, check.names = FALSE)
  )
}

write_phase2_example_data <- function(dir = file.path("inst", "extdata")) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  data <- phase2_example_data()
  utils::write.csv(data$matrix_a, file.path(dir, "phase2_example_matrix.csv"), quote = FALSE)
  utils::write.csv(data$matrix_b, file.path(dir, "phase2_example_matrix_b.csv"), quote = FALSE)
  utils::write.csv(data$edges, file.path(dir, "phase2_example_edges.csv"), row.names = FALSE, quote = FALSE)
  utils::write.csv(data$modules, file.path(dir, "phase2_example_modules.csv"), row.names = FALSE, quote = FALSE)
  utils::write.csv(data$adjacency, file.path(dir, "phase2_example_adjacency.csv"), quote = FALSE)
  utils::write.csv(data$tom, file.path(dir, "phase2_example_tom.csv"), quote = FALSE)
  invisible(normalizePath(dir, mustWork = FALSE))
}
```

- [ ] **Step 4: Generate fixtures**

Run:

```bash
/usr/local/bin/Rscript -e 'source("R/app_input_examples.R"); write_phase2_example_data()'
```

Expected: six CSV files are written under `inst/extdata/`.

- [ ] **Step 5: Run fixture test**

Run:

```bash
/usr/local/bin/Rscript -e 'testthat::test_file("tests/testthat/test-app-input-types.R")'
```

Expected: PASS.

- [ ] **Step 6: Commit fixtures**

```bash
git add R/app_input_examples.R inst/extdata/phase2_example_*.csv tests/testthat/test-app-input-types.R
git commit -m "test: add phase2 graph builder fixtures"
```

---

### Task 2: Extend Input Type Detection

**Files:**

- Modify: `R/app_validation.R`
- Modify: `inst/app/modules/mod_data_hub.R`
- Test: `tests/testthat/test-app-input-types.R`

- [ ] **Step 1: Add failing tests for new input classes**

Append to `tests/testthat/test-app-input-types.R`:

```r
testthat::test_that("detect_upload_type recognizes phase2 table classes", {
  source("R/app_validation.R")

  matrix_a <- utils::read.csv("inst/extdata/phase2_example_matrix.csv", row.names = 1, check.names = FALSE)
  edges <- utils::read.csv("inst/extdata/phase2_example_edges.csv", check.names = FALSE)
  modules <- utils::read.csv("inst/extdata/phase2_example_modules.csv", check.names = FALSE)
  adjacency <- utils::read.csv("inst/extdata/phase2_example_adjacency.csv", row.names = 1, check.names = FALSE)
  tom <- utils::read.csv("inst/extdata/phase2_example_tom.csv", row.names = 1, check.names = FALSE)

  testthat::expect_equal(detect_upload_type(matrix_a), "matrix")
  testthat::expect_equal(detect_upload_type(edges), "edge_table")
  testthat::expect_equal(detect_upload_type(modules), "module_table")
  testthat::expect_equal(detect_upload_type(adjacency), "adjacency")
  testthat::expect_equal(detect_upload_type(tom), "wgcna_tom")
})
```

- [ ] **Step 2: Run the test and verify it fails**

Run:

```bash
/usr/local/bin/Rscript -e 'testthat::test_file("tests/testthat/test-app-input-types.R")'
```

Expected: FAIL for `module_table` and `wgcna_tom` until detection is expanded.

- [ ] **Step 3: Implement deterministic type detection**

Modify `R/app_validation.R` so `detect_upload_type()` follows this order:

```r
detect_upload_type <- function(data) {
  if (!is.data.frame(data) && !is.matrix(data)) {
    return("unknown")
  }

  names_lower <- tolower(names(as.data.frame(data)))
  has_cols <- function(cols) all(cols %in% names_lower)

  if (has_cols(c("source", "target"))) {
    return("edge_table")
  }

  if (has_cols(c("node", "module")) || has_cols(c("name", "module"))) {
    return("module_table")
  }

  if (has_cols(c("node", "label")) || has_cols(c("name", "class")) || has_cols(c("id", "group"))) {
    return("annotation")
  }

  numeric_data <- suppressWarnings(data.matrix(data))
  if (anyNA(numeric_data)) {
    return("unknown")
  }

  is_square <- nrow(numeric_data) == ncol(numeric_data)
  has_matching_names <- !is.null(rownames(numeric_data)) &&
    !is.null(colnames(numeric_data)) &&
    identical(rownames(numeric_data), colnames(numeric_data))

  if (is_square && has_matching_names) {
    diagonal <- diag(numeric_data)
    if (all(abs(diagonal - 1) < 1e-8)) {
      return("wgcna_tom")
    }
    return("adjacency")
  }

  if (all(vapply(as.data.frame(data), is.numeric, logical(1)))) {
    return("matrix")
  }

  "unknown"
}
```

- [ ] **Step 4: Add UI choices for user override**

Modify `mod_data_hub_ui()` in `inst/app/modules/mod_data_hub.R` to include a type override selector near the upload controls:

```r
shiny::selectInput(
  ns("upload_type"),
  "Object type",
  choices = c(
    "Auto detect" = "auto",
    "Matrix" = "matrix",
    "Adjacency matrix" = "adjacency",
    "Edge table" = "edge_table",
    "Module table" = "module_table",
    "Annotation" = "annotation",
    "WGCNA/TOM matrix" = "wgcna_tom",
    "Sample metadata" = "sample_metadata",
    "Environment matrix" = "env_matrix"
  ),
  selected = "auto"
)
```

In the upload server path, compute:

```r
detected_type <- detect_upload_type(data)
object_type <- if (identical(input$upload_type, "auto")) detected_type else input$upload_type
```

Use `object_type` in the registry item.

- [ ] **Step 5: Run tests**

Run:

```bash
/usr/local/bin/Rscript -e 'testthat::test_file("tests/testthat/test-app-input-types.R")'
```

Expected: PASS.

- [ ] **Step 6: Commit input detection**

```bash
git add R/app_validation.R inst/app/modules/mod_data_hub.R tests/testthat/test-app-input-types.R
git commit -m "feat: expand app input type detection"
```

---

### Task 3: Add Graph Builder Adapter Layer

**Files:**

- Create: `R/app_graph_builders.R`
- Modify: `R/app_adapters.R`
- Test: `tests/testthat/test-app-graph-builders.R`

- [ ] **Step 1: Write failing adapter tests**

Create `tests/testthat/test-app-graph-builders.R`:

```r
source("R/app_validation.R")
source("R/app_adapters.R")
source("R/app_graph_builders.R")

read_fixture <- function(name, row_names = TRUE) {
  path <- file.path("inst", "extdata", name)
  if (row_names) {
    utils::read.csv(path, row.names = 1, check.names = FALSE)
  } else {
    utils::read.csv(path, check.names = FALSE)
  }
}

testthat::test_that("builder modes are discoverable", {
  modes <- graph_builder_modes()
  testthat::expect_true(all(c(
    "matrix",
    "matrix_rmt",
    "edge_table",
    "adjacency",
    "double_matrix",
    "multi_matrix",
    "wgcna_tom",
    "consensus"
  ) %in% names(modes)))
})

testthat::test_that("matrix graph builder returns app_result", {
  mat <- read_fixture("phase2_example_matrix.csv")
  result <- safe_graph_builder(
    mode = "matrix",
    inputs = list(matrix = mat),
    params = list(method = "cor", cor.method = "pearson", r.threshold = 0.2, p.threshold = 1)
  )
  testthat::expect_true(is.list(result))
  testthat::expect_true(isTRUE(result$ok))
  testthat::expect_s3_class(result$value, "igraph")
})

testthat::test_that("edge table graph builder returns app_result", {
  edges <- read_fixture("phase2_example_edges.csv", row_names = FALSE)
  result <- safe_graph_builder(
    mode = "edge_table",
    inputs = list(edge_table = edges),
    params = list()
  )
  testthat::expect_true(isTRUE(result$ok))
  testthat::expect_s3_class(result$value, "igraph")
})
```

- [ ] **Step 2: Run tests and verify failure**

Run:

```bash
/usr/local/bin/Rscript -e 'testthat::test_file("tests/testthat/test-app-graph-builders.R")'
```

Expected: FAIL because `R/app_graph_builders.R` does not exist.

- [ ] **Step 3: Create adapter functions**

Create `R/app_graph_builders.R`:

```r
graph_builder_modes <- function() {
  c(
    "Matrix" = "matrix",
    "Matrix + RMT" = "matrix_rmt",
    "Edge table" = "edge_table",
    "Adjacency matrix" = "adjacency",
    "Double matrix" = "double_matrix",
    "Multi matrix" = "multi_matrix",
    "WGCNA/TOM" = "wgcna_tom",
    "Consensus" = "consensus"
  )
}

normalize_graph_builder_params <- function(mode, params = list()) {
  params <- params %||% list()
  mode <- as.character(mode)

  if (mode %in% c("matrix", "matrix_rmt")) {
    defaults <- list(
      transfrom.method = "none",
      method = "cor",
      cor.method = "pearson",
      proc = "none",
      r.threshold = 0.1,
      p.threshold = 1,
      module.method = "Fast_greedy"
    )
    return(utils::modifyList(defaults, params, keep.null = TRUE))
  }

  if (mode %in% c("adjacency", "edge_table", "double_matrix", "multi_matrix", "wgcna_tom", "consensus")) {
    return(params)
  }

  params
}

required_builder_inputs <- function(mode) {
  switch(mode,
    matrix = c("matrix"),
    matrix_rmt = c("matrix"),
    edge_table = c("edge_table"),
    adjacency = c("adjacency"),
    double_matrix = c("matrix_a", "matrix_b"),
    multi_matrix = c("matrices"),
    wgcna_tom = c("tom"),
    consensus = c("graphs_or_adjacency"),
    character()
  )
}

validate_graph_builder_inputs <- function(mode, inputs) {
  missing <- setdiff(required_builder_inputs(mode), names(inputs))
  if (length(missing) > 0) {
    return(app_failure(sprintf("Missing required builder input: %s", paste(missing, collapse = ", "))))
  }
  app_success(TRUE)
}

safe_graph_builder <- function(mode, inputs, params = list()) {
  mode <- as.character(mode)
  inputs <- inputs %||% list()
  params <- normalize_graph_builder_params(mode, params)

  validation <- validate_graph_builder_inputs(mode, inputs)
  if (!isTRUE(validation$ok)) {
    return(validation)
  }

  fn_name <- switch(mode,
    matrix = "build_graph_from_mat",
    matrix_rmt = "build_graph_from_mat",
    edge_table = "build_graph_from_df",
    adjacency = "build_graph_from_adj_mat",
    double_matrix = "build_graph_from_double_mat",
    multi_matrix = "build_graph_from_multi_mat",
    wgcna_tom = "build_graph_from_wgcna",
    consensus = "build_graph_from_consensus",
    NULL
  )

  if (is.null(fn_name)) {
    return(app_failure(paste("Unsupported graph builder mode:", mode)))
  }

  fn <- resolve_ggnetview_function(fn_name)
  if (is.null(fn)) {
    return(app_failure(paste("Cannot find ggNetView function:", fn_name)))
  }

  call_args <- switch(mode,
    matrix = c(list(inputs$matrix), params),
    matrix_rmt = c(list(inputs$matrix), params),
    edge_table = c(list(inputs$edge_table), params),
    adjacency = c(list(inputs$adjacency), params),
    double_matrix = c(list(inputs$matrix_a, inputs$matrix_b), params),
    multi_matrix = c(list(inputs$matrices), params),
    wgcna_tom = c(list(inputs$tom), params),
    consensus = c(list(inputs$graphs_or_adjacency), params)
  )

  safe_call(
    do.call(fn, call_args),
    paste("Failed to build graph with", fn_name)
  )
}
```

If `%||%` is not already available in this app context, add this helper at the top of `R/app_graph_builders.R`:

```r
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}
```

- [ ] **Step 4: Delegate old safe builder to new adapter**

Modify `safe_build_graph()` in `R/app_adapters.R` to preserve current behavior while using the new adapter:

```r
safe_build_graph <- function(data, builder, params = list()) {
  mode <- switch(builder,
    matrix = "matrix",
    adjacency = "adjacency",
    edge_table = "edge_table",
    builder
  )

  input_name <- switch(mode,
    matrix = "matrix",
    adjacency = "adjacency",
    edge_table = "edge_table",
    "matrix"
  )

  safe_graph_builder(
    mode = mode,
    inputs = stats::setNames(list(data), input_name),
    params = params
  )
}
```

Ensure `inst/app/global.R` or the source-loading path sources `R/app_graph_builders.R` before modules use `safe_build_graph()`.

- [ ] **Step 5: Run adapter tests**

Run:

```bash
/usr/local/bin/Rscript -e 'testthat::test_file("tests/testthat/test-app-graph-builders.R")'
```

Expected: PASS for matrix and edge table adapters.

- [ ] **Step 6: Commit adapter layer**

```bash
git add R/app_graph_builders.R R/app_adapters.R inst/app/global.R tests/testthat/test-app-graph-builders.R
git commit -m "feat: add graph builder adapter layer"
```

---

### Task 4: Implement Matrix and RMT Builder UI

**Files:**

- Modify: `inst/app/modules/mod_graph_builder.R`
- Modify: `R/app_graph_builders.R`
- Test: `tests/testthat/test-app-graph-builders.R`

- [ ] **Step 1: Add failing RMT adapter test**

Append to `tests/testthat/test-app-graph-builders.R`:

```r
testthat::test_that("RMT helper returns a threshold result", {
  mat <- read_fixture("phase2_example_matrix.csv")
  result <- safe_rmt_threshold(
    mat,
    params = list(min_threshold = 0.1, max_threshold = 0.9, step = 0.1, method = "pearson")
  )
  testthat::expect_true(is.list(result))
  testthat::expect_true(isTRUE(result$ok))
  testthat::expect_true(is.data.frame(result$value) || is.list(result$value))
})
```

- [ ] **Step 2: Run test and verify failure**

Run:

```bash
/usr/local/bin/Rscript -e 'testthat::test_file("tests/testthat/test-app-graph-builders.R")'
```

Expected: FAIL because `safe_rmt_threshold()` is not defined.

- [ ] **Step 3: Add RMT adapter**

Add to `R/app_graph_builders.R`:

```r
safe_rmt_threshold <- function(matrix, params = list()) {
  fn <- resolve_ggnetview_function("ggNetView_RMT")
  if (is.null(fn)) {
    return(app_failure("Cannot find ggNetView function: ggNetView_RMT"))
  }

  safe_call(
    do.call(fn, c(list(matrix), params)),
    "Failed to calculate RMT threshold."
  )
}
```

- [ ] **Step 4: Expand matrix UI controls**

In `mod_graph_builder_ui()`, replace the single current matrix parameter block with controls that are shown for matrix modes:

```r
shiny::selectInput(
  ns("method"),
  "Association method",
  choices = c("cor", "Hmisc", "WGCNA", "SPARCC", "SpiecEasi")
),
shiny::selectInput(
  ns("transform_method"),
  "Transform",
  choices = c("none", "log10", "log10p", "sqrt", "relative")
),
shiny::selectInput(ns("cor_method"), "Correlation", choices = c("pearson", "spearman")),
shiny::selectInput(ns("proc"), "P-value adjustment", choices = c("none", "BH", "holm", "bonferroni")),
shiny::numericInput(ns("r_threshold"), "r threshold", value = 0.1, min = 0, max = 1, step = 0.01),
shiny::numericInput(ns("p_threshold"), "p threshold", value = 1, min = 0, max = 1, step = 0.01),
shiny::checkboxInput(ns("use_rmt"), "Use RMT-selected threshold", value = FALSE),
shiny::actionButton(ns("run_rmt"), "Run RMT")
```

Collect params with:

```r
params <- graph_builder_params(
  builder = input$builder,
  method = input$method,
  cor_method = input$cor_method,
  proc = input$proc,
  r_threshold = input$r_threshold,
  p_threshold = input$p_threshold,
  module_method = input$module_method,
  transform_method = input$transform_method
)
```

Update `graph_builder_params()` signature to include `transform_method` and output `transfrom.method`:

```r
graph_builder_params <- function(
  builder,
  method = "cor",
  cor_method = "pearson",
  proc = "none",
  r_threshold = 0.1,
  p_threshold = 1,
  module_method = "Fast_greedy",
  transform_method = "none"
) {
  if (!identical(builder, "matrix") && !identical(builder, "matrix_rmt")) {
    return(list())
  }

  list(
    transfrom.method = transform_method,
    method = method,
    cor.method = cor_method,
    proc = proc,
    r.threshold = r_threshold,
    p.threshold = p_threshold,
    module.method = module_method
  )
}
```

- [ ] **Step 5: Wire RMT action to registry**

In `mod_graph_builder_server()`, add a `reactiveVal()` for the last RMT result. On `input$run_rmt`, call `safe_rmt_threshold()` with the selected matrix. Register a `result` item:

```r
rmt_result <- shiny::reactiveVal(NULL)

shiny::observeEvent(input$run_rmt, {
  shiny::req(input$source_id)
  source <- registry_get(registry, input$source_id)
  shiny::req(source)

  result <- safe_rmt_threshold(
    source$data,
    params = list(method = input$cor_method)
  )

  if (!result$ok) {
    status(result$message)
    shiny::showNotification(result$message, type = "error")
    return()
  }

  rmt_result(result$value)
  registry_add(
    registry,
    name = paste0(source$name, "_rmt"),
    type = "result",
    data = result$value,
    source = source$id,
    params = list(method = input$cor_method)
  )
  status(paste("RMT result registered for:", source$name))
})
```

- [ ] **Step 6: Run tests**

Run:

```bash
/usr/local/bin/Rscript -e 'testthat::test_file("tests/testthat/test-app-graph-builders.R")'
```

Expected: PASS. If `ggNetView_RMT()` returns a plotting object plus table, adapt only the test expectation to assert the actual stable return shape.

- [ ] **Step 7: Commit matrix/RMT builder**

```bash
git add R/app_graph_builders.R inst/app/modules/mod_graph_builder.R tests/testthat/test-app-graph-builders.R
git commit -m "feat: add matrix and RMT builder controls"
```

---

### Task 5: Implement Edge and Adjacency Builders with Module Metadata

**Files:**

- Modify: `R/app_graph_builders.R`
- Modify: `inst/app/modules/mod_graph_builder.R`
- Test: `tests/testthat/test-app-graph-builders.R`

- [ ] **Step 1: Add failing metadata tests**

Append:

```r
testthat::test_that("adjacency builder accepts module metadata", {
  adjacency <- read_fixture("phase2_example_adjacency.csv")
  modules <- read_fixture("phase2_example_modules.csv", row_names = FALSE)
  result <- safe_graph_builder(
    mode = "adjacency",
    inputs = list(adjacency = adjacency, module_table = modules),
    params = list()
  )
  testthat::expect_true(isTRUE(result$ok))
  testthat::expect_s3_class(result$value, "igraph")
})

testthat::test_that("edge builder accepts module metadata", {
  edges <- read_fixture("phase2_example_edges.csv", row_names = FALSE)
  modules <- read_fixture("phase2_example_modules.csv", row_names = FALSE)
  result <- safe_graph_builder(
    mode = "edge_table",
    inputs = list(edge_table = edges, module_table = modules),
    params = list()
  )
  testthat::expect_true(isTRUE(result$ok))
  testthat::expect_s3_class(result$value, "igraph")
})
```

- [ ] **Step 2: Run tests and verify failure**

Run:

```bash
/usr/local/bin/Rscript -e 'testthat::test_file("tests/testthat/test-app-graph-builders.R")'
```

Expected: FAIL until optional module metadata maps to the module-aware functions.

- [ ] **Step 3: Route to module-aware functions**

In `safe_graph_builder()`, choose function names using optional inputs:

```r
fn_name <- switch(mode,
  matrix = "build_graph_from_mat",
  matrix_rmt = "build_graph_from_mat",
  edge_table = if (!is.null(inputs$module_table)) "build_graph_from_module" else "build_graph_from_df",
  adjacency = if (!is.null(inputs$module_table)) "build_graph_from_adj_mat_module" else "build_graph_from_adj_mat",
  double_matrix = if (!is.null(inputs$module_table)) "build_graph_from_double_mat_with_module" else "build_graph_from_double_mat",
  multi_matrix = "build_graph_from_multi_mat",
  wgcna_tom = "build_graph_from_wgcna",
  consensus = "build_graph_from_consensus",
  NULL
)
```

Set call args for module variants:

```r
call_args <- switch(mode,
  edge_table = if (!is.null(inputs$module_table)) {
    c(list(inputs$edge_table, inputs$module_table), params)
  } else {
    c(list(inputs$edge_table), params)
  },
  adjacency = if (!is.null(inputs$module_table)) {
    c(list(inputs$adjacency, inputs$module_table), params)
  } else {
    c(list(inputs$adjacency), params)
  },
  double_matrix = if (!is.null(inputs$module_table)) {
    c(list(inputs$matrix_a, inputs$matrix_b, inputs$module_table), params)
  } else {
    c(list(inputs$matrix_a, inputs$matrix_b), params)
  },
  matrix = c(list(inputs$matrix), params),
  matrix_rmt = c(list(inputs$matrix), params),
  multi_matrix = c(list(inputs$matrices), params),
  wgcna_tom = c(list(inputs$tom), params),
  consensus = c(list(inputs$graphs_or_adjacency), params)
)
```

- [ ] **Step 4: Add module selector UI**

In `mod_graph_builder_server()`, update selectors so builder modes can optionally use module tables:

```r
shiny::observe({
  shiny::updateSelectInput(session, "module_id", choices = c("None" = "", registry_choices(registry, type = "module_table")))
})
```

Add the UI control:

```r
shiny::selectInput(ns("module_id"), "Module table", choices = c("None" = ""))
```

When building inputs:

```r
inputs <- list()
inputs[[input_name]] <- source$data
if (!is.null(input$module_id) && nzchar(input$module_id)) {
  module_item <- registry_get(registry, input$module_id)
  inputs$module_table <- module_item$data
}
```

- [ ] **Step 5: Run tests**

Run:

```bash
/usr/local/bin/Rscript -e 'testthat::test_file("tests/testthat/test-app-graph-builders.R")'
```

Expected: PASS.

- [ ] **Step 6: Commit edge/adjacency metadata support**

```bash
git add R/app_graph_builders.R inst/app/modules/mod_graph_builder.R tests/testthat/test-app-graph-builders.R
git commit -m "feat: support module metadata in graph builders"
```

---

### Task 6: Implement Double, Multi-Matrix, WGCNA, and Consensus Builders

**Files:**

- Modify: `R/app_graph_builders.R`
- Modify: `inst/app/modules/mod_graph_builder.R`
- Test: `tests/testthat/test-app-graph-builders.R`

- [ ] **Step 1: Add failing broad builder tests**

Append:

```r
testthat::test_that("double matrix builder returns graph", {
  mat_a <- read_fixture("phase2_example_matrix.csv")
  mat_b <- read_fixture("phase2_example_matrix_b.csv")
  result <- safe_graph_builder(
    mode = "double_matrix",
    inputs = list(matrix_a = mat_a, matrix_b = mat_b),
    params = list(method = "cor", r.threshold = 0.2, p.threshold = 1)
  )
  testthat::expect_true(isTRUE(result$ok))
  testthat::expect_s3_class(result$value, "igraph")
})

testthat::test_that("multi matrix builder returns graph", {
  mat_a <- read_fixture("phase2_example_matrix.csv")
  mat_b <- read_fixture("phase2_example_matrix_b.csv")
  result <- safe_graph_builder(
    mode = "multi_matrix",
    inputs = list(matrices = list(otu = mat_a, gene = mat_b)),
    params = list(method = "cor", r.threshold = 0.2, p.threshold = 1)
  )
  testthat::expect_true(isTRUE(result$ok))
  testthat::expect_s3_class(result$value, "igraph")
})

testthat::test_that("WGCNA TOM builder returns graph", {
  tom <- read_fixture("phase2_example_tom.csv")
  mat <- read_fixture("phase2_example_matrix.csv")
  result <- safe_graph_builder(
    mode = "wgcna_tom",
    inputs = list(tom = tom, matrix = mat),
    params = list(threshold = 0.2)
  )
  testthat::expect_true(isTRUE(result$ok))
  testthat::expect_s3_class(result$value, "igraph")
})

testthat::test_that("consensus builder accepts adjacency matrices", {
  adjacency <- read_fixture("phase2_example_adjacency.csv")
  result <- safe_graph_builder(
    mode = "consensus",
    inputs = list(graphs_or_adjacency = list(a = adjacency, b = adjacency)),
    params = list(method = "intersection", binarize = TRUE, binarize_threshold = 0.1)
  )
  testthat::expect_true(isTRUE(result$ok))
  testthat::expect_s3_class(result$value, "igraph")
})
```

- [ ] **Step 2: Run tests and record actual API mismatches**

Run:

```bash
/usr/local/bin/Rscript -e 'testthat::test_file("tests/testthat/test-app-graph-builders.R")'
```

Expected: FAIL where call signatures need exact mapping. Use the error trace to adjust adapter call args, not the ggNetView API.

- [ ] **Step 3: Inspect function signatures**

Run:

```bash
/usr/local/bin/Rscript -e 'source("R/app_adapters.R"); f <- c("build_graph_from_double_mat","build_graph_from_multi_mat","build_graph_from_wgcna","build_graph_from_consensus"); for (x in f) { fn <- resolve_ggnetview_function(x); cat("\\n", x, "\\n"); print(args(fn)); }'
```

Expected: printed argument lists for each API.

- [ ] **Step 4: Correct adapter call args**

Update the `double_matrix`, `multi_matrix`, `wgcna_tom`, and `consensus` branches in `safe_graph_builder()` so the first arguments match the printed signatures. Keep this input contract stable for the UI:

```r
inputs <- list(
  matrix_a = data.frame(),
  matrix_b = data.frame(),
  matrices = list(block_a = data.frame(), block_b = data.frame()),
  tom = data.frame(),
  matrix = data.frame(),
  graphs_or_adjacency = list()
)
```

The UI should not pass raw `input$...` values directly into ggNetView functions. It should always construct the `inputs` list above and let the adapter map to the actual API.

- [ ] **Step 5: Add multi-input UI controls**

In `mod_graph_builder_ui()`, add selectors:

```r
shiny::selectInput(ns("source_id_b"), "Second matrix", choices = character()),
shiny::selectizeInput(ns("multi_source_ids"), "Multiple matrices", choices = character(), multiple = TRUE),
shiny::selectizeInput(ns("consensus_source_ids"), "Graphs or adjacency matrices", choices = character(), multiple = TRUE)
```

Update choices:

```r
shiny::observe({
  matrix_choices <- registry_choices_by_type(registry, c("matrix"))
  graph_adj_choices <- registry_choices_by_type(registry, c("graph", "adjacency"))
  shiny::updateSelectInput(session, "source_id_b", choices = matrix_choices)
  shiny::updateSelectizeInput(session, "multi_source_ids", choices = matrix_choices, server = TRUE)
  shiny::updateSelectizeInput(session, "consensus_source_ids", choices = graph_adj_choices, server = TRUE)
})
```

- [ ] **Step 6: Run full graph builder tests**

Run:

```bash
/usr/local/bin/Rscript -e 'testthat::test_file("tests/testthat/test-app-graph-builders.R")'
```

Expected: PASS or SKIP only for optional external dependency modes that are not installed. If a dependency is optional, use `testthat::skip_if_not_installed("packageName")` at the start of that specific test.

- [ ] **Step 7: Commit broad builders**

```bash
git add R/app_graph_builders.R inst/app/modules/mod_graph_builder.R tests/testthat/test-app-graph-builders.R
git commit -m "feat: add broad graph builder modes"
```

---

### Task 7: Make Exports Object-Aware

**Files:**

- Modify: `R/app_exports.R`
- Modify: `inst/app/modules/mod_export_center.R`
- Test: `tests/testthat/test-app-export-types.R`

- [ ] **Step 1: Add failing export tests**

Create `tests/testthat/test-app-export-types.R`:

```r
source("R/app_exports.R")
source("R/app_adapters.R")
source("R/app_graph_builders.R")

testthat::test_that("graph export formats include graph-specific artifacts", {
  formats <- export_formats_for_type("graph")
  testthat::expect_true(all(c("rds", "nodes_csv", "edges_csv", "adjacency_csv", "params_json") %in% formats))
})

testthat::test_that("result export formats include table artifacts", {
  formats <- export_formats_for_type("result")
  testthat::expect_true(all(c("csv", "rds", "params_json") %in% formats))
})

testthat::test_that("plot export formats remain plot-only", {
  graph_formats <- export_formats_for_type("graph")
  testthat::expect_false(any(c("png", "pdf") %in% graph_formats))
  plot_formats <- export_formats_for_type("plot")
  testthat::expect_true(all(c("png", "pdf") %in% plot_formats))
})
```

- [ ] **Step 2: Run tests and verify failure**

Run:

```bash
/usr/local/bin/Rscript -e 'testthat::test_file("tests/testthat/test-app-export-types.R")'
```

Expected: FAIL because `export_formats_for_type()` does not exist.

- [ ] **Step 3: Add export format map**

Add to `R/app_exports.R`:

```r
export_formats_for_type <- function(type) {
  switch(type,
    graph = c("rds", "nodes_csv", "edges_csv", "adjacency_csv", "params_json"),
    plot = c("rds", "png", "pdf", "params_json"),
    result = c("csv", "rds", "params_json"),
    matrix = c("csv", "rds", "params_json"),
    adjacency = c("csv", "rds", "params_json"),
    edge_table = c("csv", "rds", "params_json"),
    module_table = c("csv", "rds", "params_json"),
    annotation = c("csv", "rds", "params_json"),
    c("rds", "params_json")
  )
}
```

Add graph table writers:

```r
write_graph_nodes_csv <- function(graph, path) {
  nodes <- igraph::as_data_frame(graph, what = "vertices")
  utils::write.csv(nodes, path, row.names = FALSE)
}

write_graph_edges_csv <- function(graph, path) {
  edges <- igraph::as_data_frame(graph, what = "edges")
  utils::write.csv(edges, path, row.names = FALSE)
}

write_graph_adjacency_csv <- function(graph, path) {
  adjacency <- as.matrix(igraph::as_adjacency_matrix(graph, attr = "weight", sparse = FALSE))
  utils::write.csv(adjacency, path)
}
```

- [ ] **Step 4: Update Export Center UI**

In `mod_export_center.R`, replace hard-coded plot assumptions with a reactive format list:

```r
selected_formats <- shiny::reactive({
  item <- selected_item()
  if (is.null(item)) {
    return(character())
  }
  export_formats_for_type(item$type)
})
```

Use `selected_formats()` to show download buttons. Keep `png` and `pdf` visible only when the selected item type is `plot`.

- [ ] **Step 5: Run export tests**

Run:

```bash
/usr/local/bin/Rscript -e 'testthat::test_file("tests/testthat/test-app-export-types.R")'
```

Expected: PASS.

- [ ] **Step 6: Commit exports**

```bash
git add R/app_exports.R inst/app/modules/mod_export_center.R tests/testthat/test-app-export-types.R
git commit -m "feat: make exports object aware"
```

---

### Task 8: Add Browser Smoke for Phase 2 Workflow

**Files:**

- Create: `tests/run_shiny_phase2_workflow_smoke.R`
- Modify: `docs/ggnetview-shiny-next-todos.md`

- [ ] **Step 1: Create smoke script**

Create `tests/run_shiny_phase2_workflow_smoke.R` using the existing smoke-script conventions in this repo. The script should:

```r
app_dir <- normalizePath("inst/app", mustWork = TRUE)
port <- as.integer(Sys.getenv("GGNETVIEW_SHINY_SMOKE_PORT", "3878"))
url <- sprintf("http://127.0.0.1:%s", port)

cat("Starting ggNetView Shiny Phase 2 smoke at", url, "\n")

app <- callr::r_bg(
  function(app_dir, port) {
    options(shiny.port = port, shiny.host = "127.0.0.1")
    shiny::runApp(app_dir, launch.browser = FALSE, port = port, host = "127.0.0.1")
  },
  args = list(app_dir, port),
  stdout = "|",
  stderr = "|"
)
on.exit(app$kill(), add = TRUE)

deadline <- Sys.time() + 30
ready <- FALSE
while (Sys.time() < deadline) {
  ok <- tryCatch({
    response <- curl::curl_fetch_memory(url)
    response$status_code < 500
  }, error = function(e) FALSE)
  if (ok) {
    ready <- TRUE
    break
  }
  Sys.sleep(1)
}

if (!ready) {
  stop("Shiny app did not become ready for Phase 2 smoke.")
}

cat("Phase 2 smoke app is reachable\n")
```

If the repo already has a richer Playwright/shinytest2 smoke helper, reuse that helper and extend the script to click:

- Data Hub;
- example matrix load;
- Build Networks;
- Matrix builder;
- Build graph;
- Visual Lab graph selector;
- Export Center selected graph.

- [ ] **Step 2: Run startup smoke**

Run:

```bash
/usr/local/bin/Rscript tests/run_shiny_startup_smoke.R
```

Expected: PASS.

- [ ] **Step 3: Run Phase 2 smoke**

Run:

```bash
/usr/local/bin/Rscript tests/run_shiny_phase2_workflow_smoke.R
```

Expected: PASS.

- [ ] **Step 4: Update handoff notes**

In `docs/ggnetview-shiny-next-todos.md`, add a Phase 2 section with:

```markdown
## Phase 2 Graph Builder

- Typed input registry added for matrix, adjacency, edge table, module table, annotation, TOM-like matrix, sample metadata, and environment matrix.
- Graph Builder now routes through `safe_graph_builder()` and supports matrix, RMT-assisted matrix, edge table, adjacency, double matrix, multi-matrix, WGCNA/TOM, and consensus builder modes where dependencies are available.
- Export Center is object-aware for graph, plot, result, and input objects.
- Verification commands:
  - `/usr/local/bin/Rscript -e 'testthat::test_file("tests/testthat/test-app-input-types.R")'`
  - `/usr/local/bin/Rscript -e 'testthat::test_file("tests/testthat/test-app-graph-builders.R")'`
  - `/usr/local/bin/Rscript -e 'testthat::test_file("tests/testthat/test-app-export-types.R")'`
  - `/usr/local/bin/Rscript tests/run_shiny_startup_smoke.R`
  - `/usr/local/bin/Rscript tests/run_shiny_phase2_workflow_smoke.R`
```

- [ ] **Step 5: Commit smoke and docs**

```bash
git add tests/run_shiny_phase2_workflow_smoke.R docs/ggnetview-shiny-next-todos.md
git commit -m "test: add phase2 Shiny workflow smoke"
```

---

### Task 9: Final Integration Verification

**Files:**

- Modify: `README.md`
- Modify: `docs/ggnetview-shiny-next-todos.md`

- [ ] **Step 1: Run complete helper and unit-test verification**

Run:

```bash
/usr/local/bin/Rscript -e 'source("R/app_validation.R"); source("R/app_registry.R"); source("R/app_adapters.R"); source("R/app_graph_builders.R"); cat("helpers loaded\n")'
/usr/local/bin/Rscript -e 'testthat::test_file("tests/testthat/test-app-input-types.R")'
/usr/local/bin/Rscript -e 'testthat::test_file("tests/testthat/test-app-graph-builders.R")'
/usr/local/bin/Rscript -e 'testthat::test_file("tests/testthat/test-app-export-types.R")'
```

Expected: all commands exit with status 0.

- [ ] **Step 2: Run Shiny verification**

Run:

```bash
/usr/local/bin/Rscript tests/run_shiny_startup_smoke.R
/usr/local/bin/Rscript tests/run_shiny_phase2_workflow_smoke.R
```

Expected: both commands exit with status 0.

- [ ] **Step 3: Update README current status**

Add a concise Phase 2 status block to `README.md`:

```markdown
## Current Shiny Status

The Shiny app supports the core ggNetView loop and Phase 2 graph-construction inputs:

- load or upload typed data objects;
- build graphs from matrix, RMT-assisted matrix, edge table, adjacency, double matrix, multi-matrix, WGCNA/TOM, and consensus inputs where dependencies are available;
- inspect graph tables;
- draw ggNetView plots;
- compute topology results;
- export graph, result, plot, and parameter artifacts.

Use `/usr/local/bin/Rscript tests/run_shiny_startup_smoke.R` and `/usr/local/bin/Rscript tests/run_shiny_phase2_workflow_smoke.R` for local smoke checks.
```

- [ ] **Step 4: Confirm clean diff scope**

Run:

```bash
git status --short
git diff --stat
```

Expected: only Phase 2 implementation, tests, fixtures, and docs are changed.

- [ ] **Step 5: Commit final docs**

```bash
git add README.md docs/ggnetview-shiny-next-todos.md
git commit -m "docs: update phase2 graph builder status"
```

---

## Self-Review Checklist

- Spec coverage: Tasks 1-3 cover typed inputs and adapters; Tasks 4-6 cover matrix/RMT/edge/adjacency/double/multi/WGCNA/consensus builders; Task 7 covers object-aware export; Task 8 covers browser smoke; Task 9 covers final documentation.
- Scope boundary: No task implements full layout gallery, full environment analysis, full multi-network comparison, project save/restore, or gallery reproduction.
- Verification: Every task includes an exact `/usr/local/bin/Rscript` command where behavior changes.
- Commit rhythm: Each task ends with a small commit.
- Compatibility: Existing `safe_build_graph()` remains as a wrapper so the first working Shiny loop is preserved during migration.

## Execution Options

Plan complete and saved to `docs/superpowers/plans/2026-05-26-ggnetview-shiny-phase2-graph-builder.md`.

1. Subagent-Driven: dispatch a fresh subagent per task, review between tasks, fast iteration.
2. Inline Execution: execute tasks in this session using executing-plans, with checkpoints after each task.
