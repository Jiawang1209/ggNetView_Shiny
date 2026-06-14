# ggNetView Shiny 界面工程化改造 Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** 把 ggNetView Shiny 应用的界面从"能用"提升到工程级 —— 品牌主题 + 暗色模式、引导式着陆页、value_box 指标卡、统一加载态(第一梯队),以及空状态、输入校验、DT 工程化、统一错误 toast(第二梯队),不改任何 adapter 后端逻辑。

**Architecture:** 新增两个纯函数文件 `R/app_theme.R`(主题 + 调色板)与 `R/app_ui_helpers.R`(可复用 UI 件),在 `inst/app/global.R` 注册;`ui.R`/`server.R` 接入主题与新着陆页模块;各 `mod_*.R` 最小化引用 helper。测试沿用仓库现有两套机制:`tests/testthat/*.R` 的 grep 结构断言 + 纯函数单元测试,以及 `tests/run_shiny_*.R` 的 shinytest2 浏览器 smoke。

**Tech Stack:** R, Shiny, bslib (v5), bsicons, shinycssloaders, DT, testthat, shinytest2。

参考规格:`docs/superpowers/specs/2026-06-14-ggnetview-shiny-ui-engineering-design.md`

品牌色令牌(全程使用):主色 `#AE017E`、深梅 `#7D0159`、淡粉表面 `#FFF0F3`、画布 `#FAFAFA`;模块调色板 `#F08D8D #F2D24D #F2A65A #9FD18B #6FC4C0 #7FB3E0 #C9A0DC #E89BC4`。

---

## File Structure

**新建:**
- `R/app_theme.R` — `app_bs_theme()`(自定义 bs_theme)、`app_module_palette()`(8 色命名向量)。
- `R/app_ui_helpers.R` — `ui_empty_state()`、`ggnv_value_box()`、`dt_table()`、`notify()`。
- `inst/app/modules/mod_landing.R` — 引导式着陆页模块(UI + server)。
- `inst/app/www/logo.png` — logo 小尺寸副本(从 `man/figures/logo.png` 缩放)。
- `inst/app/www/favicon.png` — favicon。
- `tests/testthat/test-app-theme.R`、`tests/testthat/test-app-ui-helpers.R` — helper 单元测试。
- `tests/run_shiny_ui_polish_smoke.R` — shinytest2 浏览器 smoke。

**修改:**
- `inst/app/global.R` — 在 `app_helper_files` 注册新文件;在加载向量加新函数名;在 `module_files` 加 `mod_landing.R`。
- `inst/app/ui.R` — 接入 `app_bs_theme()`、logo/favicon、`input_dark_mode()`;Introduction 改用 `mod_landing_ui`。
- `inst/app/server.R` — 加 `mod_landing_server` 并接 nav 跳转。
- `inst/app/www/styles.css` — 加品牌 CSS 变量与着陆页/指标卡样式。
- `DESCRIPTION` — Imports 追加 `shinycssloaders`、`bsicons`。
- `inst/app/modules/mod_graph_explorer.R`、`mod_perturbation.R`、`mod_topology_results.R`、`mod_network_compare.R` — value_box / spinner / 空状态 / 校验 / dt_table / notify。
- `tests/testthat/test-shiny-files.R` — 新结构断言。

---

## Task 1: 新增依赖

**Files:**
- Modify: `DESCRIPTION`

- [ ] **Step 1: 在 Imports 追加两个包**

打开 `DESCRIPTION`,在 `Imports:` 区块的字母序位置插入 `bsicons,`(在 `boot` 后、`bslib` 前)和 `shinycssloaders,`(在 `Rcpp`/`shiny` 相关项附近,保持字母序)。最终这两行应存在于 Imports 列表:

```
Imports:
    boot,
    bsicons,
    bslib,
    ...
    shinycssloaders,
    ...
```

- [ ] **Step 2: 验证两个包已安装、可加载**

Run: `/usr/local/bin/Rscript -e 'library(bsicons); library(shinycssloaders); cat("OK\n")'`
Expected: 打印 `OK`(若报 "there is no package called …",先 `install.packages(c("bsicons","shinycssloaders"))` 再重试)。

- [ ] **Step 3: Commit**

```bash
git add DESCRIPTION
git commit -m "build: add bsicons and shinycssloaders deps"
```

---

## Task 2: 主题与调色板 helper (`R/app_theme.R`)

**Files:**
- Create: `R/app_theme.R`
- Create: `tests/testthat/test-app-theme.R`
- Modify: `inst/app/global.R`

- [ ] **Step 1: 写失败测试**

Create `tests/testthat/test-app-theme.R`:

```r
test_that("app_module_palette returns 8 named hex colors", {
  source(test_path("../../R/app_theme.R"))
  pal <- app_module_palette()
  expect_length(pal, 8)
  expect_true(!is.null(names(pal)))
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", pal)))
  expect_true(all(c("red", "teal", "purple") %in% names(pal)))
})

test_that("app_bs_theme returns a bs_theme using the brand magenta", {
  source(test_path("../../R/app_theme.R"))
  theme <- app_bs_theme()
  expect_s3_class(theme, "bs_theme")
  vars <- bslib::bs_get_variables(theme, "primary")
  expect_equal(tolower(unname(vars[["primary"]])), "#ae017e")
})
```

- [ ] **Step 2: 跑测试确认失败**

Run: `/usr/local/bin/Rscript -e 'library(testthat); test_file("tests/testthat/test-app-theme.R")'`
Expected: FAIL，错误类似 "could not find function \"app_module_palette\"" / 文件不存在。

- [ ] **Step 3: 实现 `R/app_theme.R`**

Create `R/app_theme.R`:

```r
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
```

- [ ] **Step 4: 跑测试确认通过**

Run: `/usr/local/bin/Rscript -e 'library(testthat); test_file("tests/testthat/test-app-theme.R")'`
Expected: PASS（若 `bs_get_variables` 行为异常，确认 bslib 版本 ≥ 0.5；该函数返回命名向量）。

- [ ] **Step 5: 在 global.R 注册新文件与函数**

Modify `inst/app/global.R`:在 `app_helper_files` 的 `c(` 列表最前面加 `"app_theme.R",`(在 `"app_validation.R"` 之前)。在随后 `invisible(lapply(c(...)))` 的函数名向量第一行加 `"app_bs_theme", "app_module_palette",`。

- [ ] **Step 6: Commit**

```bash
git add R/app_theme.R tests/testthat/test-app-theme.R inst/app/global.R
git commit -m "feat: add brand bslib theme and module palette helpers"
```

---

## Task 3: 可复用 UI helper (`R/app_ui_helpers.R`)

**Files:**
- Create: `R/app_ui_helpers.R`
- Create: `tests/testthat/test-app-ui-helpers.R`
- Modify: `inst/app/global.R`

- [ ] **Step 1: 写失败测试**

Create `tests/testthat/test-app-ui-helpers.R`:

```r
test_that("ui_empty_state renders a tagged empty-state card", {
  source(test_path("../../R/app_ui_helpers.R"))
  tag <- ui_empty_state(icon = "inbox", title = "No data", hint = "Load something first")
  html <- as.character(tag)
  expect_match(html, "ggnv-empty-state", fixed = TRUE)
  expect_match(html, "No data", fixed = TRUE)
  expect_match(html, "Load something first", fixed = TRUE)
})

test_that("ggnv_value_box returns a bslib value_box", {
  source(test_path("../../R/app_ui_helpers.R"))
  vb <- ggnv_value_box("Schneider R", "0.62", icon = "shield-check")
  html <- as.character(vb)
  expect_match(html, "Schneider R", fixed = TRUE)
  expect_match(html, "0.62", fixed = TRUE)
})

test_that("dt_table builds a datatable with export buttons", {
  source(test_path("../../R/app_ui_helpers.R"))
  dt <- dt_table(data.frame(a = 1:3, b = c(1.111, 2.222, 3.333)))
  expect_s3_class(dt, "datatables")
  # Buttons extension requested with copy/csv/excel
  expect_true(!is.null(dt$x$options$buttons))
  expect_true(all(c("copy", "csv", "excel") %in% unlist(dt$x$options$buttons)))
})
```

- [ ] **Step 2: 跑测试确认失败**

Run: `/usr/local/bin/Rscript -e 'library(testthat); test_file("tests/testthat/test-app-ui-helpers.R")'`
Expected: FAIL，"could not find function \"ui_empty_state\""。

- [ ] **Step 3: 实现 `R/app_ui_helpers.R`**

Create `R/app_ui_helpers.R`:

```r
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
```

- [ ] **Step 4: 跑测试确认通过**

Run: `/usr/local/bin/Rscript -e 'library(testthat); test_file("tests/testthat/test-app-ui-helpers.R")'`
Expected: PASS。

- [ ] **Step 5: 在 global.R 注册**

Modify `inst/app/global.R`:在 `app_helper_files` 列表加 `"app_ui_helpers.R",`(紧跟 `"app_theme.R",` 之后)。在函数名向量加 `"ui_empty_state", "ggnv_value_box", "dt_table", "notify",`。

- [ ] **Step 6: Commit**

```bash
git add R/app_ui_helpers.R tests/testthat/test-app-ui-helpers.R inst/app/global.R
git commit -m "feat: add reusable empty-state, value-box, dt_table, notify helpers"
```

---

## Task 4: 主题 + logo/favicon + 暗色切换接入 `ui.R`

**Files:**
- Create: `inst/app/www/logo.png`, `inst/app/www/favicon.png`
- Modify: `inst/app/ui.R`
- Modify: `tests/testthat/test-shiny-files.R`

- [ ] **Step 1: 生成 logo/favicon 小尺寸副本**

Run:
```bash
/usr/local/bin/Rscript -e 'img <- png::readPNG("man/figures/logo.png"); png::writePNG(img, "inst/app/www/logo.png"); png::writePNG(img, "inst/app/www/favicon.png")'
```
Expected: 两个文件生成（体积大无妨，浏览器会缩放；如需精确缩放可用 `magick`，但非必须）。

- [ ] **Step 2: 写失败的结构断言**

Modify `tests/testthat/test-shiny-files.R`，在文件末尾追加:

```r
test_that("Shiny UI applies the brand theme, logo, and dark mode toggle", {
  ui_text <- paste(readLines(test_path("../../inst/app/ui.R"), warn = FALSE), collapse = "\n")
  expect_match(ui_text, "app_bs_theme()", fixed = TRUE)
  expect_match(ui_text, "input_dark_mode", fixed = TRUE)
  expect_match(ui_text, "logo.png", fixed = TRUE)
  expect_true(file.exists(test_path("../../inst/app/www/logo.png")))
  expect_true(file.exists(test_path("../../inst/app/www/favicon.png")))
})
```

- [ ] **Step 3: 跑测试确认失败**

Run: `/usr/local/bin/Rscript -e 'library(testthat); test_file("tests/testthat/test-shiny-files.R")'`
Expected: 新增的 test 失败（缺 `app_bs_theme()` / `input_dark_mode`）。

- [ ] **Step 4: 修改 `ui.R`**

Modify `inst/app/ui.R` 顶部 `page_navbar(...)` 的前几个参数:

把
```r
  title = "ggNetView",
  id = "main_nav",
  theme = bslib::bs_theme(version = 5, bootswatch = "flatly"),
  header = shiny::tagList(
    shiny::tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    app_task_feedback_script()
  ),
```
改为
```r
  title = shiny::tags$span(
    class = "ggnv-brand",
    shiny::tags$img(src = "logo.png", class = "ggnv-brand-logo", alt = "ggNetView"),
    "ggNetView"
  ),
  id = "main_nav",
  theme = app_bs_theme(),
  header = shiny::tagList(
    shiny::tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    shiny::tags$link(rel = "icon", type = "image/png", href = "favicon.png"),
    app_task_feedback_script()
  ),
```
并在 `page_navbar(...)` 末尾、`bslib::nav_panel("Export", ...)` 之后,加入暗色切换(放到 navbar 右侧):
```r
  ,
  bslib::nav_spacer(),
  bslib::nav_item(bslib::input_dark_mode(id = "color_mode", mode = "light"))
```

- [ ] **Step 5: 跑测试确认通过**

Run: `/usr/local/bin/Rscript -e 'library(testthat); test_file("tests/testthat/test-shiny-files.R")'`
Expected: 全部 PASS。

- [ ] **Step 6: 启动应用确认无错**

Run: `/usr/local/bin/Rscript tests/run_shiny_app_startup.R`
Expected: 启动成功，无 error。

- [ ] **Step 7: Commit**

```bash
git add inst/app/ui.R inst/app/www/logo.png inst/app/www/favicon.png tests/testthat/test-shiny-files.R
git commit -m "feat: apply brand theme, logo, favicon, and dark mode toggle"
```

---

## Task 5: 引导式着陆页模块 `mod_landing`

**Files:**
- Create: `inst/app/modules/mod_landing.R`
- Modify: `inst/app/ui.R`, `inst/app/server.R`, `inst/app/global.R`
- Modify: `inst/app/www/styles.css`
- Modify: `tests/testthat/test-shiny-files.R`

- [ ] **Step 1: 写失败的结构断言**

Modify `tests/testthat/test-shiny-files.R`，追加:

```r
test_that("Introduction is an onboarding landing module with example CTA and README accordion", {
  expect_true(file.exists(test_path("../../inst/app/modules/mod_landing.R")))
  ui_text <- paste(readLines(test_path("../../inst/app/ui.R"), warn = FALSE), collapse = "\n")
  server_text <- paste(readLines(test_path("../../inst/app/server.R"), warn = FALSE), collapse = "\n")
  global_text <- paste(readLines(test_path("../../inst/app/global.R"), warn = FALSE), collapse = "\n")
  mod_text <- paste(readLines(test_path("../../inst/app/modules/mod_landing.R"), warn = FALSE), collapse = "\n")

  expect_match(ui_text, "mod_landing_ui(\"landing\")", fixed = TRUE)
  expect_match(server_text, "mod_landing_server(\"landing\", registry)", fixed = TRUE)
  expect_match(server_text, "nav_select", fixed = TRUE)
  expect_match(global_text, "mod_landing.R", fixed = TRUE)
  expect_match(mod_text, "start_example", fixed = TRUE)
  expect_match(mod_text, "register_gallery_examples", fixed = TRUE)
  expect_match(mod_text, "includeMarkdown", fixed = TRUE)   # README preserved in accordion
  expect_match(mod_text, "Quick start", fixed = TRUE)
})
```

- [ ] **Step 2: 跑测试确认失败**

Run: `/usr/local/bin/Rscript -e 'library(testthat); test_file("tests/testthat/test-shiny-files.R")'`
Expected: 新 test 失败。

- [ ] **Step 3: 实现 `inst/app/modules/mod_landing.R`**

Create `inst/app/modules/mod_landing.R`:

```r
mod_landing_ui <- function(id) {
  ns <- shiny::NS(id)
  app_root <- getOption("ggnetview.app_root", getwd())
  readme_path <- file.path(app_root, "README.md")

  step_card <- function(n, title, body) {
    shiny::div(
      class = "ggnv-step-card",
      shiny::span(class = "ggnv-step-num", n),
      shiny::div(
        shiny::span(class = "ggnv-step-title", title),
        shiny::span(class = "ggnv-step-body", body)
      )
    )
  }

  bslib::card(
    class = "ggnv-landing",
    shiny::div(
      class = "ggnv-hero",
      shiny::img(src = "logo.png", class = "ggnv-hero-logo", alt = "ggNetView"),
      shiny::h1("Welcome to ggNetView", class = "ggnv-hero-title"),
      shiny::p(
        class = "ggnv-hero-tagline",
        "Build, analyze & visualize association networks — reproducibly."
      ),
      shiny::actionButton(
        ns("start_example"),
        label = shiny::tagList(bsicons::bs_icon("play-fill"), "Load example data & go to Builder"),
        class = "btn btn-primary btn-lg ggnv-hero-cta"
      )
    ),
    shiny::div(
      class = "ggnv-quickstart",
      shiny::div(class = "ggnv-quickstart-label", "Quick start · 3 steps"),
      step_card("1", "Load data", "Upload a matrix / edge table, or pick a bundled example."),
      step_card("2", "Build graph", "Correlation, RMT-assisted, or WGCNA / TOM."),
      step_card("3", "Visualize & analyze", "Layouts, topology, Zi-Pi, perturbation.")
    ),
    bslib::accordion(
      open = FALSE,
      bslib::accordion_panel(
        "Learn more about ggNetView",
        if (file.exists(readme_path)) {
          shiny::div(class = "ggnv-introduction", shiny::includeMarkdown(readme_path))
        } else {
          shiny::p("README not found.")
        }
      )
    )
  )
}

mod_landing_server <- function(id, registry) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::reactive(input$start_example)
  })
}
```

- [ ] **Step 4: 接入 `ui.R`**

Modify `inst/app/ui.R`:把 Introduction 面板
```r
  bslib::nav_panel(
    "Introduction",
    bslib::card(
      class = "ggnv-introduction",
      shiny::includeMarkdown(file.path(app_root, "README.md"))
    )
  ),
```
改为
```r
  bslib::nav_panel("Introduction", mod_landing_ui("landing")),
```

- [ ] **Step 5: 接入 `server.R`**

Modify `inst/app/server.R`:在 `registry <- registry_new()` 之后、其他模块之前加:
```r
  landing_start <- mod_landing_server("landing", registry)
  shiny::observeEvent(landing_start(), {
    register_gallery_examples(registry)
    notify("Example data loaded — opening Graph Builder.", type = "message")
    bslib::nav_select("main_nav", "Graph Builder")
  }, ignoreInit = TRUE)
```

- [ ] **Step 6: 注册到 `global.R`**

Modify `inst/app/global.R`:在 `module_files` 的 `c(` 列表最前面加 `"mod_landing.R",`(在 `"mod_data_hub.R"` 之前)。

- [ ] **Step 7: 加着陆页样式**

Modify `inst/app/www/styles.css`，追加:
```css
.ggnv-brand-logo { height: 26px; width: 26px; margin-right: 8px; vertical-align: middle; }
.ggnv-landing { max-width: 920px; margin: 1rem auto; }
.ggnv-hero { text-align: center; padding: 1.5rem 1rem 0.5rem; }
.ggnv-hero-logo { width: 72px; height: 72px; }
.ggnv-hero-title { font-weight: 800; margin-top: 0.5rem; }
.ggnv-hero-tagline { color: var(--bs-secondary-color, #6b7280); }
.ggnv-hero-cta { margin-top: 0.75rem; }
.ggnv-quickstart { margin: 1.5rem auto; max-width: 640px; }
.ggnv-quickstart-label { text-transform: uppercase; letter-spacing: .5px; font-size: .8rem; font-weight: 700; color: #AE017E; margin-bottom: .5rem; }
.ggnv-step-card { display: flex; gap: .75rem; align-items: center; background: #fff; border: 1px solid #eee; border-radius: 10px; padding: .7rem .9rem; margin-bottom: .5rem; }
.ggnv-step-num { width: 28px; height: 28px; flex: 0 0 28px; border-radius: 50%; background: #AE017E; color: #fff; font-weight: 700; display: flex; align-items: center; justify-content: center; }
.ggnv-step-title { font-weight: 700; display: block; }
.ggnv-step-body { color: #6b7280; font-size: .9rem; }
.ggnv-empty-state { text-align: center; padding: 2.5rem 1rem; color: #6b7280; }
.ggnv-empty-state-icon { font-size: 2rem; color: #AE017E; }
.ggnv-empty-state-title { font-weight: 700; margin-top: .5rem; color: #374151; }
.ggnv-empty-state-hint { font-size: .9rem; margin-top: .25rem; }
.ggnv-empty-state-action { margin-top: .75rem; }
.ggnv-value-box .value-box-showcase { color: #AE017E; }
```

- [ ] **Step 8: 跑结构测试确认通过 + 启动**

Run: `/usr/local/bin/Rscript -e 'library(testthat); test_file("tests/testthat/test-shiny-files.R")'`
Expected: PASS。
Run: `/usr/local/bin/Rscript tests/run_shiny_app_startup.R`
Expected: 启动成功。

- [ ] **Step 9: Commit**

```bash
git add inst/app/modules/mod_landing.R inst/app/ui.R inst/app/server.R inst/app/global.R inst/app/www/styles.css tests/testthat/test-shiny-files.R
git commit -m "feat: add onboarding landing page with example CTA"
```

---

## Task 6: value_box 指标卡

为 Graph Explorer、Perturbation、Topology、Network Compare 增加指标卡。下面对每个模块给出具体改法。所有 `output$*_metrics` 用 `bslib::layout_columns` 包多个 `ggnv_value_box(...)`。

**Files:**
- Modify: `inst/app/modules/mod_graph_explorer.R`, `mod_perturbation.R`, `mod_topology_results.R`, `mod_network_compare.R`
- Modify: `tests/testthat/test-shiny-files.R`

- [ ] **Step 1: 写失败的结构断言**

Modify `tests/testthat/test-shiny-files.R`，追加:
```r
test_that("key panels expose value_box metric cards", {
  for (f in c("mod_graph_explorer.R", "mod_perturbation.R",
              "mod_topology_results.R", "mod_network_compare.R")) {
    txt <- paste(readLines(test_path(file.path("../../inst/app/modules", f)), warn = FALSE), collapse = "\n")
    expect_match(txt, "ggnv_value_box", fixed = TRUE)
  }
})
```

- [ ] **Step 2: 跑测试确认失败**

Run: `/usr/local/bin/Rscript -e 'library(testthat); test_file("tests/testthat/test-shiny-files.R")'`
Expected: 新 test 失败。

- [ ] **Step 3a: Graph Explorer 指标卡**

Modify `inst/app/modules/mod_graph_explorer.R`:在 UI 的 `bslib::card_header("Summary")` 卡片内、summary 文本之前,加一个 metrics 输出占位:
```r
        shiny::uiOutput(ns("metrics")),
```
在 server 部分（紧邻 `output$status` 渲染附近）加:
```r
    output$metrics <- shiny::renderUI({
      g <- selected_graph()                      # 复用本模块已有的所选图 reactive
      if (is.null(g)) return(NULL)
      ig <- coerce_tbl_graph(g)
      n_nodes <- igraph::gorder(ig)
      n_edges <- igraph::gsize(ig)
      n_mod <- tryCatch(length(unique(igraph::V(ig)$Modularity)), error = function(e) NA_integer_)
      dens <- round(igraph::edge_density(ig), 3)
      bslib::layout_columns(
        col_widths = c(3, 3, 3, 3),
        ggnv_value_box("Nodes", n_nodes, icon = "diagram-3"),
        ggnv_value_box("Edges", n_edges, icon = "share"),
        ggnv_value_box("Modules", n_mod, icon = "grid-3x3-gap"),
        ggnv_value_box("Density", dens, icon = "bounding-box")
      )
    })
```
注:若本模块所选图 reactive 名不是 `selected_graph()`，用文件内实际名替换(grep `reactive` 找到代表"当前所选图"的那个)。

- [ ] **Step 3b: Perturbation 指标卡(Schneider R)**

Modify `inst/app/modules/mod_perturbation.R`:在右侧结果区 structural attack 卡片内、曲线图之前加:
```r
        shiny::uiOutput(ns("attack_metrics")),
```
在 server 中,structural attack 结果可用后渲染(用本模块保存攻击结果的 reactive，假设名为 `attack_result()`,返回含 `schneider_R`):
```r
    output$attack_metrics <- shiny::renderUI({
      res <- attack_result()
      if (is.null(res) || is.null(res$schneider_R)) return(NULL)
      bslib::layout_columns(
        col_widths = c(6, 6),
        ggnv_value_box("Schneider R", round(res$schneider_R, 3), icon = "shield-check"),
        ggnv_value_box("Strategy", res$strategy %||% "—", icon = "bullseye")
      )
    })
```
若文件无 `%||%`，在文件顶部加 `` `%||%` <- function(a, b) if (is.null(a)) b else a ``。结果 reactive 实名以文件内为准。

- [ ] **Step 3c: Topology 指标卡**

Modify `inst/app/modules/mod_topology_results.R`:在结果区顶部加 `shiny::uiOutput(ns("topology_metrics"))`;server 中在 topology 表可用后渲染关键标量(节点数/边数/平均度/模块数 —— 字段名以 `safe_topology` 返回表为准):
```r
    output$topology_metrics <- shiny::renderUI({
      tb <- topology_table()
      if (is.null(tb) || !nrow(tb)) return(NULL)
      pick <- function(nm) { v <- tb[[nm]]; if (is.null(v)) "—" else round(v[[1]], 3) }
      bslib::layout_columns(
        col_widths = c(3, 3, 3, 3),
        ggnv_value_box("Nodes", pick("Nodes_number"), icon = "diagram-3"),
        ggnv_value_box("Edges", pick("Edges_number"), icon = "share"),
        ggnv_value_box("Avg degree", pick("Average_degree"), icon = "graph-up"),
        ggnv_value_box("Modules", pick("Module_number"), icon = "grid-3x3-gap")
      )
    })
```
注:`topology_table()` 与字段名以模块实际为准;若某字段不存在 `pick()` 会回退 `—`，安全。

- [ ] **Step 3d: Network Compare 指标卡**

Modify `inst/app/modules/mod_network_compare.R`:在结果区加 `shiny::uiOutput(ns("compare_metrics"))`;server 中在比较结果可用后渲染网络数量等(以模块已有 reactive 为准):
```r
    output$compare_metrics <- shiny::renderUI({
      res <- compare_result()
      if (is.null(res)) return(NULL)
      n_net <- tryCatch(length(res$networks), error = function(e) NA_integer_)
      bslib::layout_columns(
        col_widths = c(6, 6),
        ggnv_value_box("Networks", n_net, icon = "diagram-2"),
        ggnv_value_box("View", "comparison", icon = "bar-chart")
      )
    })
```
注:`compare_result()` 与字段以模块实际为准;若该模块结果结构不含 networks 计数，改用一个稳妥的标量(如所选图数量)，关键是出现 `ggnv_value_box` 且渲染不报错。

- [ ] **Step 4: 跑测试 + 启动确认**

Run: `/usr/local/bin/Rscript -e 'library(testthat); test_file("tests/testthat/test-shiny-files.R")'`
Expected: PASS。
Run: `/usr/local/bin/Rscript tests/run_shiny_app_startup.R`
Expected: 启动成功。

- [ ] **Step 5: Commit**

```bash
git add inst/app/modules/mod_graph_explorer.R inst/app/modules/mod_perturbation.R inst/app/modules/mod_topology_results.R inst/app/modules/mod_network_compare.R tests/testthat/test-shiny-files.R
git commit -m "feat: add value_box metric cards to key panels"
```

---

## Task 7: 统一加载态 (spinners)

把长任务输出用 `shinycssloaders::withSpinner()` 包裹(洋红色)。集中处理 Visual Lab 绘图、Topology 表、Perturbation 曲线、Compare 绘图。

**Files:**
- Modify: `inst/app/modules/mod_visual_lab.R`, `mod_topology_results.R`, `mod_perturbation.R`, `mod_network_compare.R`
- Modify: `tests/testthat/test-shiny-files.R`

- [ ] **Step 1: 写失败的结构断言**

追加到 `tests/testthat/test-shiny-files.R`:
```r
test_that("long-running outputs are wrapped in spinners", {
  for (f in c("mod_visual_lab.R", "mod_topology_results.R",
              "mod_perturbation.R", "mod_network_compare.R")) {
    txt <- paste(readLines(test_path(file.path("../../inst/app/modules", f)), warn = FALSE), collapse = "\n")
    expect_match(txt, "withSpinner", fixed = TRUE)
  }
})
```

- [ ] **Step 2: 跑测试确认失败**

Run: `/usr/local/bin/Rscript -e 'library(testthat); test_file("tests/testthat/test-shiny-files.R")'`
Expected: 新 test 失败。

- [ ] **Step 3: 逐模块包裹主输出**

对每个模块,找到主结果输出(plot/DT)的 UI 定义,用 spinner 包裹。规则:`X <- shinycssloaders::withSpinner(X, color = "#AE017E", type = 6)`。

- `mod_visual_lab.R`:绘图输出(grep `plotOutput` 或 `imageOutput`,代表名如 `ns("plot")`),改为
  ```r
  shinycssloaders::withSpinner(shiny::plotOutput(ns("plot"), height = "600px"), color = "#AE017E", type = 6)
  ```
  (保留原 `plotOutput` 的全部参数,只在外面套一层。)
- `mod_topology_results.R`:主表 `DT::DTOutput(ns("topology"))` → `shinycssloaders::withSpinner(DT::DTOutput(ns("topology")), color = "#AE017E", type = 6)`。
- `mod_perturbation.R`:曲线图输出 → 同样套 `withSpinner(..., color = "#AE017E", type = 6)`。
- `mod_network_compare.R`:比较绘图输出 → 同样套。

注:每个模块只需让 `withSpinner` 至少出现一次于主输出;输出的实际 id 以文件 grep 为准。

- [ ] **Step 4: 跑测试 + 启动确认**

Run: `/usr/local/bin/Rscript -e 'library(testthat); test_file("tests/testthat/test-shiny-files.R")'`
Expected: PASS。
Run: `/usr/local/bin/Rscript tests/run_shiny_app_startup.R`
Expected: 启动成功。

- [ ] **Step 5: Commit**

```bash
git add inst/app/modules/mod_visual_lab.R inst/app/modules/mod_topology_results.R inst/app/modules/mod_perturbation.R inst/app/modules/mod_network_compare.R tests/testthat/test-shiny-files.R
git commit -m "feat: add loading spinners to long-running outputs"
```

---

## Task 8: 空状态引导

在主要结果面板未选对象时显示 `ui_empty_state`。代表性地落地 Graph Explorer、Topology、Perturbation。

**Files:**
- Modify: `inst/app/modules/mod_graph_explorer.R`, `mod_topology_results.R`, `mod_perturbation.R`
- Modify: `tests/testthat/test-shiny-files.R`

- [ ] **Step 1: 写失败断言**

追加到 `tests/testthat/test-shiny-files.R`:
```r
test_that("result panels show empty-state guidance when nothing is selected", {
  for (f in c("mod_graph_explorer.R", "mod_topology_results.R", "mod_perturbation.R")) {
    txt <- paste(readLines(test_path(file.path("../../inst/app/modules", f)), warn = FALSE), collapse = "\n")
    expect_match(txt, "ui_empty_state", fixed = TRUE)
  }
})
```

- [ ] **Step 2: 跑测试确认失败**

Run: `/usr/local/bin/Rscript -e 'library(testthat); test_file("tests/testthat/test-shiny-files.R")'`
Expected: 新 test 失败。

- [ ] **Step 3: 各模块在"无对象"分支返回 empty state**

每个模块的主结果 `renderUI`/`renderPlot` 在无所选图时,改为返回(对 renderUI)或在外层包一个 `uiOutput` 显示:
```r
    output$placeholder <- shiny::renderUI({
      if (!is.null(selected_graph())) return(NULL)
      ui_empty_state(
        icon = "diagram-3",
        title = "No graph selected",
        hint = "Build or pick a graph object to see results here."
      )
    })
```
并在 UI 结果区顶部加 `shiny::uiOutput(ns("placeholder"))`。`selected_graph()` 用各模块实际的"当前所选对象" reactive 名替换。

- [ ] **Step 4: 跑测试 + 启动确认**

Run: `/usr/local/bin/Rscript -e 'library(testthat); test_file("tests/testthat/test-shiny-files.R")'`
Expected: PASS。
Run: `/usr/local/bin/Rscript tests/run_shiny_app_startup.R`
Expected: 启动成功。

- [ ] **Step 5: Commit**

```bash
git add inst/app/modules/mod_graph_explorer.R inst/app/modules/mod_topology_results.R inst/app/modules/mod_perturbation.R tests/testthat/test-shiny-files.R
git commit -m "feat: add empty-state guidance to result panels"
```

---

## Task 9: 输入校验全覆盖

在长任务的 server observe/eventReactive 入口加 `shiny::req()` + `validate(need(...))`,缺前置条件时给友好提示。代表性落地 Visual Lab、Topology、Perturbation。

**Files:**
- Modify: `inst/app/modules/mod_visual_lab.R`, `mod_topology_results.R`, `mod_perturbation.R`
- Modify: `tests/testthat/test-shiny-files.R`

- [ ] **Step 1: 写失败断言**

追加:
```r
test_that("long-task panels validate inputs before running", {
  for (f in c("mod_visual_lab.R", "mod_topology_results.R", "mod_perturbation.R")) {
    txt <- paste(readLines(test_path(file.path("../../inst/app/modules", f)), warn = FALSE), collapse = "\n")
    expect_match(txt, "validate(", fixed = TRUE)
    expect_match(txt, "need(", fixed = TRUE)
  }
})
```

- [ ] **Step 2: 跑测试确认失败**

Run: `/usr/local/bin/Rscript -e 'library(testthat); test_file("tests/testthat/test-shiny-files.R")'`
Expected: 新 test 失败(若某文件已含,仅补缺的)。

- [ ] **Step 3: 在每个 run 入口加校验**

在各模块主"运行"`observeEvent`/`eventReactive` 体的第一行加(以 Visual Lab 绘图为例):
```r
      shiny::validate(
        shiny::need(!is.null(input$graph) && nzchar(input$graph),
                    "Select a graph object first.")
      )
```
Topology / Perturbation 同理,把 `input$graph` 换成各自选择图的 input id(grep `selectInput` 找代表"图对象"的那个 id)。

- [ ] **Step 4: 跑测试 + 启动确认**

Run: `/usr/local/bin/Rscript -e 'library(testthat); test_file("tests/testthat/test-shiny-files.R")'`
Expected: PASS。
Run: `/usr/local/bin/Rscript tests/run_shiny_app_startup.R`
Expected: 启动成功。

- [ ] **Step 5: Commit**

```bash
git add inst/app/modules/mod_visual_lab.R inst/app/modules/mod_topology_results.R inst/app/modules/mod_perturbation.R tests/testthat/test-shiny-files.R
git commit -m "feat: validate inputs before running long tasks"
```

---

## Task 10: DT 工程化 (dt_table 接入)

把各模块的 `DT::renderDT`/`DT::renderDataTable` 主表改走 `dt_table()`,获得导出按钮/横向滚动/数值格式化。

**Files:**
- Modify: `inst/app/modules/mod_topology_results.R`, `mod_zipi_results.R`, `mod_perturbation.R`, `mod_data_hub.R`
- Modify: `tests/testthat/test-shiny-files.R`

- [ ] **Step 1: 写失败断言**

追加:
```r
test_that("primary tables route through the dt_table wrapper", {
  for (f in c("mod_topology_results.R", "mod_zipi_results.R",
              "mod_perturbation.R", "mod_data_hub.R")) {
    txt <- paste(readLines(test_path(file.path("../../inst/app/modules", f)), warn = FALSE), collapse = "\n")
    expect_match(txt, "dt_table(", fixed = TRUE)
  }
})
```

- [ ] **Step 2: 跑测试确认失败**

Run: `/usr/local/bin/Rscript -e 'library(testthat); test_file("tests/testthat/test-shiny-files.R")'`
Expected: 新 test 失败。

- [ ] **Step 3: 主表渲染改用 dt_table**

每个模块找到主表 render(grep `renderDT`/`renderDataTable`),把内部 `DT::datatable(df, ...)` 替换为 `dt_table(df)`。例:
```r
    output$topology <- DT::renderDT({
      tb <- topology_table()
      shiny::req(tb)
      dt_table(tb)
    })
```
对预览类小表(如 Data Hub preview)可用 `dt_table(df, page_length = 5)`。保留各自的 `req()`。

- [ ] **Step 4: 跑测试 + 启动确认**

Run: `/usr/local/bin/Rscript -e 'library(testthat); test_file("tests/testthat/test-shiny-files.R")'`
Expected: PASS。
Run: `/usr/local/bin/Rscript tests/run_shiny_app_startup.R`
Expected: 启动成功。

- [ ] **Step 5: Commit**

```bash
git add inst/app/modules/mod_topology_results.R inst/app/modules/mod_zipi_results.R inst/app/modules/mod_perturbation.R inst/app/modules/mod_data_hub.R tests/testthat/test-shiny-files.R
git commit -m "feat: route primary tables through engineering-grade dt_table"
```

---

## Task 11: 统一错误 toast

确保失败路径走 `notify(..., type = "error")`。代表性地接 Graph Builder、Topology、Perturbation 的 `app_failure` 反馈。

**Files:**
- Modify: `inst/app/modules/mod_graph_builder.R`, `mod_topology_results.R`, `mod_perturbation.R`
- Modify: `tests/testthat/test-shiny-files.R`

- [ ] **Step 1: 写失败断言**

追加:
```r
test_that("failure paths surface unified toasts via notify()", {
  for (f in c("mod_graph_builder.R", "mod_topology_results.R", "mod_perturbation.R")) {
    txt <- paste(readLines(test_path(file.path("../../inst/app/modules", f)), warn = FALSE), collapse = "\n")
    expect_match(txt, "notify(", fixed = TRUE)
  }
})
```

- [ ] **Step 2: 跑测试确认失败**

Run: `/usr/local/bin/Rscript -e 'library(testthat); test_file("tests/testthat/test-shiny-files.R")'`
Expected: 新 test 失败。

- [ ] **Step 3: 在 app_failure 分支加 notify**

各模块处理 adapter 结果时,失败分支加 toast。`app_result` 约定(见 `R/app_adapters.R`)结果含 `$ok`/`$message`。在每个 run 的结果处理处加:
```r
      res <- safe_topology(...)        # 模块各自的 safe_* 调用
      if (isFALSE(res$ok)) {
        notify(res$message %||% "Operation failed.", type = "error")
        return(NULL)
      }
```
若文件无 `%||%`,在顶部加 `` `%||%` <- function(a, b) if (is.null(a)) b else a ``。`$ok`/`$message` 字段名以 `app_success`/`app_failure` 实际为准(实现前先 grep `R/app_adapters.R` 的 `app_failure <- function` 确认字段)。

- [ ] **Step 4: 跑测试 + 启动确认**

Run: `/usr/local/bin/Rscript -e 'library(testthat); test_file("tests/testthat/test-shiny-files.R")'`
Expected: PASS。
Run: `/usr/local/bin/Rscript tests/run_shiny_app_startup.R`
Expected: 启动成功。

- [ ] **Step 5: Commit**

```bash
git add inst/app/modules/mod_graph_builder.R inst/app/modules/mod_topology_results.R inst/app/modules/mod_perturbation.R tests/testthat/test-shiny-files.R
git commit -m "feat: surface adapter failures via unified toasts"
```

---

## Task 12: 浏览器 smoke + 全套回归

**Files:**
- Create: `tests/run_shiny_ui_polish_smoke.R`
- Modify: `tests/testthat/test-shiny-files.R`

- [ ] **Step 1: 写失败的结构断言(smoke 存在性)**

追加到 `tests/testthat/test-shiny-files.R`:
```r
test_that("UI polish browser smoke exists and checks landing CTA + value boxes", {
  path <- test_path("../../tests/run_shiny_ui_polish_smoke.R")
  expect_true(file.exists(path))
  txt <- paste(readLines(path, warn = FALSE), collapse = "\n")
  expect_match(txt, "Load example data", fixed = TRUE)
  expect_match(txt, "Graph Builder", fixed = TRUE)
  expect_match(txt, "value-box", fixed = TRUE)
})
```

- [ ] **Step 2: 跑测试确认失败**

Run: `/usr/local/bin/Rscript -e 'library(testthat); test_file("tests/testthat/test-shiny-files.R")'`
Expected: 新 test 失败(文件不存在)。

- [ ] **Step 3: 写 `tests/run_shiny_ui_polish_smoke.R`**

以仓库现有 `tests/run_shiny_phase2_workflow_smoke.R` 为模板(同样的 shinytest2 启动样板:定位 repo_root、`shinytest2::AppDriver$new(app_dir)`)。核心断言:
```r
# (沿用 phase2 smoke 的头部:repo_root 解析 + library(shinytest2) + app 启动)
app <- shinytest2::AppDriver$new(app_dir, name = "ui-polish", height = 900, width = 1280)
on.exit(app$stop(), add = TRUE)

# 1) 着陆页 CTA 加载示例并跳到 Graph Builder
app$click(selector = "#landing-start_example")
app$wait_for_idle(timeout = 30000)
# 跳转后 Graph Builder 的数据选择应出现示例对象
app$set_inputs(main_nav = "Graph Builder")
app$wait_for_idle()

# 2) 进入 Graph Explorer,断言 value_box 出现
app$set_inputs(main_nav = "Graph Explorer")
app$wait_for_idle()
html <- app$get_html("body")
stopifnot(grepl("value-box", html))

cat("ui-polish smoke OK\n")
```
注:`#landing-start_example` 为命名空间化按钮 id(模块 id `landing` + input `start_example`)。Graph Builder 数据选择 input id 以模块实际为准;若直接断言对象较脆,可退而断言跳转后页面无 error 且 Graph Explorer 出现 `value-box`。

- [ ] **Step 4: 跑新 smoke**

Run: `/usr/local/bin/Rscript tests/run_shiny_ui_polish_smoke.R`
Expected: 打印 `ui-polish smoke OK`，退出码 0。(若 shinytest2/chromote 环境缺失而跳过,记录原因。)

- [ ] **Step 5: 全套回归**

Run（逐条）:
```bash
/usr/local/bin/Rscript -e 'library(testthat); test_file("tests/testthat/test-shiny-files.R")'
/usr/local/bin/Rscript -e 'library(testthat); test_file("tests/testthat/test-app-theme.R")'
/usr/local/bin/Rscript -e 'library(testthat); test_file("tests/testthat/test-app-ui-helpers.R")'
/usr/local/bin/Rscript -e 'library(testthat); test_file("tests/testthat/test-perturbation-availability.R")'
/usr/local/bin/Rscript tests/run_shiny_app_startup.R
/usr/local/bin/Rscript tests/run_shiny_mobile_layout_smoke.R
/usr/local/bin/Rscript tests/run_shiny_visual_layouts_smoke.R
```
Expected: 全部 PASS / OK,无移动端横向溢出回归。

- [ ] **Step 6: Commit**

```bash
git add tests/run_shiny_ui_polish_smoke.R tests/testthat/test-shiny-files.R
git commit -m "test: add UI polish browser smoke and regression coverage"
```

---

## 最终验证清单

完成后依次跑:
- [ ] `test_file("tests/testthat/test-shiny-files.R")` — 结构断言
- [ ] `test_file("tests/testthat/test-app-theme.R")` / `test-app-ui-helpers.R` — helper 单元
- [ ] `Rscript tests/run_shiny_app_startup.R` — 启动
- [ ] `Rscript tests/run_shiny_ui_polish_smoke.R` — 新 UI 浏览器 smoke
- [ ] `Rscript tests/run_shiny_mobile_layout_smoke.R` — 移动端无溢出回归
- [ ] `Rscript tests/run_shiny_visual_layouts_smoke.R` — Visual Lab 全布局回归
- [ ] 人工:亮/暗模式切换均可读;着陆页 CTA 一键加载示例并跳转;指标卡/spinner/空状态/DT 导出/错误 toast 行为正确

## 不在本轮范围(tier 3,后续)

全量 `layout_sidebar` 统一、`styles.css` 的 `!important` 彻底清理、全局 header 工具区、Manual 响应式重构。
