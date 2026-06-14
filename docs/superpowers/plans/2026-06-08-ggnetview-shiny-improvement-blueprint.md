# ggNetView Shiny 改进蓝图

Date: 2026-06-08

> **For agentic workers:** 本计划按任务逐条推进，步骤用 `- [ ]` 复选框跟踪。每个任务先写/改测试，确认失败，再实现，最后跑对应 smoke/test 验证。

**目标:** 在不破坏现有 registry / adapter 契约的前提下，(1) 同步最新版 ggNetView 包，(2) 新增"扰动 / 鲁棒性分析"工作流，(3) 改造 Visual Lab 的参数体验，(4) 收敛顶级导航。

**技术栈:** R, Shiny, bslib, DT, testthat, shinytest2, 现有 `R/app_*.R` adapter 体系。

---

## 背景与现状评估

当前 Shiny（`inst/app/`）已相当成熟：10 个导航面板 + 12 个 module，后端通过 `R/app_*.R` adapter 与包 API 解耦，13+ 个 smoke 脚本（含 shinytest2 浏览器测试），发布证据显示手册 10 个领域 100% 覆盖。

本轮改进基于两个发现：

1. **新版包多了一整套"虚拟扰动分析"导出函数**，Shiny 尚未暴露：
   - `get_network_perturbation()` — 结构性"虚拟攻击"，输出扰动曲线 + Schneider R 鲁棒性指数
   - `ggnetview_perturbation_curve()` — 绘制扰动曲线
   - `get_node_influence()` — 丰度影响力传播（Katz / 带重启随机游走）
   - `press_perturbation()` — 生态"压力扰动"，净效应矩阵 N = -A⁻¹
2. **参数体验 > 页面布局** 是当前最大短板，集中在 Visual Lab：`visual_lab_params()` 30+ 参数平铺，缺分组/渐进展示、缺上下文说明、参数未按所选布局动态显隐。

---

## 优先级总览

| 优先级 | 主题 | 价值 | 风险 | 预估工作量 |
|---|---|---|---|---|
| **P0** | 同步新版包到 Shiny 仓库 | 阻塞项，后续都依赖它 | 低 | 小 |
| **P1** | 新增 Perturbation / Robustness 标签页 | 暴露 4 个新函数，填补最大功能空白 | 中 | 中—大 |
| **P2** | Visual Lab 参数体验改造 | 直接改善上手门槛与日常使用 | 低—中 | 中 |
| **P3** | 顶级导航收敛（Analysis 父级） | 信息架构更清晰 | 低 | 小—中 |

建议按 P0 → P1 → P2 → P3 顺序推进；P2 与 P3 互相独立，可并行或按精力安排。

---

## P0 — 同步最新版 ggNetView 包

**问题:** `package/ggNetView/` 下的新版包比 Shiny 仓库当前使用的版本多 4 个导出函数（见上）。Shiny 的 `NAMESPACE`（54 个 export）落后于新包（57 个），且 `R/` 下没有这 4 个函数文件。`global.R` 通过 `resolve_ggnetview_function()` / `asNamespace("ggNetView")` 解析函数，因此必须先让新函数对 Shiny 可见。

**两种同步策略（择一，建议 A）:**

- **A（推荐）安装版优先:** 从 `package/ggNetView/` 安装最新包（`R CMD INSTALL` 或 `devtools::install()`），让 `library(ggNetView)` / `asNamespace()` 直接提供新函数。Shiny 仓库不再维护源文件副本，最干净。
- **B 源文件副本:** 把 4 个 `.R` 拷进 Shiny 仓库 `R/`，并更新 `NAMESPACE`。维护成本高、易与包版本漂移，不推荐长期使用。

### Task 0.1: 确认版本基线与同步策略

**Files:** `DESCRIPTION`, `NAMESPACE`

- [ ] 对比 `package/ggNetView/DESCRIPTION` 与 Shiny 仓库 `DESCRIPTION` 的 Version；如新包已 bump，同步版本号。
- [ ] 选定同步策略（建议 A）。
- [ ] 若用 A：从 `package/ggNetView/` 安装最新包，记录安装命令到 `Makefile`。
- [ ] 若用 B：拷贝 `get_network_perturbation.R`、`get_node_influence.R`、`ggnetview_perturbation_curve.R`、`press_perturbation.R` 到 `R/`，并在 `NAMESPACE` 追加 4 个 `export(...)`。

### Task 0.2: 验证新函数对 Shiny 可见

**Files:** 新增 `tests/testthat/test-perturbation-availability.R`

- [ ] 写测试断言 `resolve_ggnetview_function("get_network_perturbation")` 等 4 个均非 NULL。
- [ ] 跑测试确认通过；若失败，回到 Task 0.1 修复加载路径。

### Task 0.3: 顺手修包检查报错（可选但建议）

**Files:** `DESCRIPTION`, `.Rbuildignore`, `.gitignore`

- [ ] `R CMD check` 当前报 `Author`/`Maintainer` 缺失：跑一次 `devtools::document()` 让 `Authors@R` 生成对应字段。
- [ ] 把 `.RData`、`.Rhistory`、`..Rcheck/` 加入 `.Rbuildignore` 与 `.gitignore`（仓库当前 1.2G，根目录 `.RData` 30MB、`README.html` 12MB）。

---

## P1 — 新增 Perturbation / Robustness 工作流

**目标:** 新建一个标签页（或 Analysis 父级下的子页），暴露 4 个新函数。输出形态为"图 + 指标卡片 + 可下载表"，复用现有 registry / export 体系。

**新函数签名要点（实现 adapter 时对照）:**

```r
get_network_perturbation(graph_obj,
  strategy   = c("random","targeted","module","manual"),
  centrality = c("degree","strength","betweenness","closeness","eigenvector","ivi"),
  target = NULL, module_col = "Modularity",
  fractions = seq(0.05, 1, by = 0.05), decreasing = TRUE,
  bootstrap = 100, seed = 123, plot = TRUE)        # 返回 curve + Schneider R

ggnetview_perturbation_curve(curve, metric = "LCC_fraction")

get_node_influence(graph_obj, source, delta = 1, alpha = 0.5,
  signed = TRUE, drop_source = TRUE)

press_perturbation(graph_obj = NULL, cor_mat = NULL,
  self_regulation = NULL, source = NULL)            # 返回净效应矩阵 N = -A^-1
```

### Task 1.1: 锁定导航与文件骨架

**Files:** `tests/testthat/test-shiny-files.R`, `inst/app/ui.R`, `inst/app/global.R`

- [ ] 写测试要求存在 `inst/app/modules/mod_perturbation.R` 与新标签页（标题如 `Perturbation`），并在 `global.R` 的模块 source 列表中。
- [ ] 跑测试确认实现前失败。

### Task 1.2: 编写扰动 adapter

**Files:** 新增 `R/app_perturbation_adapters.R`，在 `global.R` 的 `app_helper_files` 中注册

- [ ] 仿 `app_topology_adapters.R` 的 `safe_*` 模式（`resolve_ggnetview_function` + `safe_call` + `app_success/app_failure`）实现：
  - `safe_network_perturbation(graph, params)` — 包 `get_network_perturbation`，返回 `list(curve=..., schneider_R=..., plot=...)`。
  - `safe_node_influence(graph, source, params)` — 包 `get_node_influence`，返回影响力表。
  - `safe_press_perturbation(graph_or_cor, params)` — 包 `press_perturbation`，返回净效应矩阵。
- [ ] 每个 adapter 对输入类型（igraph / 相关矩阵）做校验，错误走 `app_failure`。

### Task 1.3: 实现 mod_perturbation 模块

**Files:** 新增 `inst/app/modules/mod_perturbation.R`

- [ ] 侧栏 `accordion` 三个面板：
  - **Structural attack:** 选图对象 + `strategy`（random/targeted/module/manual）+ 条件显示 `centrality`（targeted 时）/ `target`（manual/module 时）+ `bootstrap`/`seed`/`fractions` 高级项 → `Run attack` 按钮。
  - **Node influence:** 选图对象 + 多选 `source` 节点 + `alpha`/`delta`/`signed` → `Compute influence`。
  - **Press perturbation:** 选图对象或相关矩阵 + 可选 `self_regulation`/`source` → `Run press`。
- [ ] 右侧 `card` 区：扰动曲线图（`ggnetview_perturbation_curve`，含 metric 下拉切换）、Schneider R 指标卡、影响力表（DT + 下载）、净效应矩阵热图/表（DT + 下载）。
- [ ] 用 `app_task_feedback` 包裹三个长任务按钮的忙碌态（沿用现有共享机制）。
- [ ] 把生成的曲线/表/矩阵 register 进对象 registry，使其可被 Export Center 导出。

### Task 1.4: 浏览器 smoke 测试

**Files:** 新增 `tests/run_shiny_perturbation_smoke.R`，更新 `tests/_smoke_coverage/`

- [ ] shinytest2 路径：进入 Perturbation 标签，跑一次 structural attack（random + targeted）、一次 node influence、一次 press，断言曲线渲染、Schneider R 出现、表格非空、下载按钮可用。
- [ ] 更新发布证据/覆盖率 JSON，纳入新领域。

---

## P2 — Visual Lab 参数体验改造

**目标:** 把 `mod_visual_lab.R` 的 30+ 参数从平铺改为"分层 + 动态显隐 + 带说明"，不改后端 `visual_lab_params()` 契约。

### Task 2.1: 参数三层分组

**Files:** `inst/app/modules/mod_visual_lab.R`

- [ ] 用 `bslib::accordion` 把控件分三层：
  - **基础**（默认展开）：布局选择、`layout_module`、`show_labels`、点大小、`seed`。
  - **外观**（默认折叠）：边可见性/曲度/`linealpha`/`linecolor`、标签布局/换行、point label。
  - **高级微调**（默认折叠）：`shrink`、`inner_shrink`、`k_nn`、`push_others_delta`、`node_add`、`ring_n`、`r`、`jitter`/`jitter_sd` 等。
- [ ] 写测试断言三个 accordion 面板存在且默认开合状态正确。

### Task 2.2: 参数按布局动态显隐

**Files:** `inst/app/modules/mod_visual_lab.R`

- [ ] 根据所选 `layout` 决定相关参数可见性：`circular_modules_*` / `multipartite` 才显示 ring/petal/module 相关项；`fr`/`kk`/`nicely` 等通用布局隐藏它们。
- [ ] 用 `conditionalPanel` 或 server 端 `shinyjs::toggle` / `updateSelectInput` 实现；选择最少依赖的方案（优先 `conditionalPanel`，避免新增 shinyjs 依赖）。
- [ ] 浏览器 smoke 断言：选 `fr` 时高级 ring 参数不可见，选某 `circular_modules_*` 时可见。

### Task 2.3: 参数上下文说明

**Files:** `inst/app/modules/mod_visual_lab.R`, `inst/app/www/styles.css`

- [ ] 给易混参数加 tooltip / 帮助图标（如 `shrink` vs `inner_shrink`、`k_nn`、`push_others_delta`），文案取自 manual `05-layout.Rmd`。
- [ ] 用 `bslib::tooltip()` 或 label 后缀图标实现，必要时加少量 CSS。

---

## P3 — 顶级导航收敛

**目标:** 当前 10 个顶级 tab 偏多。把分析类页面收进一个 Analysis 父级，降低导航宽度与认知负担。

### Task 3.1: 引入 Analysis 父级菜单

**Files:** `inst/app/ui.R`, `tests/testthat/test-shiny-files.R`

- [ ] 用 `bslib::nav_menu("Analysis", ...)` 收纳 `Topology`、`Zi-Pi`、`Perturbation`、`Network Compare`、`Environment Links`。
- [ ] 顶级保留：Introduction、Manual、Data Hub、Graph Builder、RMT Builder、Graph Explorer、Visual Lab、Analysis（菜单）、Export。
- [ ] 更新 `test-shiny-files.R` 中对导航结构的断言。

### Task 3.2: 回归验证

**Files:** 现有 `tests/run_shiny_*_smoke.R`

- [ ] 跑 mobile-layout 与 phase2 浏览器 smoke，确认菜单化后各 tab 仍可达、无横向溢出。

---

## 最终验证清单

完成后依次跑（沿用发布证据里的命令风格）：

- [ ] `testthat::test_file("tests/testthat/test-shiny-files.R")` — 导航/文件结构
- [ ] `testthat::test_file("tests/testthat/test-perturbation-availability.R")` — 新函数可见
- [ ] `Rscript tests/run_shiny_app_startup.R` — 启动
- [ ] `Rscript tests/run_shiny_manual_workflow_smoke.R` — 后端工作流
- [ ] `Rscript tests/run_shiny_perturbation_smoke.R` — 新扰动工作流
- [ ] `Rscript tests/run_shiny_visual_layouts_smoke.R` — Visual Lab 全布局
- [ ] `Rscript tests/run_shiny_mobile_layout_smoke.R` — 导航收敛后移动端
- [ ] 重新生成 `docs/ggnetview-shiny-release-evidence.md`

---

## 建议推进顺序

1. **先 P0**（半天内可完成），它是其余一切的前置。
2. **再 P1**（本轮核心价值，新功能）。
3. **P2 / P3 可并行**，按精力安排；两者都低风险、独立。
