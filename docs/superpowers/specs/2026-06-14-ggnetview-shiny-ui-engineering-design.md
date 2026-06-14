# ggNetView Shiny 界面工程化改造 — 设计

Date: 2026-06-14

> **For agentic workers:** 本设计只覆盖 UI/UX 表现层(第一 + 第二梯队)。**不得**修改 `R/app_*.R` adapter 后端逻辑、包的分析函数或数据流契约。实现按 writing-plans 拆出的计划逐条推进:先写/改测试,确认失败,再实现,最后跑对应 smoke/test 验证。

## 背景

Shiny 应用(`inst/app/`)功能覆盖已很全:10+ 导航面板、12 个 module、`R/app_*.R` adapter 解耦层、13 个 smoke 脚本(含 shinytest2)。但界面停留在"能用"层面,缺工程级打磨,具体表现:

- 主题只用裸 `bs_theme(bootswatch = "flatly")`,无品牌色/字体/logo/favicon,与品牌(洋红 hex 贴纸 logo)脱节。
- `inst/app/www/styles.css`(196 行)大量 `!important` 覆盖 bslib 默认布局——技术债信号。
- **零 `value_box`** 指标卡;长任务**仅 1 处 spinner**;Introduction 直接 `includeMarkdown(README)`;Manual 是裸 iframe。
- 输入校验仅 2 个 module 用了 `validate/need`;DT 表格裸用;错误反馈不统一。

## 目标与范围

| | |
|---|---|
| **做** | 第一 + 第二梯队,纯 UI/UX 层升级 |
| **不动** | `R/app_*.R` adapter 后端逻辑、包分析函数、数据流契约 |
| **暂不做(后续单独一轮)** | 全量 `layout_sidebar` 重构、`styles.css` 的 `!important` 彻底清理、全局 header 工具区、Manual 重构 |
| **新增依赖** | `shinycssloaders`、`bsicons`(写入 `DESCRIPTION` Imports) |

## 已敲定的设计决策(brainstorming 结论)

1. **范围**:第一 + 第二梯队。
2. **视觉方向**:洋红点缀(restrained accent)—— 白底干净,品牌洋红只用在 logo / 激活态 / 主按钮 / 指标卡左条 / 标题,长时间分析不累眼。
3. **着陆页**:引导式(onboarding)—— Hero + 一键"加载示例跳到 Builder" + 3 步快速上手。
4. **依赖**:允许加 `shinycssloaders` + `bsicons`,体验优先。

## 品牌色令牌(从 `man/figures/logo.png` 提取)

- **主色(magenta)**:`#AE017E`
- **深梅(标题/hover/数字)**:`#7D0159`
- **极淡粉表面**(仅指标卡左条/激活态等细微处):`#FFF0F3`
- **主画布**:白 / 浅灰(`#FAFAFA`),非粉
- **模块分类调色板**(供指标卡图标、分类图例、分类填色统一取用):
  `#F08D8D` 红 · `#F2D24D` 黄 · `#F2A65A` 橙 · `#9FD18B` 绿 · `#6FC4C0` 青 · `#7FB3E0` 蓝 · `#C9A0DC` 紫 · `#E89BC4` 粉

## 设计系统(新建 `R/app_theme.R`)

- `app_bs_theme()`:`bs_theme(version = 5)` 自定义,`primary = "#AE017E"`,设置 base/heading 字体(易读无衬线,如 Inter/系统栈),返回主题对象供 `ui.R` 使用。
- 亮/暗模式:navbar 接入 `bslib::input_dark_mode()`。
- `app_module_palette()`:返回上述 8 色命名向量,供 value_box / 图例 / 分类填色复用。
- Logo + favicon:复用 `man/figures/logo.png` 生成小尺寸,放入 `inst/app/www/`,navbar `title` 用 logo + 文字。

## 第一梯队组件

1. **品牌主题落地** — `inst/app/ui.R` 接入 `app_bs_theme()` + logo/favicon + `input_dark_mode()`。
2. **着陆页重做** — 引导式 Introduction:Hero(logo + tagline)+ "加载示例并跳到 Builder" 一键按钮 + 3 步快速上手卡(Load data → Build graph → Visualize & analyze)。原 README 详细内容移到着陆页底部 "Learn more" 折叠区(`bslib::accordion`,内容不丢)。一键按钮通过现有 registry 加载示例数据并 `nav_select` 跳转。
3. **指标卡 `value_box`** — 落点:
   - Graph Explorer:节点数 / 边数 / 模块数 / 密度
   - Topology:关键全局指标
   - Perturbation:Schneider R
   - Network Compare:网络数 / 共有边
   图标用 `bsicons::bs_icon()`,左条/数字用品牌色。
4. **统一加载态** — 所有长任务输出(graph build / RMT scan / plot / topology / perturbation / compare)包 `shinycssloaders::withSpinner()`,spinner 色用洋红;与现有 `app_task_feedback` 按钮忙碌态并存。

## 第二梯队组件(新建 `R/app_ui_helpers.R` 放可复用件)

5. **空状态** — `ui_empty_state(icon, title, hint, action = NULL)`:未选对象 / 无数据时渲染引导卡(含跳转按钮),取代空白图或红字报错。
6. **输入校验全覆盖** — 各 module 统一 `shiny::req()` + `validate(need(...))`,把"必须先建图"等前置条件变成友好提示。
7. **DT 工程化** — 共享 `dt_table(df, ...)` 封装:Buttons extension(copy/csv/excel)、`scrollX`、固定表头、数值列 `formatRound`、合理分页与列宽默认值。各 module 的 `DT::renderDT` 改走此封装。
8. **统一错误 toast** — `notify(message, type)` helper;确保所有 `app_failure` 路径一致地走 `showNotification`,错误详情可展开。

## 代码落点

- 新建 `R/app_theme.R`:`app_bs_theme()`、`app_module_palette()`。
- 新建 `R/app_ui_helpers.R`:`ui_empty_state()`、`value_box` 封装、`dt_table()`、`notify()`。
- 两个新文件在 `inst/app/global.R` 的 `app_helper_files` 中注册。
- `inst/app/www/styles.css`:加品牌 CSS 变量(`--ggnv-primary` 等);**仅**移除因新方案而冗余的 `!important`,不做 tier-3 全量清理。
- 各 module(`inst/app/modules/*.R`)最小化改动,引用上述 helper。
- `DESCRIPTION`:Imports 追加 `shinycssloaders`、`bsicons`。

## 测试

- 更新 `tests/testthat/test-shiny-files.R`:断言新着陆页元素、主题接入、`R/app_theme.R` 与 `R/app_ui_helpers.R` 存在并被注册。
- 新增/扩展 shinytest2 smoke(可放 `tests/run_shiny_ui_polish_smoke.R`):
  - 着陆页 "加载示例并跳到 Builder" CTA 能加载示例数据并跳转到 Graph Builder。
  - value_box 在 Graph Explorer / Perturbation 渲染并显示数值。
  - 长任务触发后 spinner 出现并在完成后消失。
  - 未选对象时空状态卡显示;选对象后消失。
  - DT 导出按钮(csv)存在。
- 回归:跑现有全套 smoke,重点 `tests/run_shiny_mobile_layout_smoke.R`、`tests/run_shiny_visual_layouts_smoke.R`、`tests/run_shiny_app_startup.R`,确认不破坏。

## 验收标准

- 应用以品牌洋红主题启动,navbar 有 logo + 暗色切换,亮/暗模式均可读。
- Introduction 为引导式着陆页,一键加载示例并跳转可用;README 详情仍可在折叠区访问。
- 指定页面出现 value_box 指标卡;长任务有 spinner;空状态有引导卡;DT 可导出;错误走统一 toast。
- `DESCRIPTION` 仅新增 `shinycssloaders`、`bsicons` 两个 Imports。
- 全套 smoke 通过,无移动端横向溢出回归。

## 不在本轮范围(tier 3,后续)

全量 `layout_sidebar` 统一、`styles.css` 的 `!important` 彻底清理、全局 header 工具区(当前对象指示 / reset session)、Manual 响应式重构。
