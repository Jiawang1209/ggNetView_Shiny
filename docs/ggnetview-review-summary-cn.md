# ggNetView 新包与手册审查中文总览

日期：2026-05-25

这份文档是对 `package/ggNetView/` 和 `package/ggNetView-manual/` 的中文总览，配合下面三份工程文档使用：

- `docs/ggnetview-new-package-shiny-audit.md`
- `docs/ggnetview-api-compatibility-matrix.md`
- `docs/ggnetview-shiny-rebuild-blueprint.md`

## 总体判断

你新放进来的 `package/ggNetView/` 应该被视为新版权威包，而不是当前根目录旧包的一次小修。它和旧版相比已经有明显的新工作流、新导出函数、新测试和新手册支撑。

现有 `ggNetView.shiny/` 更像旧版包的 GUI 原型。它可以继续作为起点，但不应该继续用“往现有 `ui.R` / `server.R` 里加控件”的方式扩展。新版手册对应的是一个更大的工作台型 Shiny，而不是简单五个 tab。

## 已确认的事实

- 新包 `package/ggNetView/R/` 有 127 个 R 文件。
- 新包所有 R 文件 parse 通过。
- 新包 `package/ggNetView/man/` 有 76 个 Rd 文档。
- 新包 `package/ggNetView/tests/testthat/` 有 11 个测试文件。
- 新手册顶层有 11 个 Rmd 文件，覆盖建图、RMT、图信息、子图、布局、拓扑、网络比较、环境关联、gallery 等。
- 当前 Shiny 只调用了少量核心函数：`build_graph_from_mat()`、`build_graph_from_adj_mat()`、`build_graph_from_df()`、`ggNetView()`、`get_network_topology()`、`ggnetview_zipi()`、`gglink_heatmaps()` 等。

## 新版包新增的关键能力

新版包新增或显著增强了这些能力：

| 能力 | 代表函数 | 对 Shiny 的意义 |
| --- | --- | --- |
| 多方法共识网络 | `build_graph_from_consensus()` | 可以把多个 adjacency / 方法结果融合成一个 consensus graph。 |
| 节点表 + 边表建图 | `build_graph_from_node_edge()` | 可以保留孤立节点，适合真实 PPI / annotation 场景。 |
| STRINGDB 导入 | `build_graph_from_stringdb()` | 可支持 STRING/PPI 数据导入和 score filtering。 |
| 样本级子图 | `get_sample_subgraph()` | 可以按样本提取 presence-based induced subgraph。 |
| 节点中心性 | `get_node_centrality()` | 可做节点重要性排序和可视化映射。 |
| IVI 节点影响力 | `get_node_ivi()` | 可识别关键节点，但依赖 suggested 包 `influential`。 |
| RMT 阈值 | `ggNetView_RMT()` | 可把主观相关阈值变成数据驱动阈值。 |
| 多网络比较 | `ggNetView_multi_link()` | 对应手册第 7 章，是新版 Shiny 应有的一大模块。 |
| 环境关联增强 | `gglink_heatmaps()` | 新增 Mantel 模式、collapse 模式、显著性分层、线条映射等。 |

## 同名函数替换风险

我重点比对了同名函数。结论是：大多数旧调用不会因为参数删除而立刻崩掉，因为新版没有删除这些关键函数的旧参数；但语义和可用能力已经明显扩展。

需要重点迁移的同名函数：

| 函数 | 变化重点 | 风险等级 |
| --- | --- | --- |
| `ggNetView()` | 新增 label layout、label wrapping、label outer pad、outer boundary bandwidth 控制；部分默认外圈样式变了。 | 中 |
| `gglink_heatmaps()` | 新增 Mantel block/column 模式、distance method、permutations、spec collapse、显著/非显著线条层、表达式映射。 | 高 |
| `ggnetview_modularity_heatmaps()` | 跟随新版 Mantel / distance API 扩展。 | 中 |
| `ggNetView_multi_link()` | 手册重度使用，但当前 Shiny 完全没覆盖。 | 高 |
| `ggNetView_multi()` | 新增边界平滑参数，但当前 Shiny 没覆盖。 | 中 |

当前 Shiny 如果直接切到新包，大概率能跑一部分旧流程，但会明显“落后于手册”。这不是崩溃型风险，而是产品覆盖风险。

## 手册反推出来的新版 Shiny 结构

我建议新版 Shiny 不再按旧五个 tab 组织，而是按手册工作流组织：

1. `Data Hub`
   - 管理内置数据、上传数据、对象类型识别。
   - 所有数据对象进入统一 object registry。

2. `Graph Builder`
   - 覆盖 matrix、edge list、node+edge、adjacency、module、double matrix、igraph、WGCNA、consensus、STRINGDB。

3. `Threshold Lab`
   - 专门跑 `ggNetView_RMT()`。
   - 结果回填到 Graph Builder 的 `r.threshold`。

4. `Graph Explorer`
   - 节点表、边表、模块表。
   - module subgraph 和 sample subgraph。
   - 提取出的 subgraph 可以继续用于画图和拓扑分析。

5. `Visual Lab`
   - 新版 `ggNetView()` 全参数，但要分组展示。
   - 重点加入 label 和 outer boundary 新参数。

6. `Topology and Influence`
   - topology、sample topology、centrality、IVI、zi-pi。
   - 必须支持 graph + source matrix 配对，否则部分指标只能是 NA。

7. `Multi-Network Compare`
   - 对应 `ggNetView_multi_link()`。
   - 支持 sample metadata 分组、多 graph list、比较连线、group layout。

8. `Environment Linkage`
   - 重做现有 Env-Spec tab。
   - 支持 correlation、Mantel block-vs-col、Mantel col-vs-col、spec collapse、多 core/spec block。

9. `Gallery Recipes`
   - 把手册里的例子做成可加载 preset，而不是只放说明文字。

## 最重要的架构判断

第一步不应该先改图形参数，也不应该先扩展 Env-Spec。第一步应该做 object registry。

原因是新版工作流都依赖“多个对象之间的关系”：

- graph 需要记住 source matrix；
- RMT 结果要回填给建图；
- consensus 需要多个 adjacency；
- sample subgraph 要变成新的 graph；
- topology 需要 graph + matrix；
- multi-network compare 需要 group metadata 或 graph list；
- environment linkage 需要 env/spec block；
- gallery recipe 需要知道依赖哪些对象。

如果没有 object registry，继续靠 `state$raw`、`state$graph`、`state$topo` 这几个字段，会很快变成不可维护。

## 仓库与发布风险

当前 `package/` 目录约 1.0 GB，其中 `package/ggNetView-manual/` 约 814 MB。里面包含大量渲染产物、PDF、EPUB、figure、`_book/`、`.RData` 等。

这适合本地审查，但不适合直接作为发布内容粗暴提交或打包。

建议后续先决定：

1. `package/ggNetView/` 是否直接替换根目录旧包；
2. `package/ggNetView-manual/` 是否只保留源码 Rmd，还是保留完整渲染产物；
3. 是否用 submodule / sibling repo 管理新包和手册；
4. Shiny 包究竟依赖根目录包，还是依赖 `package/ggNetView/` 这个新包。

## 推荐第一阶段里程碑

第一阶段不要试图一次重构完整 Shiny。建议目标是：

> Data Hub + object registry + matrix/adjacency/edge-list Graph Builder +
> Graph Explorer + basic Visual Lab，全部面向新版包。

这个范围有三个好处：

- 能复刻当前 Shiny 的核心能力；
- 能建立后续 RMT、subgraph、topology、multi-network、environment linkage 所需的对象基础；
- 风险可控，容易验证。

## 当前审查产物

目前已经形成四份文档：

1. `docs/ggnetview-new-package-shiny-audit.md`
   - 新包/手册总审查，API 覆盖矩阵，仓库卫生风险。

2. `docs/ggnetview-api-compatibility-matrix.md`
   - 同名函数兼容性，当前 Shiny 调用点，迁移风险。

3. `docs/ggnetview-shiny-rebuild-blueprint.md`
   - 新版 Shiny 模块边界、对象流、迁移阶段和测试策略。

4. `docs/ggnetview-review-summary-cn.md`
   - 中文总览，便于快速决策和后续沟通。

## 建议下一步

如果要继续进入实现，下一步应先做两个决定：

1. **source-of-truth 决策**
   - 是否把 `package/ggNetView/` 合并/替换到根目录包？

2. **第一阶段实现范围**
   - 是否按我建议的第一阶段：object registry + Data Hub + 基础 Graph Builder + Graph Explorer + Visual Lab？

这两个决定明确后，就可以开始写具体 implementation plan，然后再动 Shiny 代码。

