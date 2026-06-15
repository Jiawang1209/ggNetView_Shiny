# ggNetView 后端深度审计报告

Date: 2026-06-15

经多 agent 审计 + 对抗式复现验证,确认 **25 条**问题: high 5 / medium 6 / low 14。
每条均经独立 agent 实跑/读码核实(默认证伪)。

## HIGH (5)

### H1. `ggNetView / module_layout3 (R/get_geo_neighbors.R:505)` — k_nn not clamped to n-1 before FNN::get.knn; emits loud C-level 'ANN: ERROR' on small networks
- **单元/类别**: building-and-layout · robustness
- **证据**: R/ggnetview.R:483-523 the 'adjacent' path sets k_nn_try <- k_nn (default 12) and k_nn_cap <- max(1, nrow(ly1)-1) but the FIRST call to module_layout3(k_nn = k_nn_try) is made with the RAW, un-clamped k_nn. module_layout3 (R/get_geo_neighbors.R:505) and module_layout2 (R/get_geo_neighbors.R:257) call FNN::get.knn(xy, k = k_nn) directly. When nrow(ly1) <= k_nn the ANN C++ backend prints 'ANN: ERROR-------> Requesting more near neighbors than data points' to stderr (verified by running FNN::get.knn on a 10-point matrix with k=12) and either errors ('k should be less than sample size') or returns NA-padded garbage neighbor indices. This is exactly the observed loud error. There is no min(k_nn, nrow(ly1)-1) guard anywhere before the FNN call.
- **建议修复**: Clamp at the top of module_layout2/3: k_nn <- max(1L, min(as.integer(k_nn), nrow(layout) - 1L)) immediately before FNN::get.knn. Additionally in ggNetView, initialise k_nn_try <- min(k_nn, k_nn_cap) so the first attempt is already in range.
- **验证**: Confirmed by reading the cited code and reproducing the FNN behavior.

CODE (verified): R/get_geo_neighbors.R:257 (module_layout2) and :505 (module_layout3) both call `nn <- FNN::get.knn(xy, k = k_nn)$nn.index` with NO clamp/min() guard. R/ggnetview.R default is `k_nn = 12` (line 272). In the `adjacent` path (R/ggnetview.R:483-523): line 484 sets `k_nn_try <- k_nn` (RAW, un-clamped) and the first module_layout3 call (line 489-492) passes that raw value. `k_nn_cap <- max(1, nrow(ly1)-1)` exists but is only consulted inside the retry loop, never applied to the first attempt.

REPRO (actually ran

### H2. `get_node_centrality` — weighted=TRUE inverts edge weights for ALL measures, but eigenvector/PageRank/HITS treat weights as strength, not distance — meaning is reversed
- **单元/类别**: topology-centrality-ivi · correctness
- **证据**: R/get_node_centrality.R:139 sets a single `w <- 1 / raw_w` and that same `w` is passed to every measure: betweenness (L160), closeness (L165), eigen_centrality (L170), page_rank (L175), hits_scores/hub/authority (L188-200), harmonic (L211). The 1/weight inversion is only correct for the DISTANCE-based measures (betweenness/closeness/harmonic). For igraph's STRENGTH-based measures (eigen_centrality, page_rank, hits) a higher weight means a STRONGER/more-important connection, so 1/weight inverts the intended semantics. Empirically on a graph where edge a-b-c have weight 0.9 and c-d has weight 0.1: eigen_centrality with correct strength weights gives d=0.055 (least central), but with the wrapper's 1/weight gives d=0.986 (nearly most central). page_rank likewise flips d from 0.053 (correct) to 0.332. So on a real correlation network with `weighted=TRUE`, weakly-correlated peripheral nodes are reported as the MOST eigenvector/PageRank-central — the opposite of the truth.
- **建议修复**: Split the weight handling: for distance-semantic measures (Betweenness, Closeness, Harmonic) pass `1/weight`; for strength-semantic measures (Eigenvector, PageRank, Hub_score, Authority_score) pass the raw `weight` directly (|correlation| already means connection strength). Do not reuse one `w` for both families.
- **验证**: CODE CONFIRMED. R/get_node_centrality.R line 139 sets a single `w <- 1/raw_w` and that same `w` is passed to every measure: betweenness (L160), closeness (L165), eigen_centrality (L170, strength-semantic), page_rank (L175, strength-semantic), hits_scores/hub/authority (L188,194,199, strength-semantic), harmonic (L211). The code's own comment (L122-127) only justifies the inversion for distance-based path semantics, which is wrong for the eigenvector/PageRank/HITS family.

REPRODUCED. Could not load full package (heavy unmet deps: ggraph, WGCNA, Hmisc, etc.), so I replicated the function's exac

### H3. `ggNetView_multi_link` — Jitter uses unseeded RNG; module-layout seed is commented out, breaking reproducibility claim
- **单元/类别**: multi-network-compare · determinism
- **证据**: R/ggNetView_multi_link.R:1106-1112 and :1152-1158 call stats::rnorm() with no set.seed() anywhere in the function (grep confirms zero set.seed in the file). The seed argument is also commented out in every module_layout* call (lines 721-722 `# seed = seed`, 740-741, 777, 790). The `seed=1115` formal is only forwarded to build_graph_from_mat / ggNetView / get_*_topology, never used to seed the jitter or module placement RNG.
- **建议修复**: Add `set.seed(seed)` at the top of ggNetView_multi_link before any rnorm/layout call, or wrap each rnorm block with withr::with_seed(seed, ...). Also un-comment and pass `seed = seed` into module_layout/module_layout3/4/5 (and verify those functions honor it).
- **验证**: CONFIRMED. Static evidence verified exactly as claimed in /Users/liuyue/Desktop/R/R_Package_development/ggNetView_Shiny/R/ggNetView_multi_link.R:
- grep shows ZERO set.seed in the file; stats::rnorm jitter at lines 1110-1111 and 1156-1157 (claim cited 1106-1112/1152-1158 block — accurate).
- `seed = seed` is commented out in every module_layout call: lines 722, 740, 777, 790 (confirmed verbatim).
- The seed=1115 formal (line 375) is only forwarded to build_graph_from_mat (line 653) and get_*_topology_parallel (lines 813, 833) — never to seed jitter or module placement.

REPRO (run in R; full p

### H4. `mantel_block_vs_col / mantel_pairwise (and callers gglink_heatmaps, ggnetview_modularity_heatmaps)` — Mantel permutation p-values are not reproducible: no seed on the active code path
- **单元/类别**: environment-heatmaps-mantel · determinism
- **证据**: R/mantel_utils.R:311-317 (mantel_block_vs_col) and R/mantel_utils.R:82-88 (mantel_pairwise) call vegan::mantel with no seed and no set.seed. These are the ONLY mantel helpers invoked by the heatmaps (R/gglink_heatmaps.R:1085 & :1096; R/ggnetview_modularity_heatmaps.R:645 & :673). vegan::mantel permutes via the session RNG, so m$signif changes run-to-run. The only seeded helper, mantel_between_blocks (R/mantel_utils.R:198 set.seed(seed+i)), is never called by either heatmap. The package headline claim is 'reproducible & deterministic', but every Mantel p-value in these two exported functions is non-deterministic.
- **建议修复**: Add a seed argument to mantel_block_vs_col() and mantel_pairwise() and call set.seed() (or pass a permutation control with a fixed RNG) before each vegan::mantel call; thread a seed param down from gglink_heatmaps()/ggnetview_modularity_heatmaps() (default e.g. 1115 to match the layout seed) and from the Shiny adapters.
- **验证**: CONFIRMED. Code matches the evidence exactly: R/mantel_utils.R:311-315 (mantel_block_vs_col) and :82-86 (mantel_pairwise) both call vegan::mantel with no seed and no set.seed; neither function even has a seed parameter. The only seeded helper, mantel_between_blocks (set.seed(seed+i) at line 198), has ZERO call sites outside its own definition (grep across R/ and inst/ confirmed). The heatmap callers gglink_heatmaps.R:1085/1096 and ggnetview_modularity_heatmaps.R:645/673 invoke only the two unseeded helpers, with no surrounding set.seed; the seed=1115 at ggnetview_modularity_heatmaps.R:821 is p

### H5. `gglink_heatmaps (spec-env correlation links)` — spec-env correlation ignores user-selected cor.use and cor.method
- **单元/类别**: environment-heatmaps-mantel · correctness
- **证据**: R/gglink_heatmaps.R:1034 calls psych::corr.test(spec_list[[j]], env_list[[p]]) with no use=/method= arguments, so it silently uses psych defaults (use='pairwise', method='pearson') regardless of the cor.use/cor.method the user passed. The env-env tiles (lines 771 etc.) and the modularity counterpart (R/ggnetview_modularity_heatmaps.R:580) DO pass use=cor.use, method=cor.method. Result: with cor.method='spearman'/'kendall' the heatmap tiles use the requested method but the link r/p are Pearson -- inconsistent and wrong.
- **建议修复**: Change line 1034 to psych::corr.test(spec_list[[j]], env_list[[p]], use = cor.use, method = cor.method) to match the env-env and modularity paths.
- **验证**: Confirmed via source inspection plus a proof of the underlying mechanism. R/gglink_heatmaps.R:1034 calls psych::corr.test(spec_list[[j]], env_list[[p]]) with NO use=/method= args. The function DOES expose cor.method (line 497, match.arg'd at 549) and cor.use (line 498, match.arg'd at 550) to users, and EVERY other corr.test call in the same file passes them: env-env tiles at 771, 821, 871, 922 and spec-self at 1171 all use `use = cor.use, method = cor.method`. Line 1034 is the sole exception. The modularity counterpart R/ggnetview_modularity_heatmaps.R:580 does the equivalent spec-env call WIT

## MEDIUM (6)

### M1. `get_subgraph` — Character (non-factor) Modularity column yields a silently EMPTY sub_graph_all instead of error or correct result
- **单元/类别**: inspection-and-subgraphs · robustness
- **证据**: R/get_subgraph.R:26-30 computes module_name via `dplyr::pull(Modularity) %>% levels()`. If Modularity is a character column (not a factor) — which happens after any igraph round-trip that strips the factor type, the exact reason the Shiny adapter R/app_graph_inspect.R:17-34 exists — levels() returns NULL. Then names(module_list)<-NULL and names(id_list)<-NULL (lines 38,46), and the loop `for (i in module_name)` at line 51 iterates over NULL and NEVER executes, so sub_graph stays an empty list(). Verified in R: with character Modularity, module_name is NULL and the per-module subgraph list is empty; stat_module also gets numeric rownames 1,2,3 instead of real module names. The function returns success with sub_graph_all = list() and no warning.
- **建议修复**: Derive module names robustly: `module_name <- sort(unique(as.character(dplyr::pull(... Modularity))))` instead of levels(); or coerce Modularity to factor at the top of get_subgraph. Also add a guard: if Modularity column is missing, stop() with a clear message rather than letting dplyr::pull error obscurely.
- **验证**: CODE CONFIRMED: R/get_subgraph.R lines 26-30 compute module_name via `dplyr::pull(Modularity) %>% levels()`; lines 38/46 assign names(module_list)/names(id_list) <- module_name; line 51 loops `for (i in module_name)`. Exactly as the evidence states. The Shiny adapter R/app_graph_inspect.R:17-34 (normalize_graph_modularity_factor) coerces Modularity to factor before calling get_subgraph (line 66), confirming maintainers know igraph round-trips strip the factor type. get_subgraph is exported (NAMESPACE:34).

REPRODUCED (mechanism): tidygraph is not installed in this env, so I could not run get_s

### M2. `get_node_centrality` — NA edge weights bypass the non-positive guard and produce a raw C-level igraph ERROR
- **单元/类别**: topology-centrality-ivi · robustness
- **证据**: R/get_node_centrality.R:132 guards with `if (any(raw_w <= 0, na.rm = TRUE))`. The `na.rm = TRUE` means NA weights do NOT trigger the fallback; execution proceeds to L139 `w <- 1/raw_w`, yielding NA entries. igraph then aborts with a scary C error, e.g. 'At vendor/cigraph/src/centrality/betweenness.c:439 : Weight vector must not contain NaN values. Invalid value' (reproduced directly). Via the Shiny adapter this is caught by safe_call and turned into a clean app_failure, but a direct API caller of get_node_centrality() gets the raw C error rather than a graceful guard/warning.
- **建议修复**: Extend the guard to also detect NA: `if (any(!is.finite(raw_w)) || any(raw_w <= 0, na.rm = TRUE)) { warning(...); w <- NULL }` so non-finite/NA weights fall back to the unweighted computation with a clear message instead of an opaque C abort.
- **验证**: CONFIRMED. Verified against both the working-tree source and the installed package (they match).

Code check (R/get_node_centrality.R:128-148): The guard at L132 is `if (any(raw_w <= 0, na.rm = TRUE))`. The `na.rm = TRUE` makes the guard ignore NA entries, so an NA weight does NOT trigger the warning/fallback. Execution proceeds to L139 `w <- 1 / raw_w`, producing an NA (NaN) in the weight vector, which is then passed to igraph::betweenness/closeness/etc. (L160 onward).

Reproduced directly via /Library/Frameworks/R.framework R (the lib where ggNetView is installed). Built a tbl_graph with edg

### M3. `safe_zipi` — NA / out-of-range thresholds are forwarded unvalidated, silently yielding all-NA (unclassified) node roles with no error
- **单元/类别**: zipi · robustness
- **证据**: R/app_topology_adapters.R:122-155 forwards zi_threshold/pi_threshold straight to ggnetview_zipi with no validation. ggnetview_zipi.R:232-235 compares within/among connectivities against the thresholds; if a threshold is NA (e.g. the user clears the numericInput in mod_zipi_results.R:11-12, which permits an empty value), every comparison yields NA and zi_pi$type stays NA_character_ for all rows. The result table is returned ok=TRUE with an entirely unclassified 'type' column and no warning — the user sees a silent empty classification rather than a clean error. A negative or >1 pi_threshold similarly produces nonsensical (but non-erroring) classifications.
- **建议修复**: In safe_zipi (or ggnetview_zipi) validate: is.finite(zi_threshold) && is.finite(pi_threshold) && pi_threshold within [0,1], else return app_failure/stop with a clear message; in the module use shiny::req() on the threshold inputs.
- **验证**: CONFIRMED. The cited code matches the evidence exactly.

(1) safe_zipi at /Users/liuyue/Desktop/R/R_Package_development/ggNetView_Shiny/R/app_topology_adapters.R:122-163 forwards zi_threshold/pi_threshold directly to ggnetview_zipi with zero validation (only checks graph is igraph and that module/degree columns exist).

(2) ggnetview_zipi.R validates nodes_bulk, z_bulk_mat, modularity_col, degree_col, mod/deg NAs (lines 118-169) but performs NO finite/NA/range check on zi_threshold or pi_threshold. Lines 232-235 use them directly in `< zi_threshold` / `>= pi_threshold` comparisons; type is see

### M4. `get_network_perturbation (.metrics)` — Natural_connectivity overflows to Inf on large/dense survivor subgraphs
- **单元/类别**: perturbation · robustness
- **证据**: R/get_network_perturbation.R:142-144 computes nat <- log(mean(exp(ev))) directly. exp(ev) overflows to Inf once the largest adjacency eigenvalue exceeds ~709 (double-precision exp limit). Verified empirically: a 400-node gnp(0.5) graph gives max ev ~200 (fine) but denser/larger survivor graphs push lambda_max past 709, making exp(ev)=Inf and nat=Inf. exp(710) already returns Inf.
- **建议修复**: Use the numerically stable log-sum-exp form: lam <- max(ev); nat <- lam + log(mean(exp(ev - lam))). This is mathematically identical to log(mean(exp(ev))) but never overflows.
- **验证**: CODE CONFIRMED: R/get_network_perturbation.R:142-144 in the .metrics() helper literally reads `A <- as.matrix(igraph::as_adjacency_matrix(sub, sparse=FALSE)); ev <- eigen(A, symmetric=TRUE, only.values=TRUE)$values; nat <- log(mean(exp(ev)))`. This helper is invoked on the full graph at fraction=0 (lines 209, 242) and on every survivor subgraph for module/targeted/random strategies.

REPRO (math at the exact line-144 logic, run via Rscript with igraph):
- n=600 p=0.8: lambda_max=479.7, naive log(mean(exp(ev)))=473.28 (still finite).
- n=1000 p=0.80: lambda_max=800.1, naive=Inf, stable LSE=793.

### M5. `get_network_perturbation` — Schneider R-index is grid-dependent and biased by non-terminating / 0-inclusive fraction grids
- **单元/类别**: perturbation · correctness
- **证据**: R/get_network_perturbation.R:263-265 computes r_index <- mean(lcc$value) over the LCC values at fractions c(0, fractions). Two problems: (1) the grid includes fraction 0 (LCC always = 1), and a plain mean is not invariant to grid spacing, so the same network yields different R for different `fractions`/step. (2) The Shiny adapter passes fractions = seq(step,1,by=step) (R/app_perturbation_adapters.R:93); for steps that do not divide 1 (e.g. 0.07 -> ends 0.98, 0.3 -> ends 0.9) the grid never reaches fraction 1.0, so the fully-collapsed LCC=0 endpoint is omitted and mean() overestimates robustness. Standard Schneider R = (1/N) sum_{Q=1}^{N} s(Q) is an evenly-spaced sum over all removal steps up to full collapse.
- **建议修复**: Either force the grid to terminate at 1.0 (append 1 if absent) and use trapezoidal AUC over the [0,1] fraction axis (sum(diff(fraction)*(head(lcc,-1)+tail(lcc,-1))/2)), which is grid-spacing invariant, or compute the mean only over a fixed evenly-spaced grid independent of the user's plotting fractions.
- **验证**: Code confirmed at R/get_network_perturbation.R:262-267: `r_index <- mean(lcc$value)` taken over the LCC values at fractions `c(0, fractions)` (grid built at lines 223/239), with fraction 0 (LCC=1) always included. Adapter confirmed at R/app_perturbation_adapters.R:76,93: it passes `fractions = seq(step, 1, by = step)` where `step = normalize_fraction_step(params$fraction_step)`, so a user-chosen fraction step that does not divide 1 truncates the grid before fraction 1.0.

I could not `library(ggNetView)` (not installed under that name) nor `pkgload::load_all` (missing deps: tidygraph, ggraph, 

### M6. `safe_environment_link / safe_environment_heatmap / safe_module_environment_heatmap` — Shiny adapters cannot make Mantel runs reproducible because no seed is threaded
- **单元/类别**: environment-heatmaps-mantel · determinism
- **证据**: R/app_compare_environment.R:1094-1108, :1168-1190, :1263-1287 build call_args and forward permutations/mantel params, but there is no seed key in defaults and the underlying gglink_heatmaps/ggnetview_modularity_heatmaps do not accept a mantel seed (see critical finding). filter_function_call_args would also drop any seed the UI tried to pass. So the Shiny 'Compare Environment' Mantel outputs change on every recompute for identical inputs.
- **建议修复**: Once the core functions accept a mantel seed, add seed (e.g. 1115) to the defaults lists in these safe_* wrappers and ensure filter_function_call_args keeps it; expose a seed input in mod_visual_lab for the environment workflow.
- **验证**: CONFIRMED. Verified both the code path and reproduced the non-determinism.

Code verification:
- R/app_compare_environment.R defaults lists at :1094-1103 (safe_environment_link), :1168-1183 (safe_environment_heatmap), :1263-1280 (safe_module_environment_heatmap) contain NO `seed` key. They forward `permutations`/`mantel_kind`/`cor.method` etc. via modifyList + filter_function_call_args.
- filter_function_call_args (R/app_compare_environment.R:1038-1054) keeps only args whose names are in the target function's formals (line 1041: `call_args[names(call_args) %in% allowed_names]`). So any seed th

## LOW (14)

### L1. `module_layout / module_layout3 / get_neighbors (R/get_geo_neighbors.R)` — Layout RNG (sample/rnorm) relies entirely on caller's set.seed; helper-level seed is disabled
- **单元/类别**: building-and-layout · determinism
- **证据**: All helper-level set.seed(seed) calls are commented out (R/get_geo_neighbors.R:21,129,253,483,796) and the seed= argument is commented out in every dispatch from R/ggnetview.R:479,497,534,547. The helpers consume the RNG stream via sample.int (R/get_geo_neighbors.R:33), sample (lines 301,303,310,314,578-580) and rnorm jitter (212-213,421-422,834-835). Determinism therefore hinges solely on set.seed(seed) at R/ggnetview.R:345. This holds for a single top-level ggNetView() call, but any code path that invokes module_layout* directly (or that calls another RNG-consuming step between set.seed and the layout) is no longer reproducible. The adjacent retry loop also advances the stream on each failed attempt, so the chosen coordinates depend on how many retries occurred.
- **建议修复**: Re-enable a local, scoped seed in each layout helper (e.g. withr::with_seed(seed, ...)) and thread seed through the dispatch calls in ggNetView, OR document that these helpers are not standalone-deterministic and must be wrapped by ggNetView's set.seed.
- **验证**: Verified every cited fact in the real code:
- Helper-level set.seed(seed) is commented out at R/get_geo_neighbors.R:21, 129, 253, 483, 796 (confirmed via sed).
- The `seed=` argument is commented out of BOTH the helper signatures (e.g. module_layout3 at line 471-480 has `# seed = seed` in its arglist — the parameter no longer exists) AND every dispatch in R/ggnetview.R (479, 497, 534, 547).
- Helpers consume RNG: sample.int at line 33; sample at 301/303/310/314/580; rnorm jitter at 212-213/421-422/834-835. Confirmed.
- Top-level ggNetView() seeds the stream at R/ggnetview.R:345 (set.seed(seed)

### L2. `get_graph_adjacency` — Adjacency matrix silently drops edge weights despite documentation promising them
- **单元/类别**: inspection-and-subgraphs · correctness
- **证据**: R/get_graph_adjacency.R:24 calls igraph::as_adjacency_matrix(...) with NO `attr=` argument, so igraph returns a 0/1 (binary/multiplicity) matrix. But the roxygen docs at R/get_graph_adjacency.R:5-6 state: 'Non-zero entries indicate edges; values correspond to edge weights when the graph is weighted.' Verified in igraph 2.1.4: as_adjacency_matrix(g) returns 0/1, while as_adjacency_matrix(g, attr='weight') returns the weights. ggNetView graphs ALWAYS carry a numeric `weight` edge attribute (R/build_graph_from_mat.R:256-257 set E(g)$weight = abs(correlation)), so the network is genuinely weighted and the returned adjacency loses all weight/correlation magnitude information.
- **建议修复**: Either (a) honor the docs by passing attr='weight' when a weight edge attribute exists: `igraph::as_adjacency_matrix(g, attr = if ('weight' %in% igraph::edge_attr_names(ig)) 'weight' else NULL, sparse = FALSE)`, or (b) fix the documentation to state the matrix is binary (presence/absence) since the primary consumer ggnetview_zipi expects unweighted within-module degree. Pick one so behavior and docs agree.
- **验证**: CONFIRMED. Code at /Users/liuyue/Desktop/R/R_Package_development/ggNetView_Shiny/R/get_graph_adjacency.R:24 calls `igraph::as_adjacency_matrix(tidygraph::as.igraph(graph_obj))` with NO attr= argument. Roxygen docs at lines 4-5 promise: "values correspond to edge weights when the graph is weighted."

REPRODUCED (using R.framework copy where ggNetView is installed; miniforge R lacked it):
  obj <- build_graph_from_df(ppi_example$ppi, ppi_example$annotation)
  - edge_attr_names: weight, correlation (graph IS weighted; sample weights ~45.2, 50.6, 78.1)
  - get_graph_adjacency(obj): unique values =

### L3. `get_subgraph` — Missing Modularity column produces an obscure dplyr error, and invalid select_module returns an empty graph silently
- **单元/类别**: inspection-and-subgraphs · robustness
- **证据**: R/get_subgraph.R:29 `dplyr::pull(Modularity)` throws an unhelpful 'object Modularity not found / column doesn't exist' error if the node table lacks a Modularity column (no upfront validation). Separately, line 71-73: when select_module names a non-existent module, `tidygraph::filter(as.character(Modularity) %in% select_module)` returns a 0-node tbl_graph rather than an error or warning, so sub_graph_select is a silently empty graph. The Shiny adapter safe_module_subgraph (R/app_graph_inspect.R:56-72) forwards select_module blindly without validating it against graph_module_choices().
- **建议修复**: At the top of get_subgraph, validate: if (!'Modularity' %in% names(node tibble)) stop('graph_obj must have a Modularity node column.'). After filtering, if igraph::gorder(graph_select)==0L, emit a warning that the requested module produced no nodes. In safe_module_subgraph, validate select_module against graph_module_choices(graph) before forwarding.
- **验证**: Confirmed by reading the code and reproducing both behaviors in R (Framework R 4.5-arm64, sourcing R/get_subgraph.R directly since ggNetView is not installed under the project libpath but tidygraph/dplyr/purrr/igraph are available).

CODE CITATIONS VERIFIED:
- R/get_subgraph.R:26-30: module_name is derived via `... %>% dplyr::pull(Modularity) %>% levels()` with no prior check that a Modularity column exists. Matches evidence.
- R/get_subgraph.R:71-73: `if (!is.null(select_module)) graph_select <- obj %>% tidygraph::filter(as.character(Modularity) %in% select_module)` — no post-filter check on 

### L4. `get_node_centrality` — Closeness emits NaN for isolated/singleton-component nodes on disconnected graphs; passed through verbatim into the node column
- **单元/类别**: topology-centrality-ivi · robustness
- **证据**: R/get_node_centrality.R:163-166 calls igraph::closeness(ig, mode='all') and stores the result verbatim. On a disconnected graph an isolated vertex gets NaN (reproduced: node 'iso' -> NaN). The docstring (L20-23) acknowledges this, so it is documented behavior, but the resulting node table then carries NaN values that can silently break downstream sorting/binning (e.g. quantile cut, ranking, colour scales) without warning. Harmonic centrality (already offered, L209-212) is the disconnected-safe alternative but Closeness is still computed by default.
- **建议修复**: Either coerce non-finite Closeness to NA with a one-line note, or document more prominently that Closeness is NaN-prone on disconnected graphs and recommend Harmonic; optionally emit a single warning when the graph is disconnected and Closeness is requested.
- **验证**: Confirmed by reading code and reproducing. R/get_node_centrality.R:163-166 calls igraph::closeness(ig, mode="all", weights=w) and stores the result verbatim via mutate() (L234-236), with no NaN/non-finite coercion. Docstring L20-23 acknowledges the NaN.

Reproduced (R at /usr/local/bin/Rscript, with ggNetView+tidygraph+igraph installed): built a disconnected graph with two components (a-b-c, d-e) plus an isolated vertex 'iso', then called get_node_centrality(tg, measures=c("Closeness","Harmonic")). Output node table:
  name Closeness Harmonic
1 a     0.333    1.5
2 b     0.500    2.0
3 d     1

### L5. `safe_zipi` — adjacency_from_graph fallback throws 'no such edge attribute' when the graph has no weight attribute
- **单元/类别**: topology-centrality-ivi · robustness
- **证据**: R/app_topology_adapters.R:19 fallback does `igraph::as_adjacency_matrix(graph, attr='weight', sparse=FALSE)`, which errors with 'no such edge attribute' on a graph lacking a `weight` edge attribute (reproduced). adjacency_from_graph is called at R/app_topology_adapters.R:133 inside safe_zipi; if the primary resolve_ggnetview_function('get_graph_adjacency') path is unavailable and the graph is unweighted, this errors. It is invoked before the safe_call wrapper (L133 vs the wrapped fn at L145-155), so this particular error is NOT caught by safe_call and would propagate out of safe_zipi unguarded.
- **建议修复**: In the fallback (L19), guard the attr: `attr <- if ('weight' %in% igraph::edge_attr_names(graph)) 'weight' else NULL` before building the adjacency matrix, or wrap the adjacency_from_graph(graph) call at L133 in safe_call so failures become a clean app_failure.
- **验证**: Code matches the claim exactly. R/app_topology_adapters.R:19 fallback is `as.matrix(igraph::as_adjacency_matrix(graph, attr = "weight", sparse = FALSE))`. It is called at L133 (`adjacency <- adjacency_from_graph(graph)`) OUTSIDE the safe_call wrapper, which only wraps fn(...) at L145-155, so an error there propagates out of safe_zipi unguarded.

Reproduced the underlying error directly: `igraph::as_adjacency_matrix(make_ring(5), attr="weight", sparse=FALSE)` -> "no such edge attribute" (edge_attr_names showed no weight attr).

Reproduced the full chain: sourced app_validation.R + app_adapters.

### L6. `ggnetview_zipi` — Participation coefficient (Pi) mixes matrix-derived k_is with an independent node-table k_tot, producing wrong/negative Pi when they diverge
- **单元/类别**: zipi · correctness
- **证据**: R/ggnetview_zipi.R:203-208. sum_kis2 = rowSums(kis_mat^2) is computed from the binarized adjacency A (k_is = edges from node i into module s), but k_tot = as.numeric(deg) is taken from the user-supplied degree_col (nodes_bulk), NOT from A. The standard Guimera-Amaral formula P_i = 1 - sum_s (k_is/k_i)^2 requires k_i == sum_s k_is. If the degree column disagrees with the matrix (weighted Strength passed as degree_col, a subsetted/filtered matrix, a directed/asymmetric matrix, or a graph whose Degree attr was computed before edge pruning), sum_kis2 / k_tot^2 can exceed 1 and Pi goes negative — an impossible value for a participation coefficient — with no guard. In the standard pipeline Degree = centrality_degree(mode="out") == rowSums(A)-diag, so the happy path is correct, but the function publicly accepts an arbitrary degree_col and the adapter (safe_zipi) selects degree_col by name without checking consistency.
- **建议修复**: Derive k_tot from the same binarized matrix A used for k_is (k_tot = rowSums(A) - 1, i.e. rowSums(kis_mat)), instead of the node-table degree column; or at minimum clamp Pi to [0,1] and warn when sum_kis2 > k_tot^2 (indicating a degree/matrix mismatch).
- **验证**: Code confirmed at R/ggnetview_zipi.R:203-208: sum_kis2 = rowSums(kis_mat^2) is derived from the binarized adjacency A, while k_tot = as.numeric(deg) comes from nodes_bulk[[degree_col]]. Lines 205-208 compute P[nz] = 1 - sum_kis2/k_tot^2 with no consistency check and no clamp/warn (only guards are line 201 kis_mat[kis_mat<0]<-0 and line 206 k_tot==0). 

REPRO (sourced R/ggnetview_zipi.R directly; package not installed but file is self-contained): passing a degree column that disagrees with the matrix produced among_module_connectivities = -4.00 (n1) and -3.00 (n2) — impossible negative particip

### L7. `mod_zipi_results_server/unique_output_name` — Registry result name uses sample.int() without a seed, violating the package's deterministic-output claim
- **单元/类别**: zipi · determinism
- **证据**: inst/app/modules/mod_zipi_results.R:36 — suffix <- paste0(format(Sys.time(), ...), '_', sprintf('%04d', sample.int(9999, 1))). sample.int is an unseeded RNG draw, so the saved Zi-Pi result's registry name is non-reproducible across runs. This is cosmetic (it only affects the result's label, not the Zi/Pi values, which are fully deterministic), but it is unseeded RNG in the headline 'reproducible & deterministic' workflow.
- **建议修复**: Use a deterministic disambiguator (e.g. an incrementing counter, or include microseconds / a content hash) instead of sample.int(); if randomness is desired, draw from a seeded stream.
- **验证**: Confirmed by code + repro. inst/app/modules/mod_zipi_results.R:36 reads exactly: suffix <- paste0(format(Sys.time(), "%Y%m%d_%H%M%S"), "_", sprintf("%04d", sample.int(9999, 1))), and unique_output_name() is used at line 81 (result_name) to label the registry entry. sample.int(9999, 1) is an unseeded RNG draw. I ran the function twice in R; same second produced net_zipi_20260615_100757_3623 vs net_zipi_20260615_100757_3081 (identical=FALSE), reproducing the non-determinism. The package explicitly claims this property: DESCRIPTION Title = "Reproducible and Deterministic Network Analysis and Visu

### L8. `get_network_perturbation` — robustness_index (R_index) is emitted for module/manual strategies where it is meaningless
- **单元/类别**: perturbation · correctness
- **证据**: R/get_network_perturbation.R:262-267 always computes r_index <- mean(lcc$value). For strategy 'module'/'manual' the curve has only two fraction points (0 and the removed fraction; lines 212-219), so R_index is just the mean of before/after LCC (e.g. mean(1.0,0.3)=0.65) -- not a Schneider robustness/attack-curve area, but it is returned in robustness_index with the same column name and surfaced by the adapter (R/app_perturbation_adapters.R:107) as if comparable to random/targeted R.
- **建议修复**: Return R_index only for random/targeted strategies (set NA or omit for module/manual), or rename/document the before-after column so it is not conflated with the area-under-curve Schneider index.
- **验证**: Confirmed by code inspection AND reproduction. Code: R/get_network_perturbation.R:262-267 computes `r_index <- mean(lcc$value)` unconditionally for every strategy and returns it in a `robustness_index` data.frame with column `R_index` regardless of strategy. Lines 191-219 build the module/manual curve with exactly two fraction points (0 and length(rm_names)/n0), value_sd/se = NA. Lines 221-260 build random/targeted curves over the full `c(0, fractions)` grid (~21 points). The adapter R/app_perturbation_adapters.R:107 passes `robustness_index` through unchanged for all strategies. The docstring

### L9. `safe_network_perturbation` — Adapter does not validate that fraction_step divides the [0,1] range, propagating a truncated attack curve
- **单元/类别**: perturbation · robustness
- **证据**: R/app_perturbation_adapters.R:76,93 normalize_fraction_step only checks 0<value<1 then builds fractions=seq(step,1,by=step). It never ensures the sequence reaches 1.0, so non-dividing steps (0.3, 0.07, 0.4) silently produce a curve that stops short of full node removal, feeding the biased R-index in get_network_perturbation (see related finding).
- **建议修复**: In normalize_fraction_step or when building fractions, append 1 if the last element < 1 (fractions <- unique(c(seq(step,1,by=step), 1))), or snap step to a divisor of 1.
- **验证**: CONFIRMED. Code matches the evidence exactly.

R/app_perturbation_adapters.R:51-57 normalize_fraction_step only validates 0<value<1 (and finiteness), passing any in-range value through unchanged. Line 93 then builds fractions = seq(step, 1, by = step). For a step that does not evenly divide 1.0, this sequence stops short of 1.0.

Reproduced the truncation directly:
- seq(0.3,1,by=0.3) = 0.3, 0.6, 0.9 (max 0.9, never 1.0)
- seq(0.4,1,by=0.4) = 0.4, 0.8 (max 0.8)
- seq(0.07,1,by=0.07) max = 0.98
- default seq(0.05,1,by=0.05) max = 1.0 (works correctly)

Reachability confirmed: inst/app/modules/m

### L10. `ggNetView_multi_link` — scale_groups=FALSE jitter block is nested inside the per-group anchoring loop and re-loops all groups, applying jitter N times
- **单元/类别**: multi-network-compare · correctness
- **证据**: R/ggNetView_multi_link.R:1130-1175. The `for (i in seq_along(names(graph_info)))` opens at line 1131; inside it the `if (isTRUE(jitter))` at line 1152 contains a SECOND `for (i in ...)` at line 1153 that iterates every group again. Because the if/inner-for sit inside the outer loop (closing braces 1172-1174), when jitter=TRUE every group is jittered once per outer iteration -> jitter applied N times (N = number of groups), compounding the noise and overwriting the `i` index. Contrast with the scale_groups=TRUE branch (lines 1106-1112) where the jitter loop is correctly OUTSIDE the anchoring loop.
- **建议修复**: Move the `if (isTRUE(jitter)) { for (i ...) {...} }` block out of the outer anchoring `for` loop (mirror the scale_groups=TRUE structure at lines 1106-1128) so jitter is applied exactly once per group.
- **验证**: CONFIRMED structurally and by reproduction. File: /Users/liuyue/Desktop/R/R_Package_development/ggNetView_Shiny/R/ggNetView_multi_link.R. The scale_groups=FALSE branch (lines 1130-1175) is exactly as described: line 1131 opens `for (i in seq_along(names(graph_info)))` (anchoring loop); lines 1132-1149 anchor group i; line 1152 `if (isTRUE(jitter))` and line 1153 inner `for (i in ...)` sit INSIDE that outer loop. Brace audit (grep of {/} within 1130-1175): inner for closes at 1172, inner if at 1173, and the OUTER for closes at 1174 (after the jitter block) - so the jitter loop is genuinely nest

### L11. `ggNetView_multi_link` — Module-link group matching uses group name as a regex pattern (str_detect), mis-aligning groups whose names share substrings or contain regex metacharacters
- **单元/类别**: multi-network-compare · correctness
- **证据**: R/ggNetView_multi_link.R:1736 `dplyr::filter(stringr::str_detect(Group, pattern = names(graph_list)[index]))`. `Group` here is the compound key like 'WT_to_KO'; matching the bare group name as a regex means (a) a group named e.g. 'WT' will also match 'WT2' or 'NWT' (substring), and (b) names containing '.', '(', '+', '*' are interpreted as regex. Underscores are sanitized to hyphens earlier (lines 432-449) but dots/parens/plus are not.
- **建议修复**: Match on the parsed GroupA/GroupB columns with exact equality (the code already computes GroupA/GroupB via tidyr::separate) instead of str_detect on the compound Group string, or use fixed = TRUE / `\\b` anchoring with escaped names.
- **验证**: Code matches evidence: R/ggNetView_multi_link.R:1736 is verbatim `dplyr::filter(stringr::str_detect(Group, pattern = names(graph_list)[index]))`. `Group` is the compound key built as paste(GroupA,"to",GroupB,sep="_") and parsed back into GroupA/GroupB at line 904.

CLAIM'S PRIMARY MECHANISM IS REFUTED. The str_detect on line 1736 is immediately followed at line 1737 by an EXACT-equality filter `GroupA == names(graph_list)[index] | GroupB == names(graph_list)[index]`. I reproduced the claim's "Ctrl"/"Ctrl2" / "A.1" over-match scenario in R: with groups "A.1" and "AX1", str_detect(Group,"A.1") i

### L12. `safe_multi_network_compare` — Adapter validates inputs as 'igraph' but forwards them as graph_obj_list which ggNetView_multi_link requires to be 'tbl_graph'
- **单元/类别**: multi-network-compare · robustness
- **证据**: R/app_compare_environment.R:48 checks `vapply(graphs, inherits, what = 'igraph')`, then line 76 passes `graph_obj_list = graphs`. R/ggNetView_multi_link.R:413 rejects any element that is not a `tbl_graph` via stop(). A plain igraph passes the adapter guard (tbl_graph extends igraph, but not vice-versa), then is rejected downstream. The registry feeds `item$data` directly (mod_compare_environment.R:399) with no coercion to tbl_graph before this call.
- **建议修复**: In safe_multi_network_compare, coerce each graph with tidygraph::as_tbl_graph() before forwarding (and/or validate `inherits(.,'tbl_graph')` to give a precise message), matching the requirement enforced by ggNetView_multi_link.
- **验证**: CODE CONFIRMED. R/app_compare_environment.R:48 guards inputs with `vapply(graphs, inherits, logical(1), what="igraph")`, then line 76 forwards `graph_obj_list = graphs`. R/ggNetView_multi_link.R:413 rejects any non-`tbl_graph` element via stop(). mod_compare_environment.R:399 feeds `item$data` with no coercion. So the guard/downstream contract is genuinely inconsistent (tbl_graph extends igraph but not vice versa).

REPRODUCED (framework R 4.5-arm64, ggNetView 0.1.0): two plain igraph objects -> guard `all(vapply(...,what="igraph"))` returns TRUE while `inherits(g1,"tbl_graph")` is FALSE; call

### L13. `gglink_heatmaps (env-env tile loop) / ggnetview_modularity_heatmaps (env-env loop)` — No guard for tiny (<4-row) or constant-column env blocks before psych::corr.test -> scary warning + NaN tiles
- **单元/类别**: environment-heatmaps-mantel · robustness
- **证据**: R/gglink_heatmaps.R:771,821,871,922 and R/ggnetview_modularity_heatmaps.R:709 call psych::corr.test(env_list[[i]], ...) with no check that the block has >=4 complete rows or non-zero variance. psych::corr.test emits 'Number of subjects must be greater than 3' (the OBSERVED warning) and returns NaN r/p for <4 samples or constant columns. The NaN then flows into geom_tile fill and p_signif unguarded.
- **建议修复**: Before each corr.test, validate the block: drop/flag zero-variance columns (sd==0) and require nrow(complete.cases) >= 4; otherwise emit a clean stop()/message and skip the quadrant rather than letting psych warn and produce NaN.
- **验证**: CODE: Confirmed the cited lines. R/gglink_heatmaps.R:771,821,871,922 and R/ggnetview_modularity_heatmaps.R:709 all call psych::corr.test(env_list[[i]], use=cor.use, method=cor.method) with NO guard for row count or column variance. No suppressWarnings wraps these calls in the package or Shiny layer (verified by grep).

REPRO (ran on framework R 4.5.1 where ggNetView+psych are installed; default miniforge Rscript lacked them): Built documented-shape data (56 env cols / 30 spec cols, env_select 4 blocks, spec_select 2 blocks) which runs cleanly. Injecting bad conditions:
- 3-row env: gglink_heat

### L14. `mantel_pairwise / mantel_block_vs_col` — n=3 sample guard allows a degenerate Mantel test (only 6 permutations)
- **单元/类别**: environment-heatmaps-mantel · correctness
- **证据**: R/mantel_utils.R:79 (length(s) < 3L) and R/mantel_utils.R:292 (nrow(spec_df) < 3L) permit exactly 3 samples. With 3 objects a distance matrix has only 3 entries and at most 3!=6 distinct permutations, so vegan::mantel's permutation p-value is coarse/degenerate (minimum attainable p ~ 0.167) and effectively meaningless, yet it is reported without warning. Standard practice (and the function docstrings referencing the >3-subjects requirement) implies n must exceed 3.
- **建议修复**: Raise the guard to require nrow >= 4 (or warn when nrow <= 4 that the permutation distribution is degenerate), and/or cap/round permutations to the achievable count and surface a message.
- **验证**: Code confirmed: R/mantel_utils.R:79 (`if (length(s) < 3L) next`) in mantel_pairwise and R/mantel_utils.R:292 (`if (nrow(spec_df) < 3L || length(env_cols) < 1L)`) in mantel_block_vs_col both permit exactly n=3. Reproduced (vegan installed; ggNetView not installed, so I sourced R/mantel_utils.R directly):

mantel_block_vs_col(3x5 spec, 3x2 env, permutations=999) returned ID/Type/Correlation/Pvalue with Pvalue = 0.5 and 0.333, and vegan printed "'nperm' >= set of all permutations: complete enumeration" / "Set of permutations < 'minperm'. Generating entire set." -> only the 3!=6 enumeration is use
