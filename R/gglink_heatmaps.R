#' Multi-orientation species-environment correlation / Mantel heatmap
#'
#' @description
#' Render a "central species network(s)" surrounded by up to four
#' environmental-correlation heatmap quadrants, with curved or straight link
#' segments connecting each spec node (or each spec block) to each env
#' variable. Supports both Pearson/Spearman/Kendall correlation and
#' \code{vegan::mantel} tests, and exposes a unified Mantel API
#' (\code{mantel_kind = "block_vs_col" | "col_vs_col"}) shared with
#' \code{\link{ggnetview_modularity_heatmaps}}. Link line colour and width
#' are user-configurable through expression strings (\code{link_color_by},
#' \code{link_width_by}); non-significant links are rendered as a separate
#' flat-grey background layer for visual context.
#'
#' @details
#' \strong{Layered geometry.} The plot is built in three nested layers:
#' \enumerate{
#'   \item \strong{Single network geometry} (per spec block) controlled by
#'     \code{spec_layout} / \code{spec_orientation} -- how nodes inside one
#'     block are arranged.
#'   \item \strong{Group geometry} (across spec blocks) controlled by
#'     \code{group_layout} / \code{group_angle} / \code{group_arc_angle} /
#'     \code{anchor_dist} -- how the per-block anchors are scattered on the
#'     canvas.
#'   \item \strong{Heatmap geometry} (around the centre) controlled by
#'     \code{orientation} / \code{distance} / \code{HeatmapScale} -- where the
#'     four env heatmap quadrants sit relative to the central group.
#' }
#'
#' \strong{Two collapse modes for \code{spec_select}.}
#' \itemize{
#'   \item \code{spec_collapse = FALSE} (default): each block is drawn as a
#'     small network and each spec column has its own node. Link sources are
#'     per-column nodes (correlation / col_vs_col mantel) or block centroids
#'     (block_vs_col mantel).
#'   \item \code{spec_collapse = TRUE}: each block becomes ONE labelled
#'     point at its anchor position. Pairs naturally with
#'     \code{relation_method = "mantel"} + \code{mantel_kind = "block_vs_col"},
#'     where one block = one statistical unit.
#' }
#'
#' \strong{Mantel API.} Two algorithms exposed via \code{mantel_kind} and
#' shared with \code{\link{ggnetview_modularity_heatmaps}}:
#' \itemize{
#'   \item \code{"block_vs_col"} (default, ecological standard, linkET / ggcor
#'     style): the whole spec block becomes ONE community-distance matrix
#'     (\code{vegan::vegdist} with \code{spec_dist_method}); each env column
#'     becomes its own distance matrix (\code{env_dist_method}); one Mantel
#'     test per (spec_block, env_col). See \code{\link{mantel_block_vs_col}}.
#'   \item \code{"col_vs_col"} (legacy, kept for sensitivity / backwards
#'     compatibility): each spec column and each env column is reduced to a
#'     single-column distance matrix; the resulting "Mantel" is mathematically
#'     close to a rank correlation. See \code{\link{mantel_pairwise}}.
#' }
#'
#' \strong{Angle inputs.} \code{group_angle} and \code{group_arc_angle}
#' auto-detect their unit from the magnitude: \code{|x| <= 2*pi} is taken as
#' radians, \code{|x| > 2*pi} as degrees. Use \code{\link{deg}} to force a
#' small value to mean degrees (e.g. \code{deg(5)} = 5 degrees).
#'
#' \strong{Link line styling.} Spec-env links are rendered in two stacked
#' layers controlled by \code{sig_threshold}:
#' \itemize{
#'   \item \strong{Significant layer} (\code{Pvalue <= sig_threshold}):
#'     colour and width are mapped from user-supplied expressions
#'     \code{link_color_by} and \code{link_width_by} (each a string like
#'     \code{"Correlation"}, \code{"-log10(Pvalue)"}, or
#'     \code{"abs(Correlation)"}). The colour gradient endpoints come from
#'     \code{SigLineColor}; pass \code{SigLineMid} (e.g. \code{"white"}) to
#'     switch to a diverging \code{scale_colour_gradient2(midpoint = 0)},
#'     recommended whenever the mapped variable can be negative (signed
#'     correlations). The width scale range comes from \code{SigLineWidth}.
#'   \item \strong{Non-significant layer} (\code{Pvalue > sig_threshold}):
#'     drawn flat in \code{NonsigLineColor} (default light grey) with line
#'     type \code{NonsigLineType} (default dashed) at the minimum of
#'     \code{SigLineWidth}, purely as visual context. This layer is omitted
#'     when \code{drop_nonsig = TRUE}.
#' }
#' Both layers share \code{SigLineAlpha}. Because non-significant links do
#' not participate in the colour/width scales, the legends only reflect
#' the significant layer.
#'
#' \strong{Available columns for link expressions.} \code{link_color_by}
#' and \code{link_width_by} are evaluated against the link data frame,
#' which contains the spec-env stat columns plus link-source / link-target
#' coordinates: \code{ID}, \code{Type}, \code{Correlation}, \code{Pvalue},
#' \code{p_signif}, \code{spec_block}, \code{env_block}, \code{method},
#' \code{is_sig}, plus \code{x}, \code{y}, \code{x_to}, \code{y_to}. Most
#' users will only build expressions from \code{Correlation} and
#' \code{Pvalue}.
#'
#' @section Data inputs:
#' Matrices and block selectors that drive the whole computation: the env
#' and spec data tables plus the named lists that partition each into
#' visual blocks (heatmap quadrants and central networks).
#'
#' @param env Data frame or matrix. Environmental variables, one column per
#'   factor, one row per sample. Row order must match \code{spec}.
#' @param spec Data frame or matrix. Species abundance / trait data, one
#'   column per species (or taxonomic unit), one row per sample. Row order
#'   must match \code{env}.
#' @param env_select Named list (required). Each element gives the column
#'   indices or names of \code{env} that form one environmental block; each
#'   block becomes one heatmap quadrant. The list length must equal
#'   \code{length(orientation)}. Block names (used for
#'   \code{comparisons_groups}) come from \code{names(env_select)}, e.g.
#'   \code{list(Env01 = 1:14, Env02 = 15:28, Env03 = 29:42, Env04 = 43:56)}.
#' @param spec_select Named list (required). Each element gives the column
#'   indices or names of \code{spec} that form one species block. Each block
#'   is rendered as one central network (or one collapsed point if
#'   \code{spec_collapse = TRUE}). Block names come from
#'   \code{names(spec_select)}, e.g. \code{list(Spec01 = 1:15, Spec02 = 16:30)}.
#'
#' @section Per-block (spec) geometry:
#' Controls how nodes are arranged inside one spec block: the layout shape
#' (e.g. circle vs diamond outline), its base orientation, whether the
#' block is collapsed to a single labelled point, and how its visual
#' radius scales.
#'
#' @param spec_layout Character or character vector (default
#'   \code{"circle_outline"}). Shape of the per-block node layout. Length 1
#'   applies to all blocks; a vector must have length equal to
#'   \code{length(spec_select)} and is matched element-wise. Valid values:
#'   \code{"circle_outline"}, \code{"diamond_outline"},
#'   \code{"rectangle_outline"}, \code{"square_outline"}.
#'   Ignored when \code{spec_collapse = TRUE}.
#' @param spec_orientation Character (default \code{"up"}). Base orientation
#'   passed to the per-block layout function. One of \code{"up"},
#'   \code{"down"}, \code{"left"}, \code{"right"}.
#' @param spec_relation Logical (default \code{TRUE}). Whether to compute
#'   within-block species-species correlations to drive the per-block layout
#'   (e.g. modularity of \code{"circle_outline"}). Set \code{FALSE} for a
#'   geometry-only layout. Ignored when \code{spec_collapse = TRUE}.
#' @param spec_collapse Logical (default \code{FALSE}). If \code{TRUE}, each
#'   block is rendered as ONE labelled point at its anchor position; the
#'   point's label is the block name from \code{names(spec_select)}.
#'   In this mode, \code{spec_layout}, \code{spec_relation},
#'   \code{scale_networks} are all ignored, and link sources are always the
#'   collapsed point regardless of \code{relation_method}. Pairs naturally
#'   with \code{relation_method = "mantel"} +
#'   \code{mantel_kind = "block_vs_col"}.
#' @param r Numeric (default 6). Effective radius of a single central
#'   network (in plot units). When \code{spec_collapse = TRUE} this only
#'   affects how compact a single block looks before being collapsed and is
#'   essentially cosmetic.
#' @param scale_networks Logical (default \code{TRUE}). If \code{TRUE},
#'   normalise each per-block network to the same visual radius
#'   (\code{r}); if \code{FALSE}, \code{r} is the minimum network radius and
#'   larger networks scale proportionally to node count. Ignored when
#'   \code{spec_collapse = TRUE}.
#'
#' @section Across-block (group) geometry:
#' Controls how the per-block anchors are scattered across the figure:
#' the macro arrangement (circle / row / arc / ...), its rotation, the
#' anchor-to-anchor spacing, and any grid dimensions when applicable.
#'
#' @param group_layout Character (default \code{"circle"}). Arrangement of
#'   the per-block anchors when \code{spec_select} has multiple elements.
#'   One of \code{"circle"}, \code{"row"}, \code{"column"},
#'   \code{"square"}, \code{"diamond"}, \code{"triangle"},
#'   \code{"triangle_down"}, \code{"snake"}, \code{"arc"}. \code{"arc"}
#'   places anchors on a circular arc whose chord has the same row-like
#'   footprint; curvature is controlled by \code{group_arc_angle}.
#' @param group_angle Numeric (default \code{0}). Extra rotation applied to
#'   the entire anchor set, on top of \code{group_layout}'s default
#'   orientation. Accepts radians (\code{|x| <= 2*pi}) or degrees
#'   (\code{|x| > 2*pi}); use \code{\link{deg}(x)} to force a small value
#'   to mean degrees. Examples: \code{group_angle = 45} tilts a row by 45 degrees;
#'   \code{group_angle = pi/2} turns a row into a column.
#' @param group_arc_angle Numeric (default \code{pi/2}). Only used when
#'   \code{group_layout = "arc"}. Central angle subtended by the arc.
#'   \code{0} degenerates to a flat row; \code{pi/2} (= 90 degrees) is a
#'   quarter-circle (default); \code{pi} is a half-circle. Negative values
#'   flip the arc to the opposite side. Same unit auto-detection as
#'   \code{group_angle}.
#' @param anchor_dist Numeric (default \code{6}). Spacing of the anchor
#'   layout. For \code{row / column / snake} this is the centre-to-centre
#'   distance between adjacent anchors; for \code{circle / square /
#'   diamond / triangle} it is the radius from origin to each anchor; for
#'   \code{arc} it is the chord-projected spacing (chord length =
#'   \code{(n_blocks - 1) * anchor_dist}).
#' @param nrow,ncol Integer or NULL (default \code{NULL}). Grid dimensions
#'   for \code{group_layout = "row" | "column" | "snake"}. If both are
#'   NULL, defaults are inferred from the layout choice.
#'
#' @section Heatmap geometry:
#' Where the env-env heatmaps are placed and scaled relative to the
#' central spec blocks: which quadrants are drawn, how far the heatmaps
#' sit from the centre, and an overall size multiplier.
#'
#' @param orientation Character vector (default
#'   \code{c("top_right","bottom_right","top_left","bottom_left")}). Which
#'   heatmap quadrants to draw, in the same order as \code{env_select}'s
#'   elements (i.e. \code{env_select[[1]]} -> \code{orientation[1]}).
#' @param distance Numeric (default \code{3}). Offset added between the
#'   central node group and the env heatmaps. Positive pushes heatmaps
#'   outward; \code{0} places them flush against the central group;
#'   \strong{negative values are allowed} and will pull heatmaps inward
#'   (may visually overlap the central points). A \code{message()} is
#'   emitted on negative values, and a \code{warning()} is emitted if the
#'   value is so negative that an anchor coordinate becomes \eqn{\le 0}
#'   (the heatmap will then flip to the opposite side).
#' @param HeatmapScale Numeric (default \code{1}). Global scale for the
#'   overall heatmap layout (tile spacing). \code{>1} enlarges, \code{<1}
#'   shrinks.
#'
#' @section Statistics -- correlation:
#' Parameters that govern the env-env and (when
#' \code{relation_method = "correlation"}) spec-env correlation pipeline:
#' the correlation method, missing-value handling, and the choice between
#' correlation and Mantel as the spec-env relation.
#'
#' @param relation_method Character (default \code{"correlation"}). One of
#'   \code{"correlation"} or \code{"mantel"}.
#' @param cor.method Character (default \code{"pearson"}). Correlation
#'   method used by \code{psych::corr.test} for env-env (the heatmap tiles)
#'   and, when \code{relation_method = "correlation"}, for spec-env links.
#'   One of \code{"pearson"}, \code{"kendall"}, \code{"spearman"}.
#' @param cor.use Character (default \code{"everything"}). Missing-value
#'   handling for \code{psych::corr.test}. One of \code{"everything"},
#'   \code{"all"}, \code{"complete"}, \code{"pairwise"}, \code{"na"}.
#'
#' @section Statistics -- Mantel:
#' Parameters used only when \code{relation_method = "mantel"}: the
#' Mantel variant, the dissimilarity / distance metrics applied to the
#' spec block and env columns, the Mantel correlation method, and the
#' permutation count.
#'
#' @param mantel_kind Character (default \code{"block_vs_col"}). Which
#'   Mantel variant to use; see \strong{Details}. Same parameter is exposed
#'   in \code{\link{ggnetview_modularity_heatmaps}}.
#' @param spec_dist_method Character (default \code{"bray"}). Dissimilarity
#'   method (passed to \code{vegan::vegdist}) used to convert the spec
#'   block into ONE community distance matrix when
#'   \code{mantel_kind = "block_vs_col"}.
#' @param env_dist_method Character (default \code{"euclidean"}). Distance
#'   method used to convert each env column into its own distance matrix
#'   under any \code{mantel_kind}.
#' @param mantel.method2 Character (default \code{"pearson"}). Correlation
#'   coefficient passed to \code{vegan::mantel} as its \code{method}
#'   argument. One of \code{"pearson"}, \code{"kendall"},
#'   \code{"spearman"}.
#' @param permutations Integer (default \code{999L}). Number of permutations
#'   passed to \code{vegan::mantel}.
#' @param mantel.method Character. Reserved for future use -- currently
#'   accepted for backwards compatibility but \strong{not} consumed by the
#'   active code path (only \code{vegan::mantel} via \code{mantel.method2}
#'   is used).
#' @param mantel.alternative Character. Same status as \code{mantel.method}
#'   -- accepted but not consumed.
#'
#' @section What gets analysed:
#' Switches that decide which (env_block, spec_block) pairs are
#' actually computed and drawn -- the master on/off, the optional
#' restriction list, and the rule for hiding non-significant links.
#'
#' @param comparisons Logical (default \code{TRUE}). Master switch for
#'   spec-env analysis. \code{FALSE} skips all spec-env stats and links
#'   (only the env-env heatmaps remain).
#' @param comparisons_groups List or NULL (default \code{NULL}). When
#'   \code{comparisons = TRUE}, restricts which (env_block, spec_block)
#'   pairs are computed and drawn. Each element is a length-2 character
#'   vector \code{c(env_block_name, spec_block_name)}; names must match
#'   \code{names(env_select)} / \code{names(spec_select)}. \code{NULL}
#'   means "all pairs". Example:
#'   \code{list(c("Env01","Spec01"), c("Env02","Spec02"))}.
#' @param drop_nonsig Logical (default \code{FALSE}). If \code{TRUE},
#'   non-significant links (\code{Pvalue > sig_threshold}) are removed
#'   from the plot but kept in the returned stats data frame.
#'
#' @section Heatmap aesthetics:
#' Visual styling of the env-env heatmap tiles and labels: per-quadrant
#' colour palettes, label and significance-mark sizes, label rotation,
#' tile borders, and the diagonal anchor points where link lines land.
#'
#' @param HeatmapColorBar \code{NULL} or list (default \code{NULL}).
#'   Per-quadrant colour palettes. Three accepted forms:
#'   \itemize{
#'     \item \code{NULL}: use built-in defaults.
#'     \item Length-2 named list \code{list(low = ..., high = ...)}: applied
#'       to all quadrants (each value can be a vector that is recycled).
#'     \item List of length \code{length(orientation)}: each element is
#'       either \code{c(low, high)} or \code{list(low = ..., high = ...)}
#'       for that quadrant in order. Example:
#'       \code{list(c("#2166ac","#b2182b"), c("#1b7837","#762a83"),
#'                  c("#4393c3","#d6604d"), c("#92c5de","#f4a582"))}.
#'   }
#' @param HeatmapLabelSize Numeric (default \code{5}). Text size for
#'   heatmap row/column labels (ID/Type).
#' @param HeatmapSigSize Numeric (default \code{5}). Text size for the
#'   significance marks (\code{*}, \code{**}, \code{***}) inside heatmap
#'   tiles.
#' @param HeatmapLabelOrient Numeric (default \code{0}). Rotation angle (in
#'   degrees) for heatmap row/column labels. Use 45 or 90 to avoid label
#'   overlap.
#' @param HeatmapTileColor Character or \code{NA} (default \code{NA}).
#'   Border colour for heatmap tiles (passed to \code{geom_tile(colour =
#'   ...)}).
#' @param HeatmapTileSize Numeric (default \code{0}). Border line width for
#'   heatmap tiles.
#' @param HeatmapPointSize Numeric (default \code{5}). Point size for the
#'   diagonal anchor points on each heatmap (where the link lines land).
#' @param HeatmapPointFill Character vector (default \code{"#de77ae"}).
#'   Fill colour(s) for the heatmap diagonal points.
#'   \itemize{
#'     \item Length 1: same colour for all quadrants.
#'     \item Length \code{length(orientation)}: one colour per quadrant, in
#'       the order given by \code{orientation}.
#'     \item Other lengths: recycled (modulo) over quadrants.
#'   }
#'
#' @section Central node aesthetics:
#' Visual styling of the central spec nodes (or the collapsed block
#' points when \code{spec_collapse = TRUE}): point size and per-block
#' fill colour.
#'
#' @param CorePointSize Numeric (default \code{8.5}). Point size for the
#'   central species nodes (or collapsed block points).
#' @param CorePointFill Character vector (default \code{"#41b6c4"}). Fill
#'   colour(s) for the central species nodes.
#'   \itemize{
#'     \item Length 1: same colour for everyone.
#'     \item Length \code{length(spec_select)}: one colour per spec block,
#'       in the order of \code{names(spec_select)}.
#'     \item Other lengths: recycled (modulo) over spec blocks.
#'   }
#'
#' @section Link line aesthetics:
#' Visual styling of the spec-env link segments: which numeric
#' expression drives the colour and width scales for significant
#' links, the colour endpoints and optional centred midpoint, line
#' widths, transparency, and the separate styling for non-significant
#' links.
#'
#' @param link_color_by Character string (default \code{"Correlation"}).
#'   An R expression, written as a string, that is parsed via
#'   \code{rlang::parse_expr} and evaluated against the link data frame to
#'   produce the values mapped to link line colour. Accepts a bare column
#'   name (\code{"Correlation"}, \code{"Pvalue"}) or any numeric expression
#'   of the available columns (see \strong{Details} for the full list).
#'   Common choices:
#'   \itemize{
#'     \item \code{"Correlation"} -- signed effect size; pair with
#'       \code{SigLineMid = "white"} for a diverging palette around 0.
#'     \item \code{"abs(Correlation)"} -- unsigned effect size.
#'     \item \code{"-log10(Pvalue)"} -- significance strength (large = more
#'       significant).
#'   }
#'   The expression must yield a numeric vector with one entry per link
#'   row; otherwise the function errors out and lists the available
#'   numeric columns. Only significant links (\code{Pvalue <=
#'   sig_threshold}) flow through this colour scale; non-significant links
#'   are drawn flat in \code{NonsigLineColor}.
#' @param link_width_by Character string (default \code{"-log10(Pvalue)"}).
#'   An R expression (string) evaluated against the link data frame to
#'   produce the values mapped to link line width. Same syntax and
#'   evaluation rules as \code{link_color_by}; same set of available
#'   columns. Common choices: \code{"-log10(Pvalue)"} (default; significance
#'   strength), \code{"abs(Correlation)"}, \code{"Correlation^2"}. Only
#'   significant links participate in this width scale; non-significant
#'   links are drawn at \code{min(SigLineWidth)} as a flat background.
#' @param SigLineWidth Numeric vector of length 2 (default \code{c(0.5, 2)}).
#'   Min and max line width for the significant spec-env link segments
#'   (mapped through \code{link_width_by}). The minimum is also used as the
#'   fixed width for non-significant background lines.
#' @param SigLineColor Character vector of length 2 (default
#'   \code{c("#fdbb84", "#d7301f")}). Colour gradient endpoints (low, high)
#'   for the significant spec-env link segments (mapped through
#'   \code{link_color_by}).
#' @param SigLineMid Character or \code{NULL} (default \code{NULL}). If
#'   \code{NULL}, link colour uses \code{ggplot2::scale_colour_gradient(low,
#'   high)} -- no centred midpoint, suitable when the mapped variable is
#'   one-sided (e.g. \code{"Pvalue"}, \code{"-log10(Pvalue)"},
#'   \code{"abs(Correlation)"}). If a single colour string (e.g.
#'   \code{"white"}), link colour switches to
#'   \code{ggplot2::scale_colour_gradient2(low, mid, high, midpoint = 0)}
#'   -- recommended when \code{link_color_by = "Correlation"} and the
#'   values span both signs.
#' @param SigLineAlpha Numeric in \code{[0, 1]} (default \code{0.5}).
#'   Transparency of spec-env link segments (applied to both the significant
#'   and non-significant layers).
#' @param NonsigLineColor Character (default \code{"grey80"}). Fixed colour
#'   for non-significant link segments (\code{Pvalue > sig_threshold}). Only
#'   used when \code{drop_nonsig = FALSE}.
#' @param NonsigLineType Character (default \code{"dashed"}). Line type for
#'   non-significant link segments. Any value accepted by ggplot2's
#'   \code{linetype} aesthetic (e.g. \code{"solid"}, \code{"dashed"},
#'   \code{"dotted"}).
#' @param sig_threshold Numeric in \code{(0, 1)} (default \code{0.05}).
#'   P-value threshold separating significant from non-significant links.
#'   Used to (a) decide which links go through the colour/width scales,
#'   (b) drive \code{drop_nonsig} filtering.
#'
#' @section Deprecated / unused parameters:
#' These arguments are kept in the signature for backwards compatibility
#' with old call sites but are NOT consumed by the active code path. They
#' may be removed in a future release.
#' @param shape Integer (default \code{22}). Reserved; the rendered point
#'   shapes are currently hard-coded to \code{21} internally.
#' @param fontsize Numeric (default \code{5}). Deprecated. Use
#'   \code{HeatmapLabelSize} (which now also drives the central species
#'   node label size).
#'
#' @return A list of length 3:
#' \describe{
#'   \item{\code{[[1]]}}{A \code{ggplot} object with straight link segments
#'     (rendered with \code{ggplot2::geom_segment}).}
#'   \item{\code{[[2]]}}{A \code{ggplot} object with curved link segments
#'     (rendered with \code{ggplot2::geom_curve}, curvature \code{0.25}).}
#'   \item{\code{[[3]]}}{A data frame of the full spec-env statistics,
#'     \strong{unfiltered} (not affected by \code{drop_nonsig} or
#'     \code{sig_threshold}). Columns: \code{ID}, \code{Type},
#'     \code{Correlation}, \code{Pvalue}, \code{p_signif} (one of
#'     \code{""}, \code{"*"}, \code{"**"}, \code{"***"} at the fixed
#'     0.05 / 0.01 / 0.001 cutoffs), \code{spec_block}, \code{env_block},
#'     \code{method} (\code{"correlation"} or \code{"mantel"}). For
#'     \code{mantel_kind = "block_vs_col"}, \code{ID} is the spec_block
#'     name; otherwise \code{ID} is the spec column name.}
#' }
#' Both ggplot objects can be post-processed with the usual ggplot2 idioms
#' (\code{+ ggplot2::labs(...)}, \code{+ ggplot2::theme(...)}, etc.).
#'
#' @seealso
#' \code{\link{ggnetview_modularity_heatmaps}} for the modularity-based
#' counterpart with the same Mantel API;
#' \code{\link{mantel_block_vs_col}}, \code{\link{mantel_pairwise}} for the
#' underlying Mantel implementations;
#' \code{\link{deg}} to make a numeric explicitly mean degrees in
#' \code{group_angle} / \code{group_arc_angle}.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Minimal call (defaults: correlation, no collapse).
#' p <- gglink_heatmaps(
#'   env  = env,
#'   spec = spec,
#'   env_select  = list(Env01 = 1:14, Env02 = 15:28,
#'                      Env03 = 29:42, Env04 = 43:56),
#'   spec_select = list(Spec01 = 1:15, Spec02 = 16:30)
#' )
#' p[[1]]   # straight links
#' p[[2]]   # curved links
#' head(p[[3]])
#'
#' # Ecologically standard Mantel + collapse each block to one point.
#' p2 <- gglink_heatmaps(
#'   env  = env,
#'   spec = spec,
#'   env_select  = list(Env01 = 1:14, Env02 = 15:28,
#'                      Env03 = 29:42, Env04 = 43:56),
#'   spec_select = list(Spec01 = 1:15, Spec02 = 16:30),
#'   relation_method  = "mantel",
#'   mantel_kind      = "block_vs_col",
#'   spec_dist_method = "bray",
#'   env_dist_method  = "euclidean",
#'   spec_collapse    = TRUE,
#'   group_layout     = "row",
#'   group_angle      = 45,            # degrees, auto-detected
#'   anchor_dist      = 4,
#'   distance         = -1,            # heatmaps pulled slightly inward
#'   CorePointFill    = c("#2166ac", "#b2182b"),
#'   CorePointSize    = 10
#' )
#' p2[[1]]
#'
#' # Customise link aesthetics with R expressions passed as strings: colour
#' # by signed correlation (with a centred diverging palette), width by the
#' # absolute effect size, drop non-significant links, and tighten the
#' # threshold. Any string parseable as an expression of the link data
#' # frame's columns is accepted.
#' p3 <- gglink_heatmaps(
#'   env  = env,
#'   spec = spec,
#'   env_select  = list(Env01 = 1:14, Env02 = 15:28,
#'                      Env03 = 29:42, Env04 = 43:56),
#'   spec_select = list(Spec01 = 1:15, Spec02 = 16:30),
#'   link_color_by = "Correlation",
#'   link_width_by = "abs(Correlation)",
#'   SigLineColor  = c("#2166ac", "#b2182b"),
#'   SigLineMid    = "white",
#'   sig_threshold = 0.01,
#'   drop_nonsig   = TRUE
#' )
#' p3[[2]]
#' }
gglink_heatmaps <- function(
    env,
    spec,
    env_select = NULL,
    spec_select = NULL,
    spec_layout = "circle_outline",
    spec_orientation = c("up","down","left","right"),
    spec_relation = TRUE,
    relation_method = c("correlation", "mantel"),
    cor.method = c("pearson", "kendall", "spearman"),
    cor.use = c("everything", "all", "complete", "pairwise", "na"),
    mantel.method = c("mantel", "mantel.partial", "mantelhaen.test", "mantel.correlog"),
    mantel.method2 = c("pearson", "kendall", "spearman"),
    mantel.alternative = c("two.sided", "less", "greater"),
    spec_dist_method = "bray",
    env_dist_method = "euclidean",
    mantel_kind = c("block_vs_col", "col_vs_col"),
    permutations = 999L,
    spec_collapse = FALSE,
    drop_nonsig = FALSE,
    comparisons = TRUE,
    comparisons_groups = NULL,
    shape = 22,
    distance = 3,
    HeatmapLabelSize = 5,
    HeatmapSigSize = 5,
    HeatmapColorBar = NULL,
    HeatmapLabelOrient = 0,
    SigLineWidth = c(0.5, 2),
    SigLineColor = c("#fdbb84", "#d7301f"),
    SigLineMid = NULL,
    link_color_by = "Correlation",
    link_width_by = "-log10(Pvalue)",
    NonsigLineColor = "grey80",
    NonsigLineType = "dashed",
    sig_threshold = 0.05,
    HeatmapPointSize = 5,
    CorePointSize = 8.5,
    HeatmapPointFill = "#de77ae",
    CorePointFill = "#41b6c4",
    HeatmapTileColor = NA,
    HeatmapTileSize = 0,
    HeatmapScale = 1,
    SigLineAlpha = 0.5,
    fontsize = 5,
    orientation = c("top_right", "bottom_right", "top_left","bottom_left"),
    r = 6,
    group_layout = c("circle", "row", "column", "square", "diamond", "triangle", "triangle_down", "snake", "arc"),
    group_angle = 0,
    group_arc_angle = pi / 2,
    anchor_dist = 6,
    scale_networks = TRUE,
    nrow = NULL,
    ncol = NULL
){

  # argument test
  if (is.null(env_select) || is.null(spec_select)) {
    stop("`env_select` and `spec_select` must be provided (non-NULL lists).", call. = FALSE)
  }
  relation_method <- match.arg(relation_method)
  cor.method      <- match.arg(cor.method)
  cor.use         <- match.arg(cor.use)
  mantel.method   <- match.arg(mantel.method)
  mantel.method2  <- match.arg(mantel.method2)
  mantel.alternative <- match.arg(mantel.alternative)
  if (!is.character(spec_dist_method) || length(spec_dist_method) != 1L || !nzchar(spec_dist_method)) {
    stop("`spec_dist_method` must be a single non-empty character string (passed to `vegan::vegdist`).", call. = FALSE)
  }
  if (!is.character(env_dist_method) || length(env_dist_method) != 1L || !nzchar(env_dist_method)) {
    stop("`env_dist_method` must be a single non-empty character string (passed to `vegan::vegdist`).", call. = FALSE)
  }
  mantel_kind <- match.arg(mantel_kind)
  permutations <- as.integer(permutations)
  if (length(permutations) != 1L || is.na(permutations) || permutations < 1L) {
    stop("`permutations` must be a single positive integer.", call. = FALSE)
  }
  if (!is.logical(spec_collapse) || length(spec_collapse) != 1L || is.na(spec_collapse)) {
    stop("`spec_collapse` must be a single TRUE or FALSE.", call. = FALSE)
  }
  if (isTRUE(spec_collapse) &&
      !(relation_method == "mantel" && mantel_kind == "block_vs_col")) {
    message("`spec_collapse = TRUE` collapses each spec block into one point; ",
            "in the current mode (relation_method = '", relation_method,
            if (relation_method == "mantel") paste0("', mantel_kind = '", mantel_kind, "'") else "'",
            ") link sources are per spec column, so all column-level links from ",
            "a block will originate from the same collapsed point. ",
            "Consider `relation_method = 'mantel'` + `mantel_kind = 'block_vs_col'` ",
            "for a one-line-per-block visualisation.")
  }
  orientation     <- match.arg(orientation, several.ok = TRUE)
  group_layout    <- match.arg(group_layout)
  # Normalise angle inputs to radians. Both `group_angle` and `group_arc_angle`
  # accept radians (|x| <= 2*pi) or degrees (|x| > 2*pi); use `deg(x)` to be
  # explicit when the magnitude is ambiguous.
  group_angle     <- .normalize_angle(group_angle, name = "group_angle")
  group_arc_angle <- .normalize_angle(group_arc_angle, name = "group_arc_angle")
  if (group_layout == "arc" && abs(group_arc_angle) >= 2 * pi) {
    stop("`group_arc_angle` (after unit normalisation) must be in (-2*pi, 2*pi). ",
         "An arc cannot subtend a full circle or more; use `group_layout = 'circle'` instead.",
         call. = FALSE)
  }
  anchor_dist     <- as.numeric(anchor_dist)
  if (length(anchor_dist) != 1 || is.na(anchor_dist) || anchor_dist < 0) {
    stop("`anchor_dist` must be a single non-negative numeric value.", call. = FALSE)
  }



  # test
  # env = Envdf_4st
  # spec = Spedf
  # orientation = c("top_right", "bottom_right", "top_left","bottom_left")
  # # orientation = c("top_right","bottom_left")
  # # orientation = "top_right"
  # # orientation = "bottom_right"
  # cor.method = "pearson"
  # cor.use = "pairwise"
  # distance = 4
  # relation_method = "correlation"
  # r = 6
  # spec_relation = T
  # fontsize = 5




  radius = r

  HeatmapLabelSize <- as.numeric(HeatmapLabelSize)
  HeatmapSigSize   <- as.numeric(HeatmapSigSize)
  HeatmapLabelOrient <- as.numeric(HeatmapLabelOrient)
  SigLineWidth    <- as.numeric(SigLineWidth)
  SigLineColor    <- as.character(SigLineColor)
  HeatmapPointSize <- as.numeric(HeatmapPointSize)
  CorePointSize    <- as.numeric(CorePointSize)
  HeatmapPointFill <- as.character(HeatmapPointFill)
  CorePointFill    <- as.character(CorePointFill)
  HeatmapTileColor <- HeatmapTileColor
  HeatmapTileSize  <- as.numeric(HeatmapTileSize)
  HeatmapScale     <- as.numeric(HeatmapScale)
  SigLineAlpha     <- as.numeric(SigLineAlpha)
  if (is.na(HeatmapLabelSize) || length(HeatmapLabelSize) != 1 || HeatmapLabelSize <= 0) {
    stop("`HeatmapLabelSize` must be a single positive numeric value.")
  }
  if (is.na(HeatmapSigSize) || length(HeatmapSigSize) != 1 || HeatmapSigSize <= 0) {
    stop("`HeatmapSigSize` must be a single positive numeric value.")
  }
  if (length(HeatmapLabelOrient) != 1 || !is.finite(HeatmapLabelOrient)) {
    stop("`HeatmapLabelOrient` must be a single finite numeric value (angle in degrees).")
  }
  if (length(SigLineWidth) != 2 || any(!is.finite(SigLineWidth)) || any(SigLineWidth <= 0)) {
    stop("`SigLineWidth` must be a numeric vector of length 2 with positive values.")
  }
  SigLineWidth <- sort(SigLineWidth)
  if (length(SigLineColor) != 2L || any(!nzchar(SigLineColor))) {
    stop("`SigLineColor` must be a character vector of length 2 (low, high).")
  }
  if (length(HeatmapPointSize) != 1 || !is.finite(HeatmapPointSize) || HeatmapPointSize <= 0) {
    stop("`HeatmapPointSize` must be a single positive numeric value.")
  }
  if (length(CorePointSize) != 1 || !is.finite(CorePointSize) || CorePointSize <= 0) {
    stop("`CorePointSize` must be a single positive numeric value.")
  }
  if (length(HeatmapPointFill) < 1L || any(is.na(HeatmapPointFill)) || any(!nzchar(HeatmapPointFill))) {
    stop("`HeatmapPointFill` must be a non-empty character vector (length >= 1).")
  }
  if (length(CorePointFill) < 1L || any(is.na(CorePointFill)) || any(!nzchar(CorePointFill))) {
    stop("`CorePointFill` must be a non-empty character vector (length >= 1).")
  }
  if (length(HeatmapTileSize) != 1 || !is.finite(HeatmapTileSize) || HeatmapTileSize < 0) {
    stop("`HeatmapTileSize` must be a single non-negative numeric value.")
  }
  if (length(HeatmapScale) != 1 || !is.finite(HeatmapScale) || HeatmapScale <= 0) {
    stop("`HeatmapScale` must be a single positive numeric value.")
  }
  if (length(SigLineAlpha) != 1 || !is.finite(SigLineAlpha) || SigLineAlpha < 0 || SigLineAlpha > 1) {
    stop("`SigLineAlpha` must be a single numeric value between 0 and 1.")
  }
  if (!is.null(SigLineMid)) {
    if (!is.character(SigLineMid) || length(SigLineMid) != 1L || !nzchar(SigLineMid)) {
      stop("`SigLineMid` must be NULL or a single non-empty colour string.")
    }
  }
  if (!is.character(NonsigLineColor) || length(NonsigLineColor) != 1L || !nzchar(NonsigLineColor)) {
    stop("`NonsigLineColor` must be a single non-empty colour string.")
  }
  if (!is.character(NonsigLineType) || length(NonsigLineType) != 1L || !nzchar(NonsigLineType)) {
    stop("`NonsigLineType` must be a single non-empty character (linetype).")
  }
  sig_threshold <- as.numeric(sig_threshold)
  if (length(sig_threshold) != 1L || !is.finite(sig_threshold) || sig_threshold <= 0 || sig_threshold >= 1) {
    stop("`sig_threshold` must be a single numeric value in (0, 1).")
  }
  # `link_color_by` and `link_width_by` are strings holding R expressions.
  # Validate they are non-empty single strings, then parse them once into
  # quosures that can be spliced into dplyr::mutate against the link data
  # frame (e.g. "Correlation", "-log10(Pvalue)", "abs(Correlation)").
  if (!is.character(link_color_by) || length(link_color_by) != 1L || !nzchar(link_color_by)) {
    stop("`link_color_by` must be a single non-empty character string holding an R expression.", call. = FALSE)
  }
  if (!is.character(link_width_by) || length(link_width_by) != 1L || !nzchar(link_width_by)) {
    stop("`link_width_by` must be a single non-empty character string holding an R expression.", call. = FALSE)
  }
  link_color_expr <- tryCatch(
    rlang::parse_expr(link_color_by),
    error = function(e) {
      stop(sprintf("`link_color_by = '%s'` is not a parseable R expression: %s",
                   link_color_by, conditionMessage(e)), call. = FALSE)
    }
  )
  link_width_expr <- tryCatch(
    rlang::parse_expr(link_width_by),
    error = function(e) {
      stop(sprintf("`link_width_by = '%s'` is not a parseable R expression: %s",
                   link_width_by, conditionMessage(e)), call. = FALSE)
    }
  )
  # if env_select = NULL & spec_select = NULL






  ####----split data----####

  # spec_select = list(Spec01 = 1:15)
  #
  # # different env
  # env_select = list(Env01 = 1:14,
  #                   Env02 = 15:26, # 15:28
  #                   Env03 = 29:38, # 29:42
  #                   Env04 = 43:50 # 43:56
  # )

  # equal env
  # env_select = list(Env01 = 1:14,
  #                   Env02 = 15:28,
  #                   Env03 = 29:42,
  #                   Env04 = 43:56)

  # split data
  env_list <- purrr::map(env_select, ~ env[, .x, drop = FALSE])
  spec_list <- purrr::map(spec_select, ~ spec[, .x, drop = FALSE])


  k_vec  <- purrr::map_int(env_list, function(x) ncol(x))
  k_ref  <- max(k_vec)

  # distance controls the extra gap between the central species layout (radius)
  # and the environmental heatmaps.
  distance <- as.numeric(distance)
  if (is.na(distance) || length(distance) != 1) {
    stop("`distance` must be a single numeric value.")
  }
  if (distance < 0) {
    message("`distance = ", distance, "` is negative; the env heatmaps will be ",
            "pulled inward and may overlap the central point group. ",
            "If a heatmap anchor becomes <= 0 you will get a warning and the ",
            "corresponding heatmap may flip to the opposite side -- use a less ",
            "negative value if that happens.")
  }

  length_dist <- max(k_vec)


  k_gap <- length_dist - k_vec


  # purrr::map(env_list, function(x){
  #   cor_out_self <- psych::corr.test(x)
  # })


  env_cor_self_list <- list()


  for (i in seq_along(orientation)) {
    # top_right
    if (orientation[i] == "top_right") {


      cor_out_self <- psych::corr.test(env_list[[i]], use = cor.use, method = cor.method)

      # correlation
      cor_self_r <- cor_out_self$r %>% as.data.frame()
      cor_self_r[upper.tri(cor_self_r)] <- NA

      # pvalue
      cor_self_p <- cor_out_self$p %>% as.data.frame()
      cor_self_p[upper.tri(cor_self_p)] <- NA

      # combine
      cor_self_r <- cor_self_r %>%
        tibble::rownames_to_column(var = "ID") %>%
        tidyr::pivot_longer(cols = -ID,
                            names_to = "Type",
                            values_to = "Correlation") %>%
        dplyr::mutate(ID = factor(ID, levels = unique(ID), ordered = T),
                      Type = factor(Type, levels = rev(unique(Type)), ordered = T),
                      ID2 = as.numeric(ID),
                      Type2 = as.numeric(Type)
        ) %>%
        stats::na.omit()

      cor_self_p <- cor_self_p %>%
        tibble::rownames_to_column(var = "ID") %>%
        tidyr::pivot_longer(cols = -ID,
                            names_to = "Type",
                            values_to = "Pvalue") %>%
        dplyr::mutate(ID = factor(ID, levels = unique(ID), ordered = T),
                      Type = factor(Type, levels = rev(unique(Type)), ordered = T),
                      ID2 = as.numeric(ID),
                      Type2 = as.numeric(Type)) %>%
        stats::na.omit() %>%
        dplyr::mutate(p_signif = dplyr::case_when(
          Pvalue > 0.05 ~ "",
          Pvalue > 0.01 & Pvalue <= 0.05 ~ "*",
          Pvalue <= 0.01 & Pvalue >= 0.001 ~ "**",
          Pvalue < 0.001 ~ "***"
        ))

      cor_self_r_p <- cbind(cor_self_r %>% dplyr::select(1,2,4,5,3),
                            cor_self_p %>% dplyr::select(3,6))

      env_cor_self_list[[i]] <- cor_self_r_p

    }

    # bottom_right
    if (orientation[i] == "bottom_right") {

      cor_out_self <- psych::corr.test(env_list[[i]], use = cor.use, method = cor.method)


      # correlation
      cor_self_r <- cor_out_self$r %>% as.data.frame()
      cor_self_r[upper.tri(cor_self_r)] <- NA

      # pvalue
      cor_self_p <- cor_out_self$p %>% as.data.frame()
      cor_self_p[upper.tri(cor_self_p)] <- NA
      # combine
      cor_self_r <- cor_self_r %>%
        tibble::rownames_to_column(var = "ID") %>%
        tidyr::pivot_longer(cols = -ID,
                            names_to = "Type",
                            values_to = "Correlation") %>%
        dplyr::mutate(ID = factor(ID, levels = unique(ID), ordered = T),
                      Type = factor(Type, levels = unique(Type), ordered = T),
                      ID2 = as.numeric(ID),
                      Type2 = as.numeric(Type)
        ) %>%
        stats::na.omit()

      cor_self_p <- cor_self_p %>%
        tibble::rownames_to_column(var = "ID") %>%
        tidyr::pivot_longer(cols = -ID,
                            names_to = "Type",
                            values_to = "Pvalue") %>%
        dplyr::mutate(ID = factor(ID, levels = unique(ID), ordered = T),
                      Type = factor(Type, levels = unique(Type), ordered = T),
                      ID2 = as.numeric(ID),
                      Type2 = as.numeric(Type)) %>%
        stats::na.omit() %>%
        dplyr::mutate(p_signif = dplyr::case_when(
          Pvalue > 0.05 ~ "",
          Pvalue > 0.01 & Pvalue <= 0.05 ~ "*",
          Pvalue <= 0.01 & Pvalue >= 0.001 ~ "**",
          Pvalue < 0.001 ~ "***"
        ))

      cor_self_r_p <- cbind(cor_self_r %>% dplyr::select(1,2,4,5,3),
                            cor_self_p %>% dplyr::select(3,6)
      )

      env_cor_self_list[[i]] <- cor_self_r_p
    }

    # top_left
    if (orientation[i] == "top_left") {

      cor_out_self <- psych::corr.test(env_list[[i]], use = cor.use, method = cor.method)

      # correlation
      cor_self_r <- cor_out_self$r %>% as.data.frame()
      cor_self_r[lower.tri(cor_self_r)] <- NA

      # pvalue
      cor_self_p <- cor_out_self$p %>% as.data.frame()
      cor_self_p[lower.tri(cor_self_p)] <- NA

      # combine
      cor_self_r <- cor_self_r %>%
        tibble::rownames_to_column(var = "ID") %>%
        tidyr::pivot_longer(cols = -ID,
                            names_to = "Type",
                            values_to = "Correlation") %>%
        dplyr::mutate(ID = factor(ID, levels = unique(ID), ordered = T),
                      Type = factor(Type, levels = unique(Type), ordered = T),
                      ID2 = as.numeric(ID),
                      Type2 = as.numeric(Type)
        ) %>%
        stats::na.omit()

      cor_self_p <- cor_self_p %>%
        tibble::rownames_to_column(var = "ID") %>%
        tidyr::pivot_longer(cols = -ID,
                            names_to = "Type",
                            values_to = "Pvalue") %>%
        dplyr::mutate(ID = factor(ID, levels = unique(ID), ordered = T),
                      Type = factor(Type, levels = unique(Type), ordered = T),
                      ID2 = as.numeric(ID),
                      Type2 = as.numeric(Type)) %>%
        stats::na.omit() %>%
        dplyr::mutate(p_signif = dplyr::case_when(
          Pvalue > 0.05 ~ "",
          Pvalue > 0.01 & Pvalue <= 0.05 ~ "*",
          Pvalue <= 0.01 & Pvalue >= 0.001 ~ "**",
          Pvalue < 0.001 ~ "***"
        ))

      cor_self_r_p <- cbind(cor_self_r %>% dplyr::select(1,2,4,5,3),
                            cor_self_p %>% dplyr::select(3,6)
      )


      env_cor_self_list[[i]] <- cor_self_r_p
    }

    # bottom_left
    if (orientation[i] == "bottom_left") {

      cor_out_self <- psych::corr.test(env_list[[i]], use = cor.use, method = cor.method)

      # correlation
      cor_self_r <- cor_out_self$r %>% as.data.frame()
      cor_self_r[lower.tri(cor_self_r)] <- NA

      # pvalue
      cor_self_p <- cor_out_self$p %>% as.data.frame()
      cor_self_p[lower.tri(cor_self_p)] <- NA

      # combine
      cor_self_r <- cor_self_r %>%
        tibble::rownames_to_column(var = "ID") %>%
        tidyr::pivot_longer(cols = -ID,
                            names_to = "Type",
                            values_to = "Correlation") %>%
        dplyr::mutate(ID = factor(ID, levels = unique(ID), ordered = T),
                      Type = factor(Type, levels = rev(unique(Type)), ordered = T),
                      ID2 = as.numeric(ID),
                      Type2 = as.numeric(Type)
        ) %>%
        stats::na.omit()

      cor_self_p <- cor_self_p %>%
        tibble::rownames_to_column(var = "ID") %>%
        tidyr::pivot_longer(cols = -ID,
                            names_to = "Type",
                            values_to = "Pvalue") %>%
        dplyr::mutate(ID = factor(ID, levels = unique(ID), ordered = T),
                      Type = factor(Type, levels = rev(unique(Type)), ordered = T),
                      ID2 = as.numeric(ID),
                      Type2 = as.numeric(Type)) %>%
        stats::na.omit() %>%
        dplyr::mutate(p_signif = dplyr::case_when(
          Pvalue > 0.05 ~ "",
          Pvalue > 0.01 & Pvalue <= 0.05 ~ "*",
          Pvalue <= 0.01 & Pvalue >= 0.001 ~ "**",
          Pvalue < 0.001 ~ "***"
        ))

      cor_self_r_p <- cbind(cor_self_r %>% dplyr::select(1,2,4,5,3),
                            cor_self_p %>% dplyr::select(3,6)
      )


      env_cor_self_list[[i]] <- cor_self_r_p
    }
  }

  # rename list based on orientation
  names(env_cor_self_list) <- orientation

  # rename k_gap based on orientation
  names(k_gap) <- orientation




  spec_block_names <- names(spec_list)
  if (is.null(spec_block_names)) spec_block_names <- paste0("Spec", seq_along(spec_list))

  env_block_names <- names(env_list)
  if (is.null(env_block_names)) env_block_names <- paste0("Env", seq_along(env_list))

  # comparisons & comparisons_groups: which (env_block, spec_block) pairs to analyze
  if (!is.logical(comparisons) || length(comparisons) != 1 || is.na(comparisons)) {
    stop("`comparisons` must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.null(comparisons_groups)) {
    if (!is.list(comparisons_groups)) {
      stop("`comparisons_groups` must be a list, e.g. list(c('Env01', 'Spec01'), c('Env02', 'Spec01')).", call. = FALSE)
    }
    for (comp in comparisons_groups) {
      if (!is.character(comp) || length(comp) != 2) {
        stop("Each element of `comparisons_groups` must be a character vector of length 2: c(env_block, spec_block).", call. = FALSE)
      }
      if (!comp[1] %in% env_block_names) {
        stop(sprintf("`comparisons_groups`: env_block '%s' not in env_select names.", comp[1]), call. = FALSE)
      }
      if (!comp[2] %in% spec_block_names) {
        stop(sprintf("`comparisons_groups`: spec_block '%s' not in spec_select names.", comp[2]), call. = FALSE)
      }
    }
  }

  # Build (env_block, spec_block) pairs to compute
  if (!isTRUE(comparisons)) {
    pairs_to_compute <- matrix(character(0), nrow = 2, ncol = 0)
  } else if (!is.null(comparisons_groups) && length(comparisons_groups) > 0) {
    pairs_to_compute <- do.call(cbind, lapply(comparisons_groups, function(x) matrix(x, nrow = 2)))
    # Remove duplicates and invalid pairs (validation already done above)
    pairs_to_compute <- unique(as.data.frame(t(pairs_to_compute)))
    pairs_to_compute <- as.matrix(pairs_to_compute)
    if (nrow(pairs_to_compute) > 0) {
      pairs_to_compute <- t(pairs_to_compute)
    } else {
      pairs_to_compute <- matrix(character(0), nrow = 2, ncol = 0)
    }
  } else {
    # Default: all pairs (env_block x spec_block)
    grid_df <- expand.grid(env_block = env_block_names, spec_block = spec_block_names, stringsAsFactors = FALSE)
    pairs_to_compute <- t(as.matrix(grid_df[, c("env_block", "spec_block")]))
  }

  if (relation_method == "correlation") {
    cor_spec_env_parts <- list()
    for (col in seq_len(ncol(pairs_to_compute))) {
      env_blk <- pairs_to_compute[1, col]
      spec_blk <- pairs_to_compute[2, col]
      j <- which(spec_block_names == spec_blk)
      p <- which(env_block_names == env_blk)
      if (length(j) != 1 || length(p) != 1) next
      cor_env_list_tmp <- psych::corr.test(spec_list[[j]], env_list[[p]])
        cor_env_list_tmp_r <- cor_env_list_tmp$r %>%
          as.data.frame() %>%
          tibble::rownames_to_column(var = "ID") %>%
          tidyr::pivot_longer(cols = -ID,
                              names_to = "Type",
                              values_to = "Correlation")
        cor_env_list_tmp_p <- cor_env_list_tmp$p %>%
          as.data.frame() %>%
          tibble::rownames_to_column(var = "ID") %>%
          tidyr::pivot_longer(cols = -ID,
                              names_to = "Type",
                              values_to = "Pvalue") %>%
          dplyr::mutate(p_signif = dplyr::case_when(
            Pvalue > 0.05 ~ "",
            Pvalue > 0.01 & Pvalue <= 0.05 ~ "*",
            Pvalue <= 0.01 & Pvalue >= 0.001 ~ "**",
            Pvalue < 0.001 ~ "***"
          ))
        cor_env_list_tmp_r_p <- cbind(cor_env_list_tmp_r,
                                      cor_env_list_tmp_p %>% dplyr::select(3, 4)) %>%
          dplyr::mutate(spec_block = spec_blk, env_block = env_blk)
        cor_spec_env_parts[[length(cor_spec_env_parts) + 1L]] <- cor_env_list_tmp_r_p
    }
    cor_spec_env_list_out <- if (length(cor_spec_env_parts) > 0) {
      do.call(rbind, cor_spec_env_parts) %>%
        dplyr::mutate(method = "correlation")
    } else {
      tibble::tibble(ID = character(), Type = character(), Correlation = numeric(), Pvalue = numeric(),
                     p_signif = character(), spec_block = character(), env_block = character(), method = "correlation")
    }
  } else if (relation_method == "mantel") {
    # Mantel test. Dispatched to a shared helper based on `mantel_kind` so
    # that gglink_heatmaps() and ggnetview_modularity_heatmaps() use exactly
    # the same algorithm:
    #   - "block_vs_col": ecology-standard. spec block (whole) -> ONE dist;
    #                     each env column -> its own dist; mantel per pair.
    #                     ID column carries the spec_block name.
    #                     Implemented by mantel_block_vs_col().
    #   - "col_vs_col"  : legacy column-vs-column mantel (~= rank correlation).
    #                     ID column carries the spec column name.
    #                     Implemented by mantel_pairwise().
    cor_spec_env_parts <- list()
    for (col in seq_len(ncol(pairs_to_compute))) {
      env_blk <- pairs_to_compute[1, col]
      spec_blk <- pairs_to_compute[2, col]
      j <- which(spec_block_names == spec_blk)
      p <- which(env_block_names == env_blk)
      if (length(j) != 1 || length(p) != 1) next

      if (mantel_kind == "block_vs_col") {
        mout <- mantel_block_vs_col(
          spec_df          = spec_list[[j]],
          env_df           = env_list[[p]],
          block_name       = spec_blk,
          method           = mantel.method2,
          spec_dist_method = spec_dist_method,
          env_dist_method  = env_dist_method,
          permutations     = permutations,
          na_omit          = TRUE
        )
      } else {
        mout <- mantel_pairwise(
          spec_df      = spec_list[[j]],
          env_df       = env_list[[p]],
          method       = mantel.method2,
          permutations = permutations,
          na_omit      = TRUE
        )
      }

      if (base::nrow(mout) == 0L) next

      mout <- mout %>%
        dplyr::mutate(
          spec_block = spec_blk,
          env_block  = env_blk,
          p_signif = dplyr::case_when(
            is.na(Pvalue) ~ "",
            Pvalue > 0.05 ~ "",
            Pvalue > 0.01 & Pvalue <= 0.05 ~ "*",
            Pvalue <= 0.01 & Pvalue >= 0.001 ~ "**",
            Pvalue < 0.001 ~ "***",
            TRUE ~ ""
          )
        )
      cor_spec_env_parts[[length(cor_spec_env_parts) + 1L]] <- mout
    }
    cor_spec_env_list_out <- if (length(cor_spec_env_parts) > 0) {
      do.call(rbind, cor_spec_env_parts) %>%
        dplyr::mutate(method = "mantel")
    } else {
      tibble::tibble(ID = character(), Type = character(), Correlation = numeric(), Pvalue = numeric(),
                     p_signif = character(), spec_block = character(), env_block = character(), method = "mantel")
    }
  }

  cor_spec_env_list <- lapply(seq_along(orientation), function(i) {
    cor_spec_env_list_out %>%
      dplyr::filter(.data$env_block == env_block_names[i])
  })
  names(cor_spec_env_list) <- orientation

  # core location layout: one network per spec block
  n_spec <- length(spec_list)
  valid_spec_layouts <- c("circle_outline", "diamond_outline", "rectangle_outline", "square_outline")
  if (length(spec_layout) == 1) {
    spec_layout <- match.arg(spec_layout, valid_spec_layouts)
    spec_layout_vec <- rep(spec_layout, n_spec)
  } else {
    if (length(spec_layout) != n_spec) {
      stop(sprintf("`spec_layout` must be length 1 or length %d (number of spec blocks).", n_spec), call. = FALSE)
    }
    spec_layout_vec <- vapply(spec_layout, function(x) match.arg(x, valid_spec_layouts), character(1))
  }

  layout_list <- list()

  for (j in seq_len(n_spec)) {
    # `spec_collapse = TRUE` short-circuits everything: each block becomes a
    # single point labelled with the block's name. The block-level position is
    # set to (0, 0) here and translated to its anchor in the next stage.
    if (isTRUE(spec_collapse)) {
      layout_list[[j]] <- data.frame(
        ID = spec_block_names[j],
        x = 0, y = 0,
        spec_block = spec_block_names[j],
        stringsAsFactors = FALSE
      )
      next
    }
    lay_name <- spec_layout_vec[j]
    func_name <- paste0("create_layout_", lay_name)
    lay_func <- utils::getFromNamespace(func_name, "ggNetView")
    spec_relation_df <- spec_list[[j]]
    n_spec_cols <- ncol(spec_relation_df)
    if (isTRUE(spec_relation) && n_spec_cols >= 2L) {
      spec_cor_out_self <- psych::corr.test(spec_relation_df, use = cor.use, method = cor.method)
      spec_cor_self_r <- spec_cor_out_self$r %>% as.data.frame()
      spec_cor_self_r[lower.tri(spec_cor_self_r)] <- NA
      spec_cor_self_p <- spec_cor_out_self$p %>% as.data.frame()
      spec_cor_self_p[lower.tri(spec_cor_self_p)] <- NA
      spec_cor_self_r <- spec_cor_self_r %>%
        tibble::rownames_to_column(var = "ID") %>%
        tidyr::pivot_longer(cols = -ID, names_to = "Type", values_to = "Correlation") %>%
        dplyr::mutate(ID = factor(ID, levels = unique(ID), ordered = TRUE),
                      Type = factor(Type, levels = unique(Type), ordered = TRUE),
                      ID2 = as.numeric(ID), Type2 = as.numeric(Type)) %>%
        stats::na.omit()
      spec_cor_self_p <- spec_cor_self_p %>%
        tibble::rownames_to_column(var = "ID") %>%
        tidyr::pivot_longer(cols = -ID, names_to = "Type", values_to = "Pvalue") %>%
        dplyr::mutate(ID = factor(ID, levels = unique(ID), ordered = TRUE),
                      Type = factor(Type, levels = unique(Type), ordered = TRUE),
                      ID2 = as.numeric(ID), Type2 = as.numeric(Type)) %>%
        stats::na.omit() %>%
        dplyr::mutate(p_signif = dplyr::case_when(
          Pvalue > 0.05 ~ "", Pvalue > 0.01 & Pvalue <= 0.05 ~ "*",
          Pvalue <= 0.01 & Pvalue >= 0.001 ~ "**", Pvalue < 0.001 ~ "***"
        ))
      spec_cor_self_r_p <- cbind(spec_cor_self_r %>% dplyr::select(1, 2, 4, 5, 3),
                                 spec_cor_self_p %>% dplyr::select(3, 6)) %>%
        dplyr::filter(ID != Type) %>%
        dplyr::mutate(tmp = ifelse(ID > Type, paste0(ID, Type), paste0(Type, ID))) %>%
        dplyr::distinct(tmp, .keep_all = TRUE) %>%
        dplyr::select(-tmp) %>%
        dplyr::select(ID, Type, Correlation, p_signif) %>%
        purrr::set_names(c("from", "to", "weight", "sig"))
    } else {
      # spec_relation = FALSE or single-column spec (no within-block correlation)
      spec_cor_self_r_p <- data.frame(
        from = character(0), to = character(0), weight = numeric(0), sig = character(0)
      )
    }
    if (base::nrow(spec_cor_self_r_p) > 0L) {
      spec_graph_obj <- build_graph_from_df(
        df = spec_cor_self_r_p,
        node_annotation = NULL,
        directed = FALSE,
        module.method = "Fast_greedy"
      )
    } else {
      node_names <- colnames(spec_relation_df)
      spec_graph_obj <- igraph::graph_from_data_frame(
        d = data.frame(from = character(0), to = character(0), weight = numeric(0)),
        vertices = data.frame(name = node_names),
        directed = FALSE
      ) %>% tidygraph::as_tbl_graph()
    }
    lay_args <- list(graph_obj = spec_graph_obj, r = radius, node_add = NULL, orientation = spec_orientation)
    lay_args <- lay_args[names(lay_args) %in% names(formals(lay_func))]
    ly_j <- do.call(lay_func, lay_args)
    node_names <- spec_graph_obj %>%
      tidygraph::activate(nodes) %>%
      tidygraph::as_tibble() %>%
      dplyr::pull(name)
    layout_list[[j]] <- data.frame(ID = node_names, x = ly_j$x, y = ly_j$y, spec_block = spec_block_names[j])
  }

  # compute anchors for multi-network placement
  .compute_anchors <- function(n_grp, group_layout, anchor_dist, nrow, ncol,
                               group_angle = 0, group_arc_angle = pi / 2) {
    if (group_layout == "circle") {
      angles <- pi / 2 - 2 * pi * (0:(n_grp - 1)) / n_grp
      anchors <- lapply(angles, function(a) c(anchor_dist * cos(a), anchor_dist * sin(a)))
    } else if (group_layout == "row") {
      nr <- if (!is.null(nrow)) as.integer(nrow) else 1L
      nc <- if (!is.null(ncol)) as.integer(ncol) else n_grp
      if (is.null(nrow) && !is.null(ncol)) nr <- max(1L, as.integer(ceiling(n_grp / nc)))
      if (!is.null(nrow) && is.null(ncol)) nc <- max(1L, as.integer(ceiling(n_grp / nr)))
      nr <- max(1L, nr)
      nc <- max(1L, nc)
      anchors <- lapply(seq_len(n_grp), function(i) {
        ii <- i - 1L
        r <- ii %/% nc
        c <- ii %% nc
        c((c - (nc - 1) / 2) * anchor_dist, -((r - (nr - 1) / 2) * anchor_dist))
      })
    } else if (group_layout == "column") {
      nr <- if (!is.null(nrow)) as.integer(nrow) else n_grp
      nc <- if (!is.null(ncol)) as.integer(ncol) else 1L
      if (is.null(nrow) && !is.null(ncol)) nr <- max(1L, as.integer(ceiling(n_grp / nc)))
      if (!is.null(nrow) && is.null(ncol)) nc <- max(1L, as.integer(ceiling(n_grp / nr)))
      nr <- max(1L, nr)
      nc <- max(1L, nc)
      anchors <- lapply(seq_len(n_grp), function(i) {
        ii <- i - 1L
        r <- ii %% nr
        c <- ii %/% nr
        c((c - (nc - 1) / 2) * anchor_dist, -((r - (nr - 1) / 2) * anchor_dist))
      })
    } else if (group_layout == "snake") {
      nr <- if (!is.null(nrow)) as.integer(nrow) else 1L
      nc <- if (!is.null(ncol)) as.integer(ncol) else n_grp
      if (is.null(nrow) && !is.null(ncol)) nr <- max(1L, as.integer(ceiling(n_grp / nc)))
      if (!is.null(nrow) && is.null(ncol)) nc <- max(1L, as.integer(ceiling(n_grp / nr)))
      nr <- max(1L, nr)
      nc <- max(1L, nc)
      anchors <- lapply(seq_len(n_grp), function(i) {
        ii <- i - 1L
        r <- ii %/% nc
        c <- if (r %% 2L == 0L) ii %% nc else (nc - 1L) - (ii %% nc)
        c((c - (nc - 1) / 2) * anchor_dist, -((r - (nr - 1) / 2) * anchor_dist))
      })
    } else if (group_layout %in% c("square", "diamond", "triangle", "triangle_down")) {
      base_angle <- switch(group_layout,
        square = 3 * pi / 4, diamond = pi / 2,
        triangle = pi / 2, triangle_down = -pi / 2
      )
      angles <- base_angle - 2 * pi * (0:(n_grp - 1)) / n_grp
      anchors <- lapply(angles, function(a) c(anchor_dist * cos(a), anchor_dist * sin(a)))
    } else if (group_layout == "arc") {
      # Curved version of "row": n_grp anchors equally spaced by angle along
      # a circular arc whose chord length matches a flat row, i.e.
      #   chord D = (n_grp - 1) * anchor_dist
      # Given central angle theta, circle radius R = (D/2) / sin(theta/2).
      # Anchors lie on the arc with the chord on y = 0; the arc bulges in
      # +y direction (sign of theta flips it).
      if (n_grp <= 1L) {
        anchors <- list(c(0, 0))
      } else if (abs(group_arc_angle) < 1e-8) {
        # Degenerate to a flat row
        anchors <- lapply(seq_len(n_grp), function(i) {
          c((i - 1L - (n_grp - 1) / 2) * anchor_dist, 0)
        })
      } else {
        D     <- (n_grp - 1L) * anchor_dist
        theta <- group_arc_angle
        R     <- (D / 2) / sin(theta / 2)
        # Equal-angle steps from -theta/2 to +theta/2
        ang_step <- theta / (n_grp - 1L)
        node_angles <- -theta / 2 + ang_step * (0:(n_grp - 1L))
        # Position with chord on y = 0 (chord midpoint at origin)
        x <- R * sin(node_angles)
        y <- R * cos(node_angles) - R * cos(theta / 2)
        anchors <- lapply(seq_len(n_grp), function(i) c(x[i], y[i]))
      }
    } else {
      angles <- pi / 2 - 2 * pi * (0:(n_grp - 1)) / n_grp
      anchors <- lapply(angles, function(a) c(anchor_dist * cos(a), anchor_dist * sin(a)))
    }
    anchors_mat <- do.call(rbind, anchors)
    # Apply the global group rotation (in radians) to every anchor at once.
    if (!is.null(group_angle) && is.finite(group_angle) && group_angle != 0) {
      Rm <- matrix(c(cos(group_angle), -sin(group_angle),
                     sin(group_angle),  cos(group_angle)),
                   nrow = 2, byrow = TRUE)
      anchors_mat <- t(Rm %*% t(anchors_mat))
    }
    anchors_mat
  }

  if (n_spec == 1L) {
    cor_spec_env <- layout_list[[1L]] %>% dplyr::select(ID, x, y, spec_block)
  } else {
    anchors_df <- .compute_anchors(n_spec, group_layout, anchor_dist, nrow, ncol,
                                   group_angle = group_angle,
                                   group_arc_angle = group_arc_angle)
    n_nodes <- vapply(layout_list, function(x) base::nrow(x), integer(1))
    n_min <- min(n_nodes)
    cor_spec_env_parts <- list()
    for (j in seq_len(n_spec)) {
      ly_df <- layout_list[[j]]
      ax <- anchors_df[j, 1]
      ay <- anchors_df[j, 2]
      if (isTRUE(spec_collapse)) {
        # Each block is already a single (0, 0) point; just translate to anchor.
        ly_df <- ly_df %>%
          dplyr::mutate(x = ax, y = ay)
      } else if (isTRUE(scale_networks)) {
        xmin <- min(ly_df$x)
        xmax <- max(ly_df$x)
        ymin <- min(ly_df$y)
        ymax <- max(ly_df$y)
        xmid <- (xmax + xmin) / 2
        ymid <- (ymax + ymin) / 2
        scale_v <- max(xmax - xmin, ymax - ymin, 1e-8)
        ly_df <- ly_df %>%
          dplyr::mutate(
            x = (x - xmid) / scale_v * (2 * radius) + ax,
            y = (y - ymid) / scale_v * (2 * radius) + ay
          )
      } else {
        # r = minimum radius; larger networks scale by node count (n_j / n_min)
        scale_j <- n_nodes[j] / n_min
        ly_df <- ly_df %>%
          dplyr::mutate(x = x * scale_j + ax, y = y * scale_j + ay)
      }
      cor_spec_env_parts[[j]] <- ly_df %>% dplyr::select(ID, x, y, spec_block)
    }
    cor_spec_env <- do.call(rbind, cor_spec_env_parts)
  }

  # Resolve per-row fill colors for central species nodes (CorePointFill)
  # and for heatmap diagonal points (HeatmapPointFill). Both can now be
  # vectors and are recycled (modulo) over spec_blocks / orientation.
  .pick_color <- function(palette, idx) {
    palette[((as.integer(idx) - 1L) %% length(palette)) + 1L]
  }
  cor_spec_env$fill_color <- .pick_color(
    CorePointFill,
    match(cor_spec_env$spec_block, spec_block_names)
  )

  .diag_xy <- function(id_idx, type_idx, ori, k_gap, length_dist, side_anchor, heatmap_step) {
    x_anchor <- if (ori %in% c("top_right", "bottom_right")) side_anchor[["right"]] else side_anchor[["left"]]
    y_anchor <- if (ori %in% c("top_right", "top_left")) side_anchor[["top"]] else side_anchor[["bottom"]]

    x_out <- if (ori %in% c("top_right", "bottom_right")) {
      x_anchor + heatmap_step * (k_gap[[ori]] + id_idx - 1)
    } else {
      -x_anchor - heatmap_step * (length_dist - id_idx)
    }

    y_out <- if (ori %in% c("top_right", "top_left")) {
      y_anchor + heatmap_step * (k_gap[[ori]] + type_idx - 2)
    } else {
      -y_anchor - heatmap_step * (length_dist - type_idx - 1)
    }

    list(x = x_out, y = y_out)
  }

  # Heatmap step and anchor are fixed (independent of r). When r increases,
  # only the network scales; heatmaps stay at the same position and size,
  # since distance has not changed.
  r_ref <- 6
  heatmap_step <- max(1, 0.5 * r_ref / max(length_dist, 1)) * HeatmapScale
  diag_default <- min(max(abs(cor_spec_env$x), na.rm = TRUE),
                      max(abs(cor_spec_env$y), na.rm = TRUE)) / sqrt(2)
  .quad_reach <- function(df, ori, default_val) {
    val <- switch(
      ori,
      top_right = {
        idx <- df$x >= 0 & df$y >= 0
        if (!any(idx)) NA_real_ else max(pmin(df$x[idx], df$y[idx]), na.rm = TRUE)
      },
      bottom_right = {
        idx <- df$x >= 0 & df$y <= 0
        if (!any(idx)) NA_real_ else max(pmin(df$x[idx], -df$y[idx]), na.rm = TRUE)
      },
      top_left = {
        idx <- df$x <= 0 & df$y >= 0
        if (!any(idx)) NA_real_ else max(pmin(-df$x[idx], df$y[idx]), na.rm = TRUE)
      },
      bottom_left = {
        idx <- df$x <= 0 & df$y <= 0
        if (!any(idx)) NA_real_ else max(pmin(-df$x[idx], -df$y[idx]), na.rm = TRUE)
      }
    )
    if (!is.finite(val)) default_val else val
  }
  # Anchor stays at fixed position (r_ref scale) regardless of r; distance unchanged
  current_extent <- max(max(abs(cor_spec_env$x), na.rm = TRUE), max(abs(cor_spec_env$y), na.rm = TRUE), 1e-8)
  quad_reach_raw <- stats::setNames(
    vapply(orientation, function(ori) .quad_reach(cor_spec_env, ori, diag_default), numeric(1)),
    orientation
  )
  quad_anchor <- (quad_reach_raw / current_extent) * r_ref + distance
  fallback_anchor <- (diag_default / current_extent) * r_ref + distance
  # Sanity check: a non-positive anchor means the heatmap will be placed at or
  # beyond the origin (flipped to the opposite side). Most likely caused by a
  # too-negative `distance`. Warn rather than stop, so the user can still
  # inspect the resulting plot.
  if (any(c(quad_anchor, fallback_anchor) <= 0, na.rm = TRUE)) {
    warning("`distance = ", distance, "` is too negative for the current ",
            "central layout -- at least one heatmap anchor became <= 0. ",
            "The corresponding heatmap will flip to the opposite side or ",
            "collapse onto the origin. Try a larger (less negative) `distance`.",
            call. = FALSE)
  }
  side_anchor <- c(
    right = max(c(quad_anchor[intersect(c("top_right", "bottom_right"), orientation)], fallback_anchor), na.rm = TRUE),
    left = max(c(quad_anchor[intersect(c("top_left", "bottom_left"), orientation)], fallback_anchor), na.rm = TRUE),
    top = max(c(quad_anchor[intersect(c("top_right", "top_left"), orientation)], fallback_anchor), na.rm = TRUE),
    bottom = max(c(quad_anchor[intersect(c("bottom_right", "bottom_left"), orientation)], fallback_anchor), na.rm = TRUE)
  )

  # get targets informations

  .make_targets <- function(df, ori, k_gap, length_dist, side_anchor, heatmap_step){
    df_diag <- df %>%
      dplyr::mutate(
        ID = as.character(ID),
        Type = as.character(Type)
      ) %>%
      dplyr::filter(ID == Type)

    xy_diag <- .diag_xy(
      id_idx = df_diag$ID2,
      type_idx = df_diag$Type2,
      ori = ori,
      k_gap = k_gap,
      length_dist = length_dist,
      side_anchor = side_anchor,
      heatmap_step = heatmap_step
    )

    df_diag %>%
      dplyr::transmute(ID, x_to = xy_diag$x, y_to = xy_diag$y)
  }

  xy_targets <- purrr::imap_dfr(
    .x = env_cor_self_list[orientation],
    .f = .make_targets,
    k_gap = k_gap,
    length_dist = length_dist,
    side_anchor = side_anchor,
    heatmap_step = heatmap_step
  )

  # Build the lookup table that translates each row's `ID` value into a link
  # source coordinate (x, y). Three cases:
  #   - spec_collapse = TRUE : every link starts at the collapsed block point.
  #                            cor_spec_env already has one row per block with
  #                            ID = spec_block name, so a direct select works
  #                            (mantel) and we also fall back to joining via
  #                            spec_block (correlation, where row IDs are spec
  #                            column names but spec_block column carries the
  #                            block name).
  #   - relation_method = "mantel" + collapse = FALSE :
  #       ID = spec_block name -> use the network's centroid (mean of nodes).
  #   - correlation + collapse = FALSE :
  #       ID = spec column name -> use that column's node position.
  if (isTRUE(spec_collapse)) {
    block_xy <- cor_spec_env %>%
      dplyr::distinct(spec_block, .keep_all = TRUE) %>%
      dplyr::select(spec_block, x, y) %>%
      dplyr::mutate(spec_block = as.character(spec_block))

    cor_spec_env_location <- cor_spec_env_list_out %>%
      dplyr::mutate(ID = as.character(ID),
                    Type = as.character(Type),
                    spec_block = as.character(spec_block)) %>%
      dplyr::left_join(block_xy, by = "spec_block") %>%
      dplyr::left_join(xy_targets, by = c("Type" = "ID")) %>%
      dplyr::mutate(
        is_sig = .data$Pvalue <= sig_threshold
      )
  } else {
    # Pick the link-source lookup based on what `ID` actually contains:
    #   - mantel + block_vs_col : ID = spec_block name -> use block centroid
    #   - mantel + col_vs_col   : ID = spec column name -> use that node
    #   - correlation           : ID = spec column name -> use that node
    if (relation_method == "mantel" && mantel_kind == "block_vs_col") {
      link_source_xy <- cor_spec_env %>%
        dplyr::group_by(spec_block) %>%
        dplyr::summarise(x = mean(x, na.rm = TRUE),
                         y = mean(y, na.rm = TRUE),
                         .groups = "drop") %>%
        dplyr::rename(ID = spec_block) %>%
        dplyr::mutate(ID = as.character(ID))
    } else {
      link_source_xy <- cor_spec_env %>%
        dplyr::select(ID, x, y) %>%
        dplyr::mutate(ID = as.character(ID))
    }

    cor_spec_env_location <- cor_spec_env_list_out %>%
      dplyr::mutate(ID = as.character(ID), Type = as.character(Type)) %>%
      dplyr::left_join(link_source_xy, by = "ID") %>%
      dplyr::left_join(xy_targets, by = c("Type" = "ID")) %>%
      dplyr::mutate(
        is_sig = .data$Pvalue <= sig_threshold
      )
  }

  # Evaluate the expressions parsed from `link_color_by` / `link_width_by`
  # against the joined link data frame, producing internal numeric columns
  # `link_color_value` and `link_width_value`. Users can therefore pass any
  # expression of the link data frame's columns -- "Correlation", "Pvalue",
  # "-log10(Pvalue)", "abs(Correlation)", etc.
  link_df <- tryCatch(
    cor_spec_env_location %>%
      dplyr::mutate(
        link_color_value = !!link_color_expr,
        link_width_value = !!link_width_expr
      ),
    error = function(e) {
      stop(sprintf(
        "Failed to evaluate `link_color_by = '%s'` or `link_width_by = '%s'` against the link data frame. Available numeric columns: %s. Underlying error: %s",
        link_color_by, link_width_by,
        paste(names(cor_spec_env_location)[vapply(cor_spec_env_location, is.numeric, logical(1))], collapse = ", "),
        conditionMessage(e)
      ), call. = FALSE)
    }
  )
  if (!is.numeric(link_df$link_color_value)) {
    stop(sprintf(
      "`link_color_by = '%s'` must evaluate to a numeric vector. Available numeric columns: %s.",
      link_color_by,
      paste(names(cor_spec_env_location)[vapply(cor_spec_env_location, is.numeric, logical(1))], collapse = ", ")
    ), call. = FALSE)
  }
  if (!is.numeric(link_df$link_width_value)) {
    stop(sprintf(
      "`link_width_by = '%s'` must evaluate to a numeric vector. Available numeric columns: %s.",
      link_width_by,
      paste(names(cor_spec_env_location)[vapply(cor_spec_env_location, is.numeric, logical(1))], collapse = ", ")
    ), call. = FALSE)
  }

  # Guard against non-finite link widths. Expressions such as
  # `-log10(Pvalue)` become Inf when a P value is exactly 0 (floating-point
  # underflow for very strong correlations), which breaks the continuous
  # linewidth scale. Replace any non-finite width with the largest finite
  # width so the strongest links simply render at the maximum thickness.
  if (any(!is.finite(link_df$link_width_value))) {
    finite_widths <- link_df$link_width_value[is.finite(link_df$link_width_value)]
    finite_max <- if (length(finite_widths)) max(finite_widths) else 1
    link_df$link_width_value[!is.finite(link_df$link_width_value)] <- finite_max
  }

  # Filter only at plotting stage to avoid dropping central nodes
  if (isTRUE(drop_nonsig)) {
    link_df <- link_df %>% dplyr::filter(.data$Pvalue <= sig_threshold)
  }
  link_sig    <- link_df %>% dplyr::filter(.data$is_sig)
  link_nonsig <- link_df %>% dplyr::filter(!.data$is_sig)

  .offset_env <- function(df, ori, k_gap, length_dist, side_anchor, heatmap_step,
                          HeatmapLabelOrient = 0,
                          y_top_all = NULL, y_bottom_all = NULL){
    stopifnot(ori %in% c("top_right","bottom_right","top_left","bottom_left"))
    df <- df %>% dplyr::mutate(ID = as.character(ID), Type = as.character(Type))
    x_anchor <- if (ori %in% c("top_right", "bottom_right")) side_anchor[["right"]] else side_anchor[["left"]]
    y_anchor <- if (ori %in% c("top_right", "top_left")) side_anchor[["top"]] else side_anchor[["bottom"]]


    x_tile <- if (ori %in% c("top_right","bottom_right")) {
      x_anchor + heatmap_step * (k_gap[[ori]] + df$ID2 - 1)
    } else {
      -x_anchor - heatmap_step * (length_dist - df$ID2)
    }
    y_tile <- if (ori %in% c("top_right","top_left")) {
      y_anchor + heatmap_step * (k_gap[[ori]] + df$Type2 - 1)
    } else {
      -y_anchor - heatmap_step * (length_dist - df$Type2)
    }

    tile <- df %>% dplyr::mutate(x_tile = x_tile, y_tile = y_tile, orientation = ori)


    diag_df <- df %>% dplyr::filter(ID == Type)
    diag_xy <- .diag_xy(
      id_idx = diag_df$ID2,
      type_idx = diag_df$Type2,
      ori = ori,
      k_gap = k_gap,
      length_dist = length_dist,
      side_anchor = side_anchor,
      heatmap_step = heatmap_step
    )
    x_diag <- diag_xy$x
    y_diag <- diag_xy$y
    diag    <- diag_df %>% dplyr::transmute(ID, x_diag, y_diag, orientation = ori)




    if (HeatmapLabelOrient == 0 || is.null(y_top_all) || is.null(y_bottom_all)) {
      y_id_lab <- if (ori %in% c("top_right","top_left")) {
        y_anchor + heatmap_step * length_dist
      } else {
        -y_anchor - heatmap_step * length_dist
      }
    } else {
      if (ori %in% c("top_right","top_left")) {

        y_id_lab <- y_top_all + heatmap_step
      } else {

        y_id_lab <- y_bottom_all - heatmap_step
      }
    }
    x_type_lab <- if (ori %in% c("top_right","bottom_right")) {
      x_anchor + heatmap_step * length_dist
    } else {
      -x_anchor - heatmap_step * length_dist
    }
    hjust_type <- if (ori %in% c("top_right","bottom_right")) "left" else "right"

    id_lab <- df %>%
      dplyr::distinct(ID, .keep_all = TRUE) %>%
      dplyr::transmute(ID,
                       x_id = if (ori %in% c("top_right","bottom_right")) {
                         x_anchor + heatmap_step * (k_gap[[ori]] + ID2 - 1)
                       } else {
                         -x_anchor - heatmap_step * (length_dist - ID2)
                       },
                       y_id = y_id_lab, orientation = ori)

    type_lab <- df %>%
      dplyr::distinct(Type, .keep_all = TRUE) %>%
      dplyr::transmute(Type,
                       x_type = x_type_lab,
                       y_type = if (ori %in% c("top_right","top_left")) {
                         y_anchor + heatmap_step * (k_gap[[ori]] + Type2 - 1)
                       } else {
                         -y_anchor - heatmap_step * (length_dist - Type2)
                       },
                       hjust_type = hjust_type, orientation = ori)

    list(tile = tile, diag = diag, id_lab = id_lab, type_lab = type_lab)
  }

  .add_quadrant_layers <- function(p,
                                   pack,
                                   idx,
                                   scale_name = "Env",
                                  HeatmapLabelSize = 5,
                                  HeatmapSigSize = 5,
                                  HeatmapLabelOrient = 0,
                                  HeatmapTileColor = NA,
                                  HeatmapTileSize = 0,
                                   low_pal  = c("#4d9221", "#8073ac", "#4393c3", "#66bd63"),
                                   high_pal = c("#c51b7d", "#e08214", "#d6604d", "#f46d43")){
    tile     <- pack$tile
    diag     <- pack$diag
    id_lab   <- pack$id_lab
    type_lab <- pack$type_lab


    ori_lab <- unique(id_lab$orientation)[1]
    hjust_id <- dplyr::case_when(
      ori_lab %in% c("top_right", "top_left")       ~ 0,
      ori_lab %in% c("bottom_right", "bottom_left") ~ 1,
      TRUE ~ 0.5
    )
    vjust_id <- dplyr::case_when(
      ori_lab %in% c("top_right", "top_left")       ~ -0.25,
      ori_lab %in% c("bottom_right", "bottom_left") ~  0.25,
      TRUE ~ 0.5
    )

    p +
      ggplot2::geom_tile(
        data = tile,
        ggplot2::aes(x = x_tile, y = y_tile, fill = Correlation),
        colour = HeatmapTileColor,
        linewidth = HeatmapTileSize
      ) +
      ggplot2::geom_text(data = tile, ggplot2::aes(x = x_tile, y = y_tile, label = p_signif), size = HeatmapSigSize) +
      ggplot2::geom_text(data = id_lab, ggplot2::aes(x = x_id,   y = y_id,   label = ID),
                size = HeatmapLabelSize,
                vjust = vjust_id,
                hjust = hjust_id,
                angle = HeatmapLabelOrient) +
      ggplot2::geom_text(data = type_lab, ggplot2::aes(x = x_type, y = y_type, label = Type),
                hjust = type_lab$hjust_type[1],
                size = HeatmapLabelSize) +
      ggplot2::geom_point(data = diag, ggplot2::aes(x = x_diag, y = y_diag),
                 shape = 21,
                 fill = .pick_color(HeatmapPointFill, idx),
                 size = HeatmapPointSize) +
      ggplot2::scale_fill_gradient2(
        low = low_pal[idx], mid = "#ffffff", high = high_pal[idx],
        midpoint = 0, name = paste0(scale_name, " ", idx),
        guide = ggplot2::guide_colorbar(order = idx)
      )
  }

  # Resolve heatmap palettes (per-quadrant low/high)
  .resolve_heatmap_pal <- function(heatmap_colorbar, n_quad,
                                   low_default, high_default) {
    low <- rep(low_default, length.out = n_quad)
    high <- rep(high_default, length.out = n_quad)

    if (is.null(heatmap_colorbar)) {
      return(list(low = low, high = high))
    }

    if (is.list(heatmap_colorbar) &&
        length(heatmap_colorbar) == 2 &&
        all(c("low", "high") %in% names(heatmap_colorbar))) {
      low_in <- heatmap_colorbar$low
      high_in <- heatmap_colorbar$high
      if (!is.character(low_in) || !is.character(high_in)) {
        stop("`heatmap_colorbar$low` and `heatmap_colorbar$high` must be character color vectors.")
      }
      low <- rep(low_in, length.out = n_quad)
      high <- rep(high_in, length.out = n_quad)
      return(list(low = low, high = high))
    }

    if (is.list(heatmap_colorbar) && length(heatmap_colorbar) == n_quad) {
      for (i in seq_len(n_quad)) {
        el <- heatmap_colorbar[[i]]
        if (is.list(el) && all(c("low", "high") %in% names(el))) {
          low[i] <- as.character(el$low)[1]
          high[i] <- as.character(el$high)[1]
        } else if (is.atomic(el) && length(el) >= 2) {
          low[i] <- as.character(el[[1]])[1]
          high[i] <- as.character(el[[2]])[1]
        } else {
          stop("`heatmap_colorbar[[i]]` must be `c(low, high)` or `list(low=..., high=...)`.")
        }
      }
      return(list(low = low, high = high))
    }

    stop("`heatmap_colorbar` must be NULL, a list(low=..., high=...), or a per-quadrant list.")
  }


  .compute_y_range <- function(df, ori, k_gap, length_dist, side_anchor, heatmap_step){
    df <- df %>% dplyr::mutate(ID = as.character(ID), Type = as.character(Type))
    y_anchor <- if (ori %in% c("top_right", "top_left")) side_anchor[["top"]] else side_anchor[["bottom"]]
    y_tile <- if (ori %in% c("top_right","top_left")) {
      y_anchor + heatmap_step * (k_gap[[ori]] + df$Type2 - 1)
    } else {
      -y_anchor - heatmap_step * (length_dist - df$Type2)
    }
    data.frame(
      orientation = ori,
      ymin = min(y_tile, na.rm = TRUE),
      ymax = max(y_tile, na.rm = TRUE)
    )
  }

  y_ranges <- purrr::imap_dfr(
    env_cor_self_list[orientation],
    ~ .compute_y_range(.x, .y, k_gap, length_dist, side_anchor, heatmap_step)
  )

  y_top_all <- y_ranges %>%
    dplyr::filter(.data$orientation %in% c("top_right","top_left")) %>%
    dplyr::pull(.data$ymax) %>%
    max(na.rm = TRUE)

  y_bottom_all <- y_ranges %>%
    dplyr::filter(.data$orientation %in% c("bottom_right","bottom_left")) %>%
    dplyr::pull(.data$ymin) %>%
    min(na.rm = TRUE)


  packs <- purrr::imap(
    env_cor_self_list[orientation],
    ~ .offset_env(.x, .y, k_gap, length_dist, side_anchor, heatmap_step,
                  HeatmapLabelOrient = HeatmapLabelOrient,
                  y_top_all = y_top_all,
                  y_bottom_all = y_bottom_all)
  )


  diag_all <- purrr::map_dfr(packs, "diag")
  if (base::nrow(diag_all) > 0L) {
    diag_all$fill_color <- .pick_color(
      HeatmapPointFill,
      match(diag_all$orientation, orientation)
    )
  } else {
    diag_all$fill_color <- character(0)
  }

  p0 <- ggplot2::ggplot()

  pal_default_low  <- c("#4d9221", "#8073ac", "#4393c3", "#66bd63")
  pal_default_high <- c("#c51b7d", "#e08214", "#d6604d", "#f46d43")
  pal <- .resolve_heatmap_pal(HeatmapColorBar, length(packs), pal_default_low, pal_default_high)

  for (i in seq_along(packs)) {
    if (i > 1) p0 <- p0 + ggnewscale::new_scale_fill()
    p0 <- .add_quadrant_layers(
      p0, packs[[i]], idx = i, scale_name = "Env",
      HeatmapLabelSize = HeatmapLabelSize,
      HeatmapSigSize = HeatmapSigSize,
      HeatmapLabelOrient = HeatmapLabelOrient,
      HeatmapTileColor = HeatmapTileColor,
      HeatmapTileSize = HeatmapTileSize,
      low_pal = pal$low,
      high_pal = pal$high
    )
  }

  # Pick the colour scale for the significant link layer. SigLineMid = NULL
  # keeps the historical 2-colour gradient (suitable for one-sided variables
  # such as Pvalue / neglog10p). A non-NULL SigLineMid switches to a
  # diverging scale_colour_gradient2(midpoint = 0), recommended when mapping
  # a signed variable like Correlation.
  link_color_scale <- if (is.null(SigLineMid)) {
    ggplot2::scale_color_gradient(
      low = SigLineColor[1], high = SigLineColor[2],
      name = link_color_by
    )
  } else {
    ggplot2::scale_color_gradient2(
      low = SigLineColor[1], mid = SigLineMid, high = SigLineColor[2],
      midpoint = 0, name = link_color_by
    )
  }

  # Build the link layers: an optional non-significant background (flat
  # gray, fixed thinnest width, dashed) and a significant foreground that
  # carries the two scales the user can configure via `link_color_by` and
  # `link_width_by`. We keep these as small builders so p1 (geom_segment)
  # and p2 (geom_curve) share exactly the same logic.
  .nonsig_layer <- function(geom_fn, extra_args = list()) {
    if (isTRUE(drop_nonsig) || base::nrow(link_nonsig) == 0L) return(NULL)
    do.call(geom_fn, c(list(
      data = link_nonsig,
      mapping = ggplot2::aes(x = x, y = y, xend = x_to, yend = y_to),
      colour    = NonsigLineColor,
      linetype  = NonsigLineType,
      linewidth = min(SigLineWidth),
      alpha     = SigLineAlpha
    ), extra_args))
  }
  .sig_layer <- function(geom_fn, extra_args = list()) {
    if (base::nrow(link_sig) == 0L) return(NULL)
    do.call(geom_fn, c(list(
      data = link_sig,
      mapping = ggplot2::aes(
        x = x, y = y, xend = x_to, yend = y_to,
        colour    = .data$link_color_value,
        linewidth = .data$link_width_value
      ),
      linetype = "solid",
      alpha    = SigLineAlpha
    ), extra_args))
  }

  .build_link_plot <- function(geom_fn, geom_extra = list(), plot_margin) {
    has_sig <- base::nrow(link_sig) > 0L
    p <- p0 +
      ggnewscale::new_scale_color() +
      .nonsig_layer(geom_fn, geom_extra) +
      .sig_layer(geom_fn, geom_extra) +
      (if (has_sig) link_color_scale else NULL) +
      (if (has_sig) ggplot2::scale_linewidth_continuous(range = SigLineWidth, name = link_width_by) else NULL) +
      ggplot2::geom_point(
        data = diag_all,
        ggplot2::aes(x = x_diag, y = y_diag),
        shape = 21, fill = diag_all$fill_color, size = HeatmapPointSize
      ) +
      ggplot2::geom_point(
        data = cor_spec_env,
        ggplot2::aes(x = x, y = y), shape = 21,
        fill = cor_spec_env$fill_color, size = CorePointSize
      ) +
      ggplot2::geom_text(
        data = cor_spec_env,
        ggplot2::aes(x = x, y = y, label = ID),
        size = HeatmapLabelSize
      ) +
      ggplot2::coord_cartesian(clip = "off") +
      ggplot2::theme_void() +
      ggplot2::theme(
        plot.margin = plot_margin,
        aspect.ratio = 1,
        legend.position = "top"
      )
    p
  }

  p1 <- .build_link_plot(
    geom_fn     = ggplot2::geom_segment,
    geom_extra  = list(),
    plot_margin = ggplot2::margin(10, 10, 10, 10)
  )

  p2 <- .build_link_plot(
    geom_fn     = ggplot2::geom_curve,
    geom_extra  = list(curvature = 0.25),
    plot_margin = ggplot2::margin(1, 1, 1, 1, "cm")
  )

  return(list(p1, p2, cor_spec_env_list_out))

}


