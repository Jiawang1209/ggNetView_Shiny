## Dataset documentation for ggNetView
## All exported datasets are documented here so that R CMD check has
## an entry for every name shipped under data/. Real-world data sets
## bundled with the package are kept small (subsets of published or
## simulated studies) and are intended only for examples and tests.

#' Example OTU abundance table
#'
#' A microbial Operational Taxonomic Unit (OTU) abundance matrix used by
#' the package examples. Rows are OTUs and columns are samples.
#'
#' @format A `data.frame` with 2,859 rows (OTUs) and 18 columns (samples).
#'   Cell values are raw read counts.
#' @source Internal example data shipped with ggNetView.
#' @examples
#' data(otu_tab)
#' otu_tab[1:5, 1:5]
"otu_tab"

#' Rarefied OTU abundance table
#'
#' A rarefied version of [otu_tab] in which every sample has been
#' subsampled to a common sequencing depth.
#'
#' @format A `data.frame` with 2,859 rows (OTUs) and 18 columns (samples).
#' @source Derived from [otu_tab] by rarefaction.
#' @examples
#' data(otu_rare)
#' otu_rare[1:5, 1:5]
"otu_rare"

#' Relative-abundance OTU table
#'
#' A relative-abundance transformation of [otu_rare]: each column sums
#' approximately to 1.
#'
#' @format A `data.frame` with 2,859 rows and 18 columns.
#' @source Derived from [otu_rare].
#' @examples
#' data(otu_rare_relative)
#' otu_rare_relative[1:5, 1:5]
"otu_rare_relative"

#' Taxonomy annotation table
#'
#' Taxonomic classification (Kingdom to Species) for the OTUs in
#' [otu_tab]. The first column `OTUID` is used as the join key.
#'
#' @format A `tbl_df` with 2,859 rows and 8 columns
#'   (`OTUID`, `Kingdom`, `Phylum`, `Class`, `Order`, `Family`,
#'   `Genus`, `Species`).
#' @source Internal example data shipped with ggNetView.
#' @examples
#' data(tax_tab)
#' head(tax_tab)
"tax_tab"

#' Environmental variables (single block)
#'
#' Environmental measurements per sample used together with the
#' species/OTU tables for correlation and Mantel-style examples.
#'
#' @format A `data.frame` with 24 rows (samples) and 14 columns
#'   (environmental variables).
#' @source Internal example data shipped with ggNetView.
#' @examples
#' data(Envdf)
#' head(Envdf)
"Envdf"

#' Environmental variables across four sampling stages
#'
#' Wide-form environmental data covering four stages (`Env01`-`Env04`),
#' used by [gglink_heatmaps()] examples.
#'
#' @format A `data.frame` with 24 rows (samples) and 56 columns.
#' @source Internal example data shipped with ggNetView.
#' @examples
#' data(Envdf_4st)
#' Envdf_4st[1:3, 1:6]
"Envdf_4st"

#' Alternative four-stage environmental dataset
#'
#' A second, slightly smaller variant of [Envdf_4st] used to exercise
#' multi-block alignment functions.
#'
#' @format A `data.frame` with 18 rows and 56 columns.
#' @source Internal example data shipped with ggNetView.
#' @examples
#' data(Envdf_4st_2)
#' dim(Envdf_4st_2)
"Envdf_4st_2"

#' Species composition table for link-heatmap examples
#'
#' A wide-form species/OTU table that is paired with [Envdf_4st] in
#' the `gglink_heatmaps*` examples.
#'
#' @format A `data.frame` with 24 rows (samples) and 44 columns
#'   (species or features).
#' @source Internal example data shipped with ggNetView.
#' @examples
#' data(Spedf)
#' Spedf[1:3, 1:6]
"Spedf"

#' Bacterial ASV demonstration table
#'
#' A small subset of bacterial Amplicon Sequence Variants used by the
#' package's classification helpers.
#'
#' @format A `data.frame` with 50 rows and 10 columns.
#' @source Internal example data shipped with ggNetView.
#' @examples
#' data(BASV_tab)
#' head(BASV_tab)
"BASV_tab"

#' Fungal ASV demonstration table
#'
#' A small subset of fungal Amplicon Sequence Variants used together
#' with [BASV_tab] for cross-domain network examples.
#'
#' @format A `data.frame` with 50 rows and 10 columns.
#' @source Internal example data shipped with ggNetView.
#' @examples
#' data(FASV_tab)
#' head(FASV_tab)
"FASV_tab"

#' Larger OTU table from a published water-quality study
#'
#' A wide OTU abundance matrix from a Nature Water companion dataset,
#' provided for performance and parallel-topology examples.
#'
#' @format A `data.frame` with 6,752 rows (OTUs) and 1,186 columns
#'   (samples).
#' @source Adapted from a published study; subset for demonstration.
#' @examples
#' data(otu_NatureWater)
#' dim(otu_NatureWater)
"otu_NatureWater"

#' Taxonomy table for [otu_NatureWater]
#'
#' Taxonomic annotation matching [otu_NatureWater].
#'
#' @format A `tbl_df` with 51,998 rows and 18 columns.
#' @source Adapted from the same published study as [otu_NatureWater].
#' @examples
#' data(tax_NatureWater)
#' head(tax_NatureWater)
"tax_NatureWater"

#' Sample metadata for the OTU example
#'
#' Sample-level metadata accompanying [otu_tab].
#'
#' @format A `data.frame` with 18 rows (samples) and 2 columns.
#' @source Internal example data shipped with ggNetView.
#' @examples
#' data(otu_sample)
#' head(otu_sample)
"otu_sample"

#' Protein-protein interaction example
#'
#' A small protein-protein interaction (PPI) network used by the
#' `build_graph_from_pie` and STRING-DB examples. The list contains
#' the interaction edge table and a node annotation table.
#'
#' @format A list with two elements:
#' \describe{
#'   \item{`ppi`}{Edge `data.frame` (`from`, `to`, `weight`).}
#'   \item{`annotation`}{Node annotation `data.frame`.}
#' }
#' @source Subset of the STRING database (https://string-db.org/).
#' @examples
#' data(ppi_example)
#' names(ppi_example)
"ppi_example"

#' Protein-protein interaction example with module assignment
#'
#' Same shape as [ppi_example] but the annotation table additionally
#' carries a `Module` column.
#'
#' @format A list with two elements (`ppi`, `annotation`).
#' @source Subset of the STRING database (https://string-db.org/).
#' @examples
#' data(ppi_module)
#' names(ppi_module)
"ppi_module"

#' Two-block node table for double-matrix examples
#'
#' Node-level information used to build cross-block ("double matrix")
#' networks. Each row corresponds to one node and identifies which of
#' the two blocks it belongs to.
#'
#' @format A `data.frame` with 100 rows and 2 columns.
#' @source Internal example data shipped with ggNetView.
#' @examples
#' data(double_mat_node_df)
#' head(double_mat_node_df)
"double_mat_node_df"

#' Two-block node table with pre-computed modularity
#'
#' Same as [double_mat_node_df] with an extra `Module` / modularity
#' column attached so that examples can skip community detection.
#'
#' @format A `data.frame` with 100 rows and 3 columns.
#' @source Internal example data shipped with ggNetView.
#' @examples
#' data(double_mat_node_df_with_modularity)
#' head(double_mat_node_df_with_modularity)
"double_mat_node_df_with_modularity"

#' Pre-computed adjacency matrix
#'
#' A symmetric numeric adjacency matrix derived from [otu_rare_relative]
#' for tests that need a deterministic input.
#'
#' @format A 2,859 x 2,859 numeric `matrix`.
#' @source Computed from [otu_rare_relative] using a Pearson correlation
#'   threshold.
#' @examples
#' data(adjacency_matrix_example)
#' adjacency_matrix_example[1:5, 1:5]
"adjacency_matrix_example"
