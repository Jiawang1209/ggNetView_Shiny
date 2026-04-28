# ggNetView.shiny

`ggNetView.shiny` provides an interactive **shinydashboard** front-end
for the [`ggNetView`](https://github.com/Jiawang1209/ggNetView) R
package — reproducible and deterministic network analysis &
visualization.

## Installation

```r
# install ggNetView first
# devtools::install_github("Jiawang1209/ggNetView")

# then this companion GUI package:
devtools::install_local("path/to/ggNetView.shiny")
```

## Quick start

```r
library(ggNetView.shiny)
launch_ggNetView()           # opens browser at http://127.0.0.1:<port>
```

To deploy on Shiny Server / shinyapps.io / Posit Connect:

```r
ggNetView.shiny::ggNetViewApp()
```

## Modules

| Tab                | ggNetView function(s) it wraps                          |
|--------------------|---------------------------------------------------------|
| Data               | `data(package = "ggNetView")` + CSV / TSV / RDS upload  |
| Build Network      | `build_graph_from_mat`, `build_graph_from_adj_mat`, `build_graph_from_df` |
| Visualize          | `ggNetView`                                             |
| Topology / zi-pi   | `get_network_topology`, `ggnetview_zipi`                |
| Env-Spec Linkage   | `gglink_heatmaps`                                       |

### Built-in datasets exposed in the GUI
`otu_tab`, `otu_rare`, `otu_rare_relative`, `tax_tab`,
`Envdf`, `Envdf_4st`, `Envdf_4st_2`, `Spedf`, `BASV_tab`, `FASV_tab`,
`otu_NatureWater`, `tax_NatureWater`, `otu_sample`, `ppi_example`,
`ppi_module`, `double_mat_node_df`, `double_mat_node_df_with_modularity`,
`adjacency_matrix_example`.

## Author

Yue Liu, Chao Wang — IAE
