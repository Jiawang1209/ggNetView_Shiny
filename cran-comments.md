## Test environments

* local macOS, R 4.4.x
* win-builder (devel and release)
* R-hub (ubuntu-latest, windows-latest, macos-latest)

## R CMD check results

0 errors | 0 warnings | 0 notes

## Submission notes

This is the initial CRAN submission of `ggNetView`.

`ggNetView` provides a unified, reproducible framework for analyzing
and visualizing complex biological, ecological, and microbial
association networks, with deterministic layout generators built on
`ggraph` and `ggplot2`.

* The package depends on Bioconductor / GitHub-only packages
  (`WGCNA`, `SpiecEasi`) listed under `Suggests` where possible;
  hard dependencies on these are documented in the `Imports` field
  and installation instructions are provided in the README.
* All exported functions are documented with runnable `\examples`
  using the bundled example datasets.
* No non-ASCII characters remain in `R/*.R` files.
* `LazyData: true` is enabled; bundled datasets are small.

## Downstream dependencies

There are currently no downstream dependencies on CRAN.
