.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "ggNetView.shiny: launch the interactive GUI with ",
    "`launch_ggNetView()`."
  )
}
