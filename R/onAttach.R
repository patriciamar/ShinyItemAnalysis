.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    paste("This is ShinyItemAnalysis version "), packageVersion("ShinyItemAnalysis"),
    "\n- to run the interactive {shiny} app, call `run_app()`",
    "\n- to learn more, visit \"www.ShinyItemAnalysis.org\""
  )
}
