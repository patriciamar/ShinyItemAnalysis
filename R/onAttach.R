.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    paste("This is ShinyItemAnalysis version "), packageVersion("ShinyItemAnalysis"),
    "\nTo run interactive shiny app, print startShinyItemAnalysis()",
    "\nTo learn more, visit www.ShinyItemAnalysis.org"
  )
}
