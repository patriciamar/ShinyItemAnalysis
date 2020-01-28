.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    paste("This is ShinyItemAnalysis version "), packageVersion("ShinyItemAnalysis"),
    "\nTo run interactive ShinyItemAnalysis application, print: startShinyItemAnalysis()"
  )
}
