.onLoad <- function(libname, pkgname) {
  packageStartupMessage(
    paste("This is ShinyItemAnalysis version "), packageVersion(pkgname),
    "\nTo run interactive ShinyItemAnalysis application, print: startShinyItemAnalysis()"
  )
}
