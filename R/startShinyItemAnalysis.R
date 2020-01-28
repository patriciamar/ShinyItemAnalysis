#' This function will start ShinyItemAnalysis application.
#'
#' @aliases startShinyItemAnalysis
#'
#' @description An interactive shiny application for running test and item analysis.
#'
#' @usage startShinyItemAnalysis()
#'
#' @author
#' Patricia Martinkova \cr
#' Institute of Computer Science of the Czech Academy of Sciences \cr
#' \email{martinkova@@cs.cas.cz} \cr
#'
#' Adela Hladka \cr
#' Institute of Computer Science of the Czech Academy of Sciences \cr
#' Faculty of Mathematics and Physics, Charles University \cr
#' \email{hladka@@cs.cas.cz} \cr
#'
#' @examples
#' \dontrun{
#' rm(list = ls())
#' startShinyItemAnalysis()
#' }
#' @export
startShinyItemAnalysis <- function() {
  appDir <- system.file("shiny-examples", "ShinyItemAnalysis", package = "ShinyItemAnalysis")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `ShinyItemAnalysis`.", call. = FALSE)
  }
  shiny::runApp(appDir, display.mode = "normal", launch.browser = T)
}
