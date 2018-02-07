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
#' Institute of Computer Science, The Czech Academy of Sciences \cr
#' martinkova@cs.cas.cz \cr
#'
#' Adela Drabinova \cr
#' Institute of Computer Science, The Czech Academy of Sciences \cr
#' Faculty of Mathematics and Physics, Charles University \cr
#' drabinova@cs.cas.cz \cr
#'
#' Ondrej Leder \cr
#' Institute of Computer Science, The Czech Academy of Sciences \cr
#' Faculty of Mathematics and Physics, Charles University \cr
#' ondraleder@gmail.com  \cr
#'
#' Jakub Houdek \cr
#' Institute of Computer Science, The Czech Academy of Sciences \cr
#' The Faculty of Informatics and Statistics, University of Economics, Prague \cr
#' houdek.james@gmail.com \cr
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
  shiny::runApp(appDir, display.mode = "normal")
}
