#' @export
startShinyItemAnalysis <- function() {
  appDir <- system.file("shiny-examples", "ShinyItemAnalysis", package = "ShinyItemAnalysis")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `ShinyItemAnalysis`.", call. = FALSE)
  }
  shiny::runApp(appDir, display.mode = "normal")
}
