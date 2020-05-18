#' Start ShinyItemAnalysis application
#'
#' @aliases startShinyItemAnalysis
#'
#' @description
#' An interactive shiny application for running test and item analysis.
#' By default, the function runs the application as a background process
#' (Jobs tab in RStudio). User is then free to use the Console for other
#' work and to try the sample R code examples.
#' You can still run the app the usual way in the console by specifying
#' anything but "background" in the \code{run_in} argument.
#'
#' @usage startShinyItemAnalysis(run_in = "background")
#'
#' @param run_in character: how the application shloud run; set to anything except "background" to run the application the usual way (in the console)
#' @return No return value. Called for side effects. However, the call creates \code{job_id} object accessible from global scope.
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
#' startShinyItemAnalysis(run_in = "console")
#' }
#'
#' @import rstudioapi
#'
#' @export
startShinyItemAnalysis <- function(run_in = "background") {

  start_sia <- '
    appDir <-
      system.file("shiny-examples", "ShinyItemAnalysis", package = "ShinyItemAnalysis")
    if (appDir == "") {
      stop("Could not find example directory. Try re-installing `ShinyItemAnalysis`.",
           call. = FALSE)
    }
    shiny::runApp(appDir, display.mode = "normal", launch.browser = T)
  '

  if (run_in == "background") {
    if (rstudioapi::isAvailable()) {

      temp_script <- tempfile(fileext = ".R")
      cat(start_sia, file = temp_script)
      job_id <- rstudioapi::jobRunScript(temp_script, name = "ShinyItemAnalysis")

      rstudioapi::jobSetStatus(job_id,
                               "The app is running in background. You can close it with the STOP button.")

      rstudioapi::executeCommand("activateConsole")

      message(
        "\n----------------------------------------------------------------\n",
        "ShinyItemAnalysis will start shortly as a background process.\n",
        "The process automatically stops as you close the browser window.\n",
        "----------------------------------------------------------------"
      )
    } else {
      message(
        "\n---------------------------------------------------------------\n",
        "ShinyItemAnalysis will run in the console as usual.\n",
        "To run the app in a background process, please use RStudio IDE.\n",
        "---------------------------------------------------------------"
      )
      Sys.sleep(5)
      eval(parse(text = start_sia))
    }
  } else {
    eval(parse(text = start_sia))
  }
}
