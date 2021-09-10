#' Start ShinyItemAnalysis application
#'
#' @aliases startShinyItemAnalysis
#'
#' @description An interactive shiny application to run test and item analysis.
#' By default, the function runs the application as a background process (Jobs
#' tab in the 'RStudio'). User is then free to use the Console for other work
#' and to try the sample R code examples. You can still run the app the usual
#' way in the console by specifying \code{background = FALSE}.
#'
#' @param background logical: should the application be run as a background
#'   process (in the 'RStudio')?
#'
#' @return No return value. Called for side effects.
#'
#' @author
#' Patricia Martinkova \cr
#' Institute of Computer Science of the Czech Academy of Sciences \cr
#' \email{martinkova@@cs.cas.cz} \cr
#'
#' Adela Hladka \cr
#' Institute of Computer Science of the Czech Academy of Sciences \cr
#' \email{hladka@@cs.cas.cz} \cr
#'
#' @examples
#' \dontrun{
#' rm(list = ls())
#' startShinyItemAnalysis()
#' startShinyItemAnalysis(background = FALSE)
#' }
#'
#' @import rstudioapi
#'
#' @export
startShinyItemAnalysis <- function(background = TRUE) {
  start_sia <- '
    appDir <-
      system.file("ShinyItemAnalysis", package = "ShinyItemAnalysis")
    if (appDir == "") {
      stop("Could not find example directory. Try re-installing `ShinyItemAnalysis`.",
           call. = FALSE)
    }
    shiny::runApp(appDir, display.mode = "normal", launch.browser = TRUE)
  '

  if (!rstudioapi::isAvailable()) {
    message(
      "\n---------------------------------------------------------------\n",
      "ShinyItemAnalysis will run in the console as usual.\n",
      "To run the app in a background process, please use RStudio IDE.\n",
      "---------------------------------------------------------------"
    )
    Sys.sleep(5)
    eval(parse(text = start_sia))
  } else if (background) {
    temp_script <- tempfile(fileext = ".R")
    cat(start_sia, file = temp_script)

    job_id <- tryCatch(
      {
        rstudioapi::jobRunScript(temp_script, name = "ShinyItemAnalysis")
      },
      error = function(e) {
        message(
          "Your system username '",
          Sys.getenv("USERNAME"),
          "' contains special characters.\nPlease use 'startShinyItemAnalysis(background = FALSE)'."
        )
        return("fail")
      }
    )

    if (job_id != "fail") {
      rstudioapi::jobSetStatus(
        job_id,
        "The app is running in background. You can close it with the STOP button."
      )

      rstudioapi::executeCommand("activateConsole")

      message(
        "\n----------------------------------------------------------------\n",
        "ShinyItemAnalysis will start shortly as a background process.\n",
        "The process automatically stops as you close the browser window.\n",
        "----------------------------------------------------------------"
      )
    }
  } else {
    eval(parse(text = start_sia))
  }
}
