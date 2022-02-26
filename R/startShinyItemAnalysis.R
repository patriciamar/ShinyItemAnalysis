#' Start ShinyItemAnalysis application
#'
#' An interactive shiny application to run test and item analysis. By default,
#' the function runs the application as a background process ("Jobs" tab in the
#' "RStudio" IDE). User is then free to use the `R` Console for other work
#' and to try the sample R code examples. You can still run the app the usual
#' way in the console by specifying `background = FALSE`.
#'
#' @param background *logical*, should the application be run as a
#'   background process (in the 'RStudio')?
#'
#' @inheritDotParams utils::install.packages -pkgs -libs_only
#'
#' @return No return value. Called for side effects.
#'
#' @author
#' Patricia Martinkova \cr
#' Institute of Computer Science of the Czech Academy of Sciences \cr
#' \email{martinkova@@cs.cas.cz}
#'
#' Adela Hladka \cr
#' Institute of Computer Science of the Czech Academy of Sciences \cr
#' \email{hladka@@cs.cas.cz}
#'
#' Jan Netik \cr
#' Institute of Computer Science of the Czech Academy of Sciences \cr
#' \email{netik@@cs.cas.cz}
#'
#' @examples
#' \dontrun{
#' startShinyItemAnalysis()
#' startShinyItemAnalysis(background = FALSE)
#' }
#'
#' @aliases run_app
#'
#' @importFrom rstudioapi isAvailable jobRunScript jobSetStatus executeCommand
#'
#' @export
startShinyItemAnalysis <- function(background = TRUE, ...) {
  check_app_deps(...) # dots are for install.packages() options

  run_app_script <- '
  appDir <- system.file("ShinyItemAnalysis", package = "ShinyItemAnalysis")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `ShinyItemAnalysis`.", call. = FALSE)
  }
  shiny::runApp(appDir, display.mode = "normal", launch.browser = TRUE)
  '

  if (!isAvailable()) {
    message(
      "\n---------------------------------------------------------------\n",
      "ShinyItemAnalysis will run in the console as usual.\n",
      "To run the app in a background process, please use RStudio IDE.\n",
      "---------------------------------------------------------------"
    )
    Sys.sleep(5)
    eval(parse(text = run_app_script))
  } else if (background) {
    temp_script <- tempfile(fileext = ".R")
    writeLines(run_app_script, con = temp_script)

    job_id <- tryCatch(
      {
        jobRunScript(temp_script, name = "ShinyItemAnalysis")
      },
      error = function(e) {
        message(
          "There was an error running the app as a background job.\n",
          "It is likely because of your username \"",
          Sys.info()[["user"]],
          "\" containing special characters.\n",
          "Please use `startShinyItemAnalysis(background = FALSE)`."
        )
        return("fail")
      }
    )

    if (job_id != "fail") {
      jobSetStatus(
        job_id,
        "The app is running in background. You can close it with the STOP button."
      )

      executeCommand("activateConsole")

      message(
        "\n----------------------------------------------------------------\n",
        "ShinyItemAnalysis has started as a background process.\n",
        "The process automatically stops as you close the browser window.\n",
        "----------------------------------------------------------------"
      )
    }
  } else {
    eval(parse(text = run_app_script))
  }
}


#' @rdname startShinyItemAnalysis
#' @export
run_app <- function(background = TRUE, ...) {
  startShinyItemAnalysis(background = background, ...)
}


#' Check that all dependencies are satisfied
#'
#' @keywords internal
#' @noRd
check_app_deps <- function(...) {
  suggests <- read.dcf(system.file("DESCRIPTION", package = "ShinyItemAnalysis"), fields = "Suggests")
  suggests <- trimws(strsplit(suggests, c(",", "\\("))[[1]])
  suggests <- sapply(strsplit(suggests, "\\("), function(x) trimws(x[[1]]))


  suggests_installed <- sapply(suggests, function(x) requireNamespace(x, quietly = TRUE))

  suggest_to_install <- suggests[!suggests_installed]

  n_pkg <- length(suggest_to_install)


  if (n_pkg > 0) {
    message(
      "Following ", ifelse(n_pkg > 1, "packages are", "package is"),
      " needed to run the app but ",
      ifelse(n_pkg > 1, "are", "is"), " not installed:"
    )
    cat("", paste("-", suggest_to_install), "", sep = "\n")

    out <- utils::menu(
      choices = c("Yes", "No"),
      title = message("Do you want to install ", ifelse(n_pkg > 1, "them", "it"), "?")
    )

    if (out == 1L) {
      utils::install.packages(pkgs = suggest_to_install, ...)
    } else {
      stop("The interactive Shiny app could not be started because of missing dependencies.", call. = FALSE)
    }
  }
}
