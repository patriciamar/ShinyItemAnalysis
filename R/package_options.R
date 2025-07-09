#' Options consulted by ShinyItemAnalysis
#'
#' The package and interactive `{shiny}` app consult several options that you
#' can easily set via [options()]. Moreover, there is some behavior that can be
#' changed through environment variables.
#'
#'
#' # Options
#'
#' Options are set with `options(<option> = <value>)`.
#'
#' - `sia.disable_modules`: You can completely disable SIA modules by setting
#' this to `TRUE`.
#'
#' - `sia.modules_repo`: This is the URL for a CRAN-like repository that the app
#' uses to retrieve information about available module packages.
#'
#' - `sia.offer_modules`: If set to `TRUE` (the default), calling `run_app()`
#' will check for the available SIA modules on the official repository and offer
#' to install those module packages that are not installed yet.
#'
#' # Environment variables
#'
#' You can set this variable system-wide or use `R` or project-wise `.Renviron`
#' file. For more details, please navigate to [the R documentation][Startup].
#'
#'  - `SIA_MODULES_DEBUG`: Setting this to `TRUE` provides a verbose description
#' of SIA modules-related processes. Useful only for debugging purposes.
#'
#'  - `SIA_MODULES_FORCE_GUI_INSTALLATION`: When the app is running on
#' shiny-server, interactive module installation within the app is not allowed
#' by default. Setting this variable to `TRUE` will override this restriction
#' and enable module installation in the app.
#'
#' @name ShinyItemAnalysis_options
NULL


# wrappers around getOptions ----------------------------------------------

sm_repo <- function() {
  getOption("sia.modules_repo", "https://applstat.github.io/SIArepo/")
}

# use this for repos arg in install.packages
# wrap inside a function to always get the current `repos` R option
sm_installation_repos <- function() {
  orig_repos <- getOption("repos")

  # normal R session outside of RStudio have repos set to "@CRAN@"
  # which enforces to choose the mirror interactively
  # this cannot be done when SIA runs as a background jobs
  # so we set the default CRAN mirror to the cloud one
  if (orig_repos[1L] == "@CRAN@" || is.null(orig_repos)) {
    orig_repos <- c(CRAN = "https://cloud.r-project.org/")
  }

  unique(
    c(
      sm_repo(),
      orig_repos
    )
  )
}

sm_disabled <- function() {
  getOption("sia.disable_modules", FALSE)
}

sm_offer <- function() {
  getOption("sia.offer_modules", TRUE)
}
