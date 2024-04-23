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
#' will check for the available SIA modules at the official repository and offer
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
#' @name ShinyItemAnalysis_options
NULL



# wrappers around getOptions ----------------------------------------------

sm_repo <- function() {
  getOption("sia.modules_repo", "https://applstat.github.io/SIArepo/")
}

sm_disabled <- function() {
  getOption("sia.disable_modules", FALSE)
}

sm_offer <- function() {
  getOption("sia.offer_modules", TRUE)
}
