#' The Eysenck Personality Inventory Impulsivity Subscale
#'
#' The data came from a published study and was kindly provided by Dr. Ferrando.
#' A group of 1,033 undergraduate students were asked to check on a 112 mm line
#' segment with two end points (almost never, almost always) using their own
#' judgement for the five items taken from the Spanish version of the EPI-A
#' impulsivity subscale. The direct item score was the distance in mm of the
#' check mark from the left end point (Ferrando, 2002).
#'
#' @format A data frame with 1033 observations on the following 5 variables.
#'   The sixth variable is a total score (i.e. the sum of the items).
#' \describe{
#'   \item{\code{Item 1}}{Longs for excitement}
#'   \item{\code{Item 2}}{Does not stop and think things over before doing anything}
#'   \item{\code{Item 3}}{Often shouts back when shouted at}
#'   \item{\code{Item 4}}{Likes doing things in which he/she has to act quickly}
#'   \item{\code{Item 5}}{Tends to do many things at the same time}
#'   \item{\code{score}}{Total score for the aforementioned items}
#' }
#'
#' @source Reexport from `EstCRM` package with added total scores.
#'
#' @references Ferrando, P. J. (2002). Theoretical and Empirical Comparison
#'   between Two Models for Continuous Item Responses. \emph{Multivariate
#'   Behavioral Research}, *37*(4), 521--542.
#'
#'   Zopluoglu C (2022). _EstCRM: Calibrating Parameters for the Samejima's Continuous IRT Model_.
#'   R package version 1.5, <https://CRAN.R-project.org/package=EstCRM>.
#'
"EPIA"
