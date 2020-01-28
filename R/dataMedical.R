#' Dichotomous Dataset of Admission Test to Medical School
#'
#' @md # allow RMarkdown formating usage
#'
#' @description The `dataMedical` dataset consists of the responses of 2,392 subjects
#' (750 males, 1,633 females and 9 subjects without gender specification) to admission test to
#' a medical school. It contains 100 items. A correct answer is coded as `"1"` and incorrect
#' answer as `"0"`. Missing answers were evaluated as incorrect, i.e. `"0"`.
#'
#' @usage data(dataMedical)
#'
#' @author
#' Cestmir Stuka \cr
#' First Faculty of Medicine, Charles University \cr
#'
#' Martin Vejrazka \cr
#' First Faculty of Medicine, Charles University \cr
#'
#' Patricia Martinkova \cr
#' Institute of Computer Science of the Czech Academy of Sciences \cr
#' \email{martinkova@@cs.cas.cz} \cr
#'
#' @references
#' Stuka, C. Vejrazka, M., Martinkova, P. Komenda, M. & Stepanek, L. (2016). The Use of Test and Item
#' Analysis for Improvement of Tests. Workshop held at conference MEFANET, 2016, Brno, Czech Republic.
#'
#' @keywords datasets
#'
#' @seealso \code{\link{dataMedicaltest}}, \code{\link{dataMedicalkey}}, \code{\link{dataMedicalgraded}}
#'
#' @format A `dataMedical` is a `data.frame` consisting of 2,392 observations on
#' the following 102 variables.
#' \describe{
#' \item{X}{the first 100 columns represent dichotomously scored items of the test.}
#' \item{gender}{variable describing gender; values `"0"` and `"1"` refer to males and females.}
#' \item{StudySuccess}{criterion variable; value `"1"` means that student studies standardly,
#' `"0"` otherwise (e.g. leaving or interrupting studies).}
#' }
"dataMedical"
