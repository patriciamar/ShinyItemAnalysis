#' Graded dataset of admission test to medical school
#'
#' @md # allow RMarkdown formatting usage
#'
#' @description The \code{dataMedicalgraded} dataset consists of the responses
#'   of 2,392 subjects (750 males, 1,633 females and 9 subjects without gender
#'   specification) to multiple-choice admission test to a medical school. It
#'   contains 100 items. Each item is graded with 0 to 4 points. Maximum of 4
#'   points were set if all correct answers and none of incorrect answers were
#'   selected.
#'
#' @usage data(dataMedicalgraded)
#'
#' @references Martinkova, P., & Drabinova, A. (2018). ShinyItemAnalysis for
#' teaching psychometrics and to enforce routine analysis of educational tests.
#' The R Journal, 10(2), 503--515, \doi{10.32614/RJ-2018-074}
#'
#' @source Stuka, C., Vejrazka, M., Martinkova, P., Komenda, M., & Stepanek, L. (2016).
#' The use of test and item analysis for improvement of tests. Workshop held at
#' conference MEFANET, 2016, Brno, Czech Republic.
#'
#' @keywords datasets
#'
#' @seealso \code{\link{dataMedical}}, \code{\link{dataMedicaltest}},
#'   \code{\link{dataMedicalkey}}
#'
#' @format A `dataMedicalgraded` is a `data.frame` consisting of 2,392
#'   observations on the following 102 variables.
#' \describe{
#' \item{X}{The first 100 columns represent ordinal item scores of the test. }
#' \item{gender}{Variable describing gender; values `"0"` and `"1"` refer to
#' males and females.}
#' \item{StudySuccess}{Criterion variable; value `"1"` means that student
#' studies standardly, `"0"` otherwise (e.g., leaving or interrupting studies).}
#' }
"dataMedicalgraded"
