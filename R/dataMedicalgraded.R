#' Graded Dataset of Admission Test to Medical School
#'
#' @md # allow RMarkdown formating usage
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
#' @author Cestmir Stuka \cr
#' First Faculty of Medicine, Charles University \cr
#'
#' Martin Vejrazka \cr
#' First Faculty of Medicine, Charles University \cr
#'
#' Patricia Martinkova \cr
#' Institute of Computer Science of the Czech Academy of Sciences \cr
#' \email{martinkova@@cs.cas.cz} \cr
#'
#' @references Stuka, C. Vejrazka, M., Martinkova, P. Komenda, M. & Stepanek, L.
#' (2016). The Use of  Test and Item Analisis for Improvement of Tests. Workshop
#' held at conference MEFANET, 2016, Brno, Czech Republic.
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
