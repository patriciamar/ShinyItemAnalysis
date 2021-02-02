#' Homeostasis concept inventory dataset of graduate students
#'
#' @description \code{HCIgrads} dataset consists of the responses of 10 graduate
#'   students to Homeostasis Concept Inventory (HCI) multiple-choice test. It
#'   contains answers to 20 multiple-choice items, scored items, and total test
#'   score.
#'
#' @usage data(HCIgrads)
#'
#' @author Jenny L. McFarland \cr Biology Department, Edmonds Community College
#'
#' @references McFarland, J. L., Price, R. M., Wenderoth, M. P., Martinkova, P.,
#' Cliff, W., Michael, J., ... & Wright, A. (2017). Development and validation
#' of the homeostasis concept inventory. CBE-Life Sciences Education, 16(2),
#' ar35. \doi{10.1187/cbe.16-10-0305}
#'
#' @keywords datasets
#'
#' @seealso
#' \code{\link{HCIdata}} for HCI full dataset\cr
#' \code{\link{HCI}} for HCI dichotomous dataset\cr
#' \code{\link{HCItest}} for HCI multiple-choice dataset\cr
#' \code{\link{HCIkey}} for key of correct answers for HCI\cr
#' \code{\link{HCIprepost}} for HCI pretest and posttest scores\cr
#' \code{\link{HCItestretest}} for HCI test-retest dataset
#'
#' @format \code{HCIgrads} is a \code{data.frame} consisting of 10 observations
#'   on the 42 variables.
#'   \describe{
#'   \item{A1-A20}{Multiple-choice items of the HCI test. }
#'   \item{QR1-QR20}{Scored items of the HCI test, \code{"0"} incorrect,
#'   \code{"1"} correct. }
#'   \item{total}{Total test score. }
#'   }
"HCIgrads"
