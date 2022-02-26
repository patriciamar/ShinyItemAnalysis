#' Homeostasis concept inventory test-retest dataset
#'
#' @description `HCItestretest` dataset consists of the responses of 45
#'   students to Homeostasis Concept Inventory (HCI). It contains answers to 20
#'   multiple-choice items, scored items, identifier of test/retest, total
#'   score, gender membership and identifier whether students plan to major in
#'   life sciences. The data are organized so that each pair of subsequent rows
#'   belongs to one student. Students took no courses on homeostasis between the
#'   test and retest.
#'
#' @usage data(HCItestretest)
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
#' [HCIdata()] for HCI full dataset\cr
#' [HCI()] for HCI dichotomous dataset\cr
#' [HCItest()] for HCI multiple-choice dataset\cr
#' [HCIkey()] for key of correct answers for HCI\cr
#' [HCIgrads()]  for HCI dataset of graduate students\cr
#' [HCIprepost()] for HCI pretest and posttest scores
#'
#' @format `HCItestretest` is a `data.frame` consisting of 90
#'   observations on the 44 variables.
#'   \describe{
#'   \item{A1-A20}{Multiple-choice items of the HCI test. }
#'   \item{QR1-QR20}{Scored items of the HCI test, `"0"` incorrect,
#'   `"1"` correct. }
#'   \item{test}{Identifier of test vs retest, `"test"` test,
#'   `"retest"` retest after. }
#'   \item{total}{Total test score. }
#'   \item{gender}{Gender membership, `"M"` male, `"F"` female. }
#'   \item{major}{Identifier whether student plans to major in the life sciences. } }
"HCItestretest"
