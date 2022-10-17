#' Homeostasis concept inventory multiple-choice dataset
#'
#' @description `HCItest` dataset consists of the responses of 651
#'   students (405 males, 246 females) to Homeostasis Concept Inventory (HCI)
#'   multiple-choice test. It containts 20 items, vector of gender membership
#'   and identificator whether students plan to major in life sciences.
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
#' [HCI] for HCI dichotomous dataset\cr
#' [HCIkey] for key of correct answers for HCI\cr
#' [HCIdata] for HCI full dataset\cr
#' [HCIlong] for HCI in a long format\cr
#' [HCIgrads]  for HCI dataset of graduate students\cr
#' [HCIprepost] for HCI pretest and posttest scores\cr
#' [HCItestretest] for HCI test-retest dataset\cr
#'
#' @format `HCItest` is a `data.frame` consisting of 651 observations
#'   on the 22 variables.
#'   \describe{
#'   \item{Item1-Item20}{Multiple-choice items of the HCI test. }
#'   \item{gender}{Gender membership, `"0"` males, `"1"` females. }
#'   \item{major}{Identificator whether student plans to major in the life
#'   sciences. } }
"HCItest"
