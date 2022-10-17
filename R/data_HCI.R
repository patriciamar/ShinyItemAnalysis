#' Homeostasis Concept Inventory dichotomous dataset
#'
#' `HCI` dataset consists of the dichotomously scored responses of 651
#' students (405 males, 246 females) to Homeostasis Concept Inventory (HCI)
#' multiple-choice test. It contains 20 items, vector of gender membership and
#' identificator whether students plan to major in life sciences.
#'
#' @author Jenny L. McFarland \cr Biology Department, Edmonds Community College
#'
#' @references McFarland, J. L., Price, R. M., Wenderoth, M. P., Martinkova, P.,
#'   Cliff, W., Michael, J., ... & Wright, A. (2017). Development and validation
#'   of the homeostasis concept inventory. CBE-Life Sciences Education, 16(2),
#'   ar35. \doi{10.1187/cbe.16-10-0305}
#'
#' @keywords datasets
#'
#' @seealso
#' [HCItest] for HCI multiple-choice dataset\cr
#' [HCIkey] for key of correct answers for HCI\cr
#' [HCIdata] for HCI full dataset\cr
#' [HCIlong] for HCI in a long format\cr
#' [HCIgrads]  for HCI dataset of graduate students\cr
#' [HCIprepost] for HCI pretest and posttest scores\cr
#' [HCItestretest] for HCI test-retest dataset\cr
#'
#' @format `HCI` is a `data.frame` consisting of 651 observations on
#'   the 22 variables.
#'   \describe{
#'   \item{Item1-Item20}{Dichotomously scored items of the HCI test. }
#'   \item{gender}{Gender membership, `"0"` males, `"1"` females. }
#'   \item{major}{Identificator whether student plans to major in the life
#'   sciences. }
#'   \item{total}{Total score}
#' }
"HCI"


#' Homeostasis Concept Inventory in a long format
#'
#' `HCIlong` dataset consists of the dichotomously scored responses of 651
#' students (405 males, 246 females) to Homeostasis Concept Inventory (HCI)
#' multiple-choice test. It contains 20 items (**in a long format**), vector of
#' gender membership and identificator whether students plan to major in life
#' sciences.
#'
#' @author Jenny L. McFarland \cr Biology Department, Edmonds Community College
#'
#' @references McFarland, J. L., Price, R. M., Wenderoth, M. P., Martinkova, P.,
#'   Cliff, W., Michael, J., ... & Wright, A. (2017). Development and validation
#'   of the homeostasis concept inventory. CBE-Life Sciences Education, 16(2),
#'   ar35. \doi{10.1187/cbe.16-10-0305}
#'
#' @keywords datasets
#'
#' @seealso
#' [HCI] for HCI dichotomous dataset (in a wide format)\cr
#' [HCItest] for HCI multiple-choice dataset\cr
#' [HCIkey] for key of correct answers for HCI\cr
#' [HCIdata] for HCI full dataset\cr
#' [HCIgrads]  for HCI dataset of graduate students\cr
#' [HCIprepost] for HCI pretest and posttest scores\cr
#' [HCItestretest] for HCI test-retest dataset\cr
#'
#' @format `HCIlong` is a `data.frame` consisting of 13,020 rows and 5
#'   variables.
#'   \describe{
#'   \item{id}{Row number of the original observation in a wide format.}
#'   \item{item}{Name of the item the rating is for.}
#'   \item{rating}{Response to the item.}
#'   \item{gender}{Gender membership, `"0"` males, `"1"` females. }
#'   \item{major}{Identificator whether student plans to major in the life
#'   sciences.}
#'   \item{total}{Total score}
#' }
"HCIlong"
