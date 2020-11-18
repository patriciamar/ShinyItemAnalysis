#' Homeostasis concept inventory dichotomous dataset
#'
#' @description (\code{HCI}) dataset consists of the dichotomously scored
#'   responses of 651 students (405 males, 246 females) to Homeostasis Concept
#'   Inventory (HCI) multiple-choice test. It containts 20 items, vector of
#'   gender membership and identificator whether students plan to major.
#' @usage data(HCI)
#'
#' @author Jenny L. McFarland \cr Biology Department, Edmonds Community College
#'
#' @references McFarland, J. L., Price, R. M., Wenderoth, M. P., Martinkova, P.,
#' Cliff, W., Michael, J., ... & Wright, A. (2017). Development and validation
#' of the homeostasis concept inventory. CBE-Life Sciences Education, 16(2),
#' ar35. \url{https://doi.org/10.1187/cbe.16-10-0305}
#'
#' @keywords datasets
#'
#' @seealso \code{\link{HCItest}}, \code{\link{HCIkey}}, \code{\link{HCIdata}},
#' \code{\link{HCIgrads}}, \code{\link{HCIprepost}}, \code{\link{HCItestretest}}
#'
#' @format \code{HCI} is a \code{data.frame} consisting of 651 observations on
#'   the 22 variables.
#'   \describe{
#'   \item{Item1-Item20}{Dichotomously scored items of the HCI test. }
#'   \item{gender}{Gender membership, \code{"0"} males, \code{"1"} females. }
#'   \item{major}{Identificator whether student plans to major in the life
#'   sciences. }
#' }
"HCI"
