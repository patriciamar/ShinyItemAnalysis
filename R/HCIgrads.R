#' Homeostasis Concept Inventory Dataset
#'
#' @description \code{HCIgrads} dataset consists of the responses of 10 graduate students
#' to Homeostasis Concept Inventory (HCI) multiple-choice test. It contains answers to
#' 20 multiple-choice items, scored items, total score, and type of school.
#'
#' @usage data(HCIgrads)
#'
#' @author
#' Jenny L. McFarland \cr
#' Biology Department, Edmonds Community College
#'
#' @references
#' McFarland, J. L., Price, R. M., Wenderoth, M. P., Martinkova, P., Cliff, W., Michael, J., ... & Wright, A. (2017).
#' Development and validation of the homeostasis concept inventory. CBE-Life Sciences Education, 16(2), ar35.
#'
#' @keywords datasets
#'
#' @seealso \code{\link{HCIdata}}, \code{\link{HCIkey}}
#'
#' @format \code{HCIgrads} is a \code{data.frame} consisting of 10 observations on the 42 variables.
#' \describe{
#' \item{A1-A20}{multiple-choice items of the HCI test}
#' \item{QR1-QR20}{scored items of the HCI test, \code{"0"} incorrect, \code{"1"} correct}
#' \item{total}{total score}
#' }
"HCIgrads"
