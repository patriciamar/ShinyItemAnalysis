#' Homeostasis concept inventory full dataset
#'
#' @description \code{HCIdata} dataset consists of the responses of 669 students
#'   (405 males, 246 females, 18 without gender specification) to Homeostasis
#'   Concept Inventory (HCI) multiple-choice test. It contains answers to 20
#'   multiple-choice items, scored items, total score, gender membership,
#'   identifier whether students plan to major in science, study year, minority
#'   membership, identifier whether English is the student's first language, and
#'   type of school.
#'
#' @usage data(HCIdata)
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
#' \code{\link{HCI}} for HCI dichotomous dataset\cr
#' \code{\link{HCItest}} for HCI multiple-choice dataset\cr
#' \code{\link{HCIkey}} for key of correct answers for HCI\cr
#' \code{\link{HCIgrads}}  for HCI dataset of graduate students\cr
#' \code{\link{HCIprepost}} for HCI pretest and posttest scores\cr
#' \code{\link{HCItestretest}} for HCI test-retest dataset
#'
#' @format \code{HCIdata} is a \code{data.frame} consisting of 669 observations
#'   on the 47 variables.
#'   \describe{
#'   \item{A1-A20}{Multiple-choice items of the HCI test. }
#'   \item{QR1-QR20}{Scored items of the HCI test, \code{"0"} incorrect,
#'   \code{"1"} correct. }
#'   \item{total}{Total test score. }
#'   \item{gender}{Gender membership, \code{"M"} males, \code{"F"} females,
#'   \code{"none"} undisclosed. }
#'   \item{major}{Identifier whether students plans to major in the life sciences. }
#'   \item{yearc5}{Study year. }
#'   \item{minority}{Minority membership, \code{"maj"} majority, \code{"min"}
#'   minority, \code{"none"} undisclosed. }
#'   \item{EnglishF}{Identifier whether English is the student's first language. }
#'   \item{typeS}{Course type, \code{"allied"} allied health, \code{"majors"}
#'   physiology courses for
#'   science majors, \code{"mixed majors"} courses for non-majors. }
#'   \item{typeSCH}{Type of school, \code{"AC"} associate's college, \code{"BCAS"}
#'   baccalaureate college: arts and sciences focus, \code{"R1"} research
#'   university, \code{"MCU"} master's college and university. } }
"HCIdata"
