#' Czech matura dataset
#'
#' @description The \code{CZmatura} dataset comes from the Czech matura exam in
#'   mathematics. The exam is assigned in spring to students from Grade 13, at
#'   the end of their secondary education.
#'
#' @usage data(CZmatura)
#'
#' @keywords datasets
#'
#' @seealso
#' \code{\link{CZmaturaS}}
#'
#' @format \code{CZmatura} is a \code{data.frame} consisting of 15,702 observations on
#'   75 variables.
#'   \describe{
#'   \item{Project}{Project ID, \code{"MZ2019"} for all.  }
#'   \item{Exam}{Exam ID, \code{"MA"} for all. }
#'   \item{SchType}{School type code. }
#'   \item{FirstAtt}{First attempt; \code{"1"} yes, \code{"0"} no. }
#'   \item{SchTypeGY}{School type gymnasium; \code{"1"} yes, \code{"0"} no. }
#'   \item{o1 -- o26.2}{Item answers. }
#'   \item{b1 -- b26}{Scored item answers. }
#'   \item{Total}{Total score, calculated as sum of item scores (0 - 50). }
#'   \item{IRTscore}{Score estimated from GPCM/2PL model. }
#'   \item{IRTscoreSE}{SE of score estimated from GPCM/2PL model. }
#' }
"CZmatura"
