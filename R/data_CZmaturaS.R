#' CZmatura dataset - sample
#'
#' @description The `CZmaturaS` dataset comes from a matura exam in
#'   mathematics. The exam was assigned in 2019 to students in Grade 13, at the end of
#'   their secondary education. This is a random sample of 2,000 students from a
#'   total of 15,702. Original data available from
#'   [https://cermat.gov.cz/](https://cermat.gov.cz/).
#'
#' @keywords datasets
#'
#' @seealso [CZmatura()]
#'
#' @format `CZmatura` is a `data.frame` consisting of 2,000
#'   observations on 75 variables.
#'   \describe{
#'   \item{SchType}{School type code. }
#'   \item{FirstAtt}{First attempt; `"1"` yes, `"0"` no. }
#'   \item{SchTypeGY}{School type gymnasium; `"1"` yes, `"0"` no. }
#'   \item{o1 -- o26.2}{Item answers. }
#'   \item{b1 -- b26}{Scored item answers. }
#'   \item{Total}{Total score, calculated as sum of item scores (0 - 50). }
#'   \item{IRTscore}{Score estimated from GPCM/2PL model. }
#'   \item{IRTscoreSE}{SE of score estimated from GPCM/2PL model. }
#' }
"CZmaturaS"
