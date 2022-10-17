#' CZmatura dataset
#'
#' @description The `CZmatura` dataset comes from matura exam in
#'   mathematics. The exam was assigned in 2019 to students from Grade 13, at
#'   the end of their secondary education. Original data available from
#'   [https://cermat.cz/](https://cermat.cz/).
#'
#' @keywords datasets
#'
#' @seealso
#' [CZmaturaS()]
#'
#' @format `CZmatura` is a `data.frame` consisting of 15,702 observations on
#'   75 variables.
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
"CZmatura"
