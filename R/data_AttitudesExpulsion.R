#' Attitudes towards the Expulsion of the Sudeten Germans (dataset)
#'
#' Dataset from Kolek et al. (2021) study investigating a video game's effects
#' on implicit and explicit attitudes towards depicted historical events in the
#' short- and long-term. As an intervention tool, a serious game
#' *Czechoslovakia 38–89: Borderlands* was utilized that deals with the
#' expulsion of the Sudeten Germans from the former Czechoslovakia after the
#' WWII. Data consists responses from 145 adults from two groups (experimental
#' and control group) on number of multi-item measurements.
#'
#' @keywords datasets
#' @encoding utf-8
#' @md
#'
#' @format A *data.frame* with `r nrow(AttitudesExpulsion)` rows and
#' `r ncol(AttitudesExpulsion)` variables:
#' \describe{
#'   \item{ID}{anonymous identifier}
#'   \item{Group}{C = control or E = experimental group}
#'   \item{Gender}{*factor*, `male` or `female`}
#'   \item{GenderF}{*integer*, 1 = female}
#'   \item{Merkel}{effect of Merkel speech between the posttest and the delayed
#'   posttest, range 0--5, where 0 stands for no effect, 5 for very significant
#'   effect}
#'   \item{Sudety}{*factor*, N = not originally from Czech Borderlands; Y =
#'   originally from Czech Borderlands}
#'   \item{Education}{*factor*, V = university; S = high school; Z = elementary
#'   school}
#'   \item{Education123}{*integer*, same as above, but coded as 3= university;
#'   2= high school; 1= elementary school, meaning higher the number, higher the
#'   education}
#'   \item{*PANASpn}{total PANAS score of positive and negative affect scales}
#'   \item{*PANASp}{total PANAS score of positive affect scale}
#'   \item{*PANASn}{total PANAS score of negative affect scale}
#'   \item{*Macro}{Macro attitude measurement}
#'   \item{*Micro}{Micro attitude measurement}
#'   \item{*IATeffect}{Single-Category Implicit association test score}
#' }
#'
#' Items beginning with an asterisk have following prefixes in the actual
#' dataset:
#' \describe{
#'   \item{pre}{pretest}
#'   \item{post}{immediate posttest}
#'   \item{del}{one month delayed posttest}
#'   \item{Post_Pre}{difference between posttest_pretest}
#'   \item{Del_Post}{difference between delayed posttest and posttest}
#' }
#'
#' @source{ Kolek, L., Šisler, V., Martinková, P., & Brom, C. (2021). Can video
#' games change attitudes towards history? Results from a laboratory experiment
#' measuring short- and long-term effects. *Journal of Computer Assisted
#' Learning*, *1--22*. \doi{10.1111/jcal.12575}}
#'
"AttitudesExpulsion"
