#' Range-restricted reliability with intra-class correlation
#'
#' Estimates reliability with intra-class correlation for whole or
#' range-restricted samples using REML.
#'
#' @param Data \code{matrix} or \code{data.frame} which includes variables
#'   describing ID of ratees (specified in \code{case}), ratings (specified in
#'   \code{var}), and (optionally) rank of ratees (specified in \code{rank}).
#' @param case character: name of the variable in \code{Data} with ID of the
#'   ratee (subject or object being evaluated, such as a respondent, proposal,
#'   patient, applicant etc.)
#' @param var character: name of the variable in \code{Data} with the
#'   ratings/scores.
#' @param rank numeric: vector of ranks of ratees. If not provided, rank of
#'   ratee is calculated based on average rating based on \code{var} variable.
#' @param dir character: direction of range-restriction, available options are
#'   \code{"top"} (default) or \code{"bottom"}. Can be an unambiguous
#'   abbreviation (i.e., \code{"t"} or \code{"b"}).
#' @param sel numeric: selected number (given > 1) or percentage (given <= 1) of
#'   ratees. Default value is 1 (complete dataset).
#' @param nsim numeric: number of simulations for bootstrap confidence interval.
#'   Default value is 100.
#' @param ci numeric: confidence interval. Default value is 0.95.
#' @param seed seed for simulations. Default value is \code{NULL}, random seed.
#'   See \code{\link{lme4::bootMer}} for more detail.
#'
#' @returns A \code{data.frame} with the following columns: \item{n_sel}{number
#'   of ratees selected/subsetted.} \item{prop_sel}{proportion of ratees
#'   selected.} \item{dir}{direction of range-restriction.}
#'   \item{var_ratee}{variance due to ratee, "true variance", between-group
#'   variance.} \item{var_resid}{residual variance.} \item{var_total}{total
#'   variance.} \item{ICC1}{single-rater inter-rater reliability.}
#'   \item{ICC1_LCI}{lower bound of the confidence interval for \code{ICC1}.}
#'   \item{ICC1_UCI}{upper bound of the confidence interval for \code{ICC1}.}
#'   \item{ICC3}{multiple-rater inter-rater reliability.} \item{ICC3_LCI}{lower
#'   bound of the confidence interval for \code{ICC3}.} \item{ICC3_UCI}{upper
#'   bound of the confidence interval for \code{ICC3}.}
#'
#' @examples
#' # loading AIBS dataset
#' data(AIBS, package = "ShinyItemAnalysis")
#'
#' # ICC for the whole sample
#' ICCrestricted(
#'   Data = AIBS, case = "ID", var = "Score", rank = "ScoreRankAdj"
#' )
#' # ICC for the range-restricted sample considering 80% of top ratees
#' ICCrestricted(
#'   Data = AIBS, case = "ID", var = "Score", rank = "ScoreRankAdj", sel = 0.8
#' )
#' @importFrom lme4 lmer VarCorr bootMer
#' @importFrom dplyr between arrange group_by ungroup mutate
#' @importFrom tidyr nest unnest
#' @importFrom tibble tibble rowid_to_column
#' @importFrom stats quantile
#' @importFrom rlang .data
#'
#' @export
#'
ICCrestricted <- function(Data, case, var, rank = NULL,
                          dir = "top", sel = 1, nsim = 100, ci = .95, seed = NULL) {
  # if rank = NULL, compute rank based on Score
  if (is.null(rank)) {
    Data <- Data %>%
      group_by(.data[[case]]) %>%
      mutate(.mean_score = mean(.data[[var]], na.rm = TRUE)) %>%
      arrange(.mean_score) %>%
      nest() %>%
      rowid_to_column(".rank") %>%
      unnest(cols = .data$data) %>%
      ungroup()

    rank <- ".rank"
  }

  sel_max <- max(Data[[rank]], na.rm = TRUE)

  # if sel <= 1, assume percentage, convert to integer sel
  if (between(sel, 0, 1)) {
    sel <- round(sel * sel_max)
  }

  formula <- formula(paste0(var, " ~ 1 + (1 | ", case, ")"))

  dir <- match.arg(dir, c("top", "bottom"))

  Data <- switch(dir,
    top = Data[Data[[rank]] <= sel, ],
    bottom = Data[Data[[rank]] > sel_max - sel, ]
  )

  model <- lmer(formula = formula, data = Data)

  var_ratee <- as.numeric(VarCorr(model)[case])
  var_resid <- sigma(model)^2
  var_total <- var_ratee + var_resid

  ICC1 <- var_ratee / var_total
  ICC3 <- var_ratee / (var_ratee + var_resid / 3)

  bs <- bootMer(
    model,
    function(mm) {
      c(as.numeric(VarCorr(mm)), sigma(mm)^2)
    }, nsim, seed
  )$t

  probs <- c(1 - ci, 1 + ci) / 2

  bICC1 <- bs[, 1] / rowSums(bs)
  ICC1_CI <- quantile(bICC1, probs)

  bICC3 <- bs[, 1] / (bs[, 1] + bs[, 2] / 3)
  ICC3_CI <- quantile(bICC3, probs)

  data.frame(
    n_sel = sel,
    prop_sel = sel / sel_max,
    dir,
    var_ratee,
    var_resid,
    var_total,
    ICC1,
    ICC1_LCI = ICC1_CI[1],
    ICC1_UCI = ICC1_CI[2],
    ICC3,
    ICC3_LCI = ICC3_CI[1],
    ICC3_UCI = ICC3_CI[2]
  )
}
