#' Range-restricted reliability with intra-class correlation
#'
#' Function estimating reliability with intra-class correlation for the complete
#' or for the range-restricted sample.
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
#'   See \code{\link[lme4:bootMer]{lme4::bootMer}} for more detail.
#'
#' @returns A \code{data.frame} with the following columns: \item{n_sel}{number
#'   of ratees selected/subsetted.} \item{prop_sel}{proportion of ratees
#'   selected.} \item{dir}{direction of range-restriction. \code{NA} if range is
#'   effectively not restricted (100% used).} \item{VarID}{variance due to
#'   ratee, "true variance", between-group variance.} \item{VarResid}{residual
#'   variance.} \item{VarTotal}{total variance.} \item{ICC1}{single-rater
#'   inter-rater reliability.} \item{ICC1_LCI}{lower bound of the confidence
#'   interval for \code{ICC1}.} \item{ICC1_UCI}{upper bound of the confidence
#'   interval for \code{ICC1}.} \item{ICC3}{multiple-rater inter-rater
#'   reliability.} \item{ICC3_LCI}{lower bound of the confidence interval for
#'   \code{ICC3}.} \item{ICC3_UCI}{upper bound of the confidence interval for
#'   \code{ICC3}.}
#'
#'
#' @author
#' Patricia Martinkova \cr
#' Institute of Computer Science of the Czech Academy of Sciences \cr
#' \email{martinkova@@cs.cas.cz}
#'
#' Jan Netik \cr
#' Institute of Computer Science of the Czech Academy of Sciences \cr
#' \email{netik@@cs.cas.cz}
#'
#' @references
#' Erosheva, E., Martinkova, P., & Lee, C. (2021a). When zero may not be zero: A
#' cautionary note on the use of inter-rater reliability in evaluating grant
#' peer review. Journal of the Royal Statistical Society - Series A. Accepted.
#'
#' Erosheva, E., Martinkova, P., & Lee, C. (2021b). Supplementary material for
#' When zero may not be zero: A cautionary note on the use of inter-rater
#' reliability in evaluating grant peer review.
#'
#' @examples
#' # loading AIBS dataset
#' data(AIBS, package = "ShinyItemAnalysis")
#'
#' # ICC for the whole sample
#' ICCrestricted(Data = AIBS, case = "ID", var = "Score", rank = "ScoreRankAdj")
#'
#' # ICC for the range-restricted sample considering 80% of top ratees
#' ICCrestricted(Data = AIBS, case = "ID", var = "Score", rank = "ScoreRankAdj",
#'               sel = 0.8)
#'
#' @importFrom lme4 lmer VarCorr bootMer
#' @importFrom stats sigma
#' @importFrom dplyr arrange group_by ungroup mutate
#' @importFrom tidyr nest unnest
#' @importFrom tibble tibble rowid_to_column
#' @importFrom stats quantile
#' @importFrom rlang .data
#'
#' @export
#'
ICCrestricted <- function(Data, case, var, rank = NULL,
                          dir = "top", sel = 1, nsim = 100, ci = .95, seed = NULL) {
  if (is.null(Data[[case]])) stop("Case variable '", case, "' not present in Data.", call. = FALSE)
  if (is.null(Data[[var]])) stop("Dependent variable '", var, "' not present in Data.", call. = FALSE)

  sel_max <- Data[[case]] %>% unique() %>% length()

  if (sel <= 1) {
    sel <- round(sel * sel_max)
    if (sel < 2) {
      sel <- 2
      warning(
        "There must be at least 2 cases in the subset.\n",
        "A minimum proportion, i.e., ", round(2 / sel_max, 3), " was used.",
        call. = FALSE
      )
    }
  }

  if (sel != sel_max) {
    if (is.null(rank)) {
      Data <- Data %>%
        group_by(.data[[case]]) %>%
        mutate(.ms = mean(.data[[var]], na.rm = TRUE)) %>%
        arrange(.data[[".ms"]]) %>%
        nest() %>%
        rowid_to_column(".rank") %>%
        unnest(cols = .data$data) %>%
        ungroup()

      rank <- ".rank"
    }

    if (is.null(Data[[rank]])) stop("Rank variable '", rank, "' not present in Data.", call. = FALSE)

    dir <- match.arg(dir, c("top", "bottom"))

    Data <- switch(dir,
      top = Data[Data[[rank]] <= sel, ],
      bottom = Data[Data[[rank]] > sel_max - sel, ]
    )
  }

  formula <- formula(paste0(var, " ~ 1 + (1 | ", case, ")"))

  model <- lmer(formula = formula, data = Data)

  VarID <- as.numeric(VarCorr(model)[case])
  VarResid <- sigma(model)^2
  VarTotal <- VarID + VarResid

  ICC1 <- VarID / VarTotal
  ICC3 <- VarID / (VarID + VarResid / 3)

  bs <- bootMer(
    model,
    function(mm) {
      c(as.numeric(VarCorr(mm)), sigma(mm)^2)
    }, nsim, seed
  )$t

  probs <- c(1 - ci, 1 + ci) / 2

  bICC1 <- bs[, 1] / rowSums(bs)
  ICC1_CI <- quantile(bICC1, probs, names = FALSE)

  bICC3 <- bs[, 1] / (bs[, 1] + bs[, 2] / 3)
  ICC3_CI <- quantile(bICC3, probs, names = FALSE)

  out <- data.frame(
    n_sel = sel,
    prop_sel = sel / sel_max,
    dir = ifelse(sel == sel_max, NA, dir),
    VarID,
    VarResid,
    VarTotal,
    ICC1,
    ICC1_LCI = ICC1_CI[1],
    ICC1_UCI = ICC1_CI[2],
    ICC3,
    ICC3_LCI = ICC3_CI[1],
    ICC3_UCI = ICC3_CI[2]
  )

  out
}
