#' Compute Cronbach alpha with confidence interval
#'
#' So far internal function.
#'
#' @param Data *data.frame* or *matrix*, item data, `NA` gets excluded
#'   automatically.
#' @param ci *logical*, whether to compute CI or not. Defaults to `TRUE`.
#' @param ci_lvl *numeric* ranging from 0 to 1, a confidence level to construct
#'   CI for. Defaults to `.95`.
#'
#' @return A list with \eqn{\alpha} estimate and optionally CI.
#'
#' @md
#' @importFrom stats qf var
#' @keywords internal
#'
#' @examples
#' ShinyItemAnalysis:::cronbach_alpha(HCI[, 1:20])
#'
cronbach_alpha <- function(Data, ci = TRUE, ci_lvl = .95) {
  x <- na.exclude(as.matrix(Data))
  k <- ncol(x)
  n <- nrow(x)

  out <- list()

  # compute alpha
  out$estimate <- k / (k - 1) * (1 - sum(apply(x, 2, var)) / sum(var(x)))

  if (ci) {
    # assert ci_lvl
    if (ci_lvl < 0 || ci_lvl > 1) {
      stop("`ci_lvl` must be numeric between 0 and 1.", call. = FALSE
      )
    }

    # lower & upper CI bounds
    ci <- (1 - ci_lvl) / 2
    ci <- c(1 - ci, ci)

    # F-statistics for CI bounds
    Fdist <- qf(ci, n - 1, (k - 1) * (n - 1))

    # Feldt, Woodruff, & Salih, 1987, p. 95, formula 6
    out$ci <- 1 - (1 - out$estimate) * Fdist
  }

  return(out)
}
