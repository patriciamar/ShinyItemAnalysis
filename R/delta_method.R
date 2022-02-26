#' Get standard errors using delta method approximation
#'
#' @param formula a list of RHS-only formula(s)
#' @param mean estimated mean of X
#' @param cov  estimated variance-covariance matrix of X
#'
#' @return SEs vector
#' @keywords internal
#'
#' @importFrom stats deriv
#'
delta_ses <- function(formula, mean, cov) {
  cov <- as.matrix(cov)
  formula <- as.list(formula)

  syms <- paste0("x", seq_along(mean))
  for (i in seq_along(mean)) assign(syms[i], mean[i])

  gdashmu <- t(sapply(formula, function(x) {
    as.numeric(attr(eval(deriv(x, syms)), "gradient"))
  }))

  new.covar <- gdashmu %*% cov %*% t(gdashmu)
  sqrt(diag(new.covar))
}
