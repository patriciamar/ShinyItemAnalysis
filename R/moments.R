#' Compute standardized moments (internal function)
#'
#' @description Internal function substituting the \code{moments} package. Only
#'   third (skewness) and fourth (kurtosis) moments are included.
#'
#' @keywords internal
#' @noRd
skewness <- function(x) {
  n <- length(x)
  (sum((x - mean(x, na.rm = TRUE))^3, na.rm = TRUE) / n) / (sum((x - mean(x, na.rm = TRUE))^2, na.rm = TRUE) / n)^(3 / 2)
}

kurtosis <- function(x) {
  n <- length(x)
  n * sum((x - mean(x, na.rm = TRUE))^4, na.rm = TRUE) / (sum((x - mean(x, na.rm = TRUE))^2, na.rm = TRUE)^2)
}
