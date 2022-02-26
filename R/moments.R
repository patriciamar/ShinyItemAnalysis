#' Compute standardized moments (internal function)
#'
#' @description Internal function substituting the `moments` package. Only
#'   third (skewness) and fourth (kurtosis) moments are included.
#'
#' @keywords internal
#' @noRd
skewness <- function(x) {
  n <- length(x)
  dev <- x - mean(x, na.rm = TRUE)

  (sum(dev^3, na.rm = TRUE) / n) / (sum(dev^2, na.rm = TRUE) / n)^(3 / 2)
}

kurtosis <- function(x) {
  n <- length(x)
  dev <- x - mean(x, na.rm = TRUE)
  n * sum(dev^4, na.rm = TRUE) / (sum(dev^2, na.rm = TRUE)^2)
}
