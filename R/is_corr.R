#' Test if Object is Correlation
#'
#' @param Data An object coercible to `matrix` to test for.
#'
#' @keywords internal
#' @noRd
is_corr <- function(Data) {
  dm <- dim(Data)

  if (dm[1L] != dm[2L]) {
    return(FALSE)
  }

  Data <- as.matrix(Data)

  if (!isSymmetric(Data)) {
    return(FALSE)
  }

  if (prod(diag(Data)) != 1) {
    return(FALSE)
  }

  if ((min(Data, na.rm = TRUE) < -1) || (max(Data, na.rm = TRUE) > 1)) {
    return(FALSE)
  }

  TRUE
}
