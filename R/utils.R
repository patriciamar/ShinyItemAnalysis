#' Remove columns that are empty
#'
#' @param .data data.frame
#'
#' @keywords internal
#'
#' @return cleaned df
#'
remove_empty_cols <- function(.data) {
  mask_keep <- colSums(!is.na(.data)) > 0
  .data[, mask_keep, drop = FALSE]
}


na_to_null <- function(x) {
  if (is.na(x)) NULL else x
}
