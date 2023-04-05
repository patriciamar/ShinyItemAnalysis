#' Recognize and recode not-reached responses
#'
#' @aliases recode_nr
#'
#' @description `recode_nr()` function recognizes and recodes not-reached
#' responses, i.e., missing responses to items such that all subsequent
#' items are missed as well by the respondent.
#'
#' @param Data matrix or data.frame: object to be recoded, must include only
#'   items columns and no additional information
#' @param nr_code single character, integer or numeric: specifying how should be
#'   recognized not-reached responses coded (default is `99`)
#' @param df deprecated. Use argument `Data` instead.
#'
#' @return A `data.frame` object.
#'
#' @author
#' Jan Netik \cr
#' Institute of Computer Science of the Czech Academy of Sciences \cr
#' \email{netik@@cs.cas.cz}
#'
#' Patricia Martinkova \cr
#' Institute of Computer Science of the Czech Academy of Sciences \cr
#' \email{martinkova@@cs.cas.cz} \cr
#'
#' @seealso
#' [ItemAnalysis()]
#'
#' @examples
#' HCImissed <- HCI[, 1:20]
#'
#' # simulate skipped (missed) and not-reached items in HCI dataset
#' set.seed(4211)
#' for (i in 1:150) {
#'   # not-reached (minimum at 10th item, maximum at 20th)
#'   HCImissed[sample(1:nrow(HCImissed), 1), seq(sample(10:20, 1), 20)] <- NA
#'
#'   # missed with random location
#'   HCImissed[sample(1:nrow(HCImissed), 1), sample(1:20, 1)] <- NA
#' }
#'
#' summary(HCImissed)
#'
#' HCImissedNR <- recode_nr(HCImissed, nr_code = 99)
#' head(HCImissedNR)
#' summary(HCImissedNR)
#' @export
recode_nr <- function(Data, nr_code = 99, df) {
  # deprecated args handling
  if (!missing(df)) {
    warning("Argument 'df' is deprecated; please use 'Data' instead.",
      call. = FALSE
    )
    Data <- df
  }

  # coerce to data.frame (tibble cannot be subset by a matrix)
  if (inherits(Data, "tbl_df")) Data <- as.data.frame(Data)

  if (any(sapply(Data, is.factor))) {
    for (i in 1:ncol(Data)) {
      if (!is.null(levels(Data[, i]))) {
        levels(Data[, i]) <- c(levels(Data[, i]), nr_code)
      }
    }
  }


  mask_nr <- function(vec, i) {
    out <- logical(i)
    while (i > 0 && vec[i]) { # don't let i == 0 (occurs when all values are NA)
      out[i] <- TRUE
      i <- i - 1
    }
    out
  }

  mask <- t(apply(is.na(Data), 1, mask_nr, i = ncol(Data)))

  Data[mask] <- nr_code

  return(Data)
}
