#' Recognize and recode not-reached responses
#'
#' @aliases recode_nr
#'
#' @description \code{recode_nr()} function recognizes and recodes not-reached
#' responses, i.e., missing responses to items such that all subsequent
#' items are missed as well by the respondent.
#'
#' @param Data matrix or data.frame: object to be recoded, must include only
#'   items columns and no additional information
#' @param nr_code single character, integer or numeric: specifying how should be
#'   recognized not-reached responses coded (default is \code{99})
#' @param df deprecated. Use argument \code{Data} instead.
#'
#' @return A \code{data.frame} object.
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
#' \code{\link{ItemAnalysis}}
#'
#' @examples
#' data(HCI, package = "ShinyItemAnalysis")
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

  nr_count <- apply(Data, 1, function(x) {
    with(rle(is.na(unlist(x))), {
      ifelse(values[length(values)],
        lengths[values][length(lengths[values])],
        NA
      )
    })
  })

  indx <- which(!is.na(nr_count))
  rows <- rep(indx, nr_count[indx])
  cols <-
    unlist(lapply(nr_count[indx], function(x) {
      seq(ncol(Data) - x + 1, ncol(Data))
    }))

  arr_indx <- cbind(rows, cols)

  Data[arr_indx] <- nr_code

  return(Data)
}
