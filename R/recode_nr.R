#' Recognize and recode not-reached responses
#'
#' @aliases recode_nr
#'
#' @description \code{recode_nr()} function recognizes and recodes not-reached
#' responses, i.e. missing responses to items such that all subsequent
#' items are missed as well by the respondent.
#'
#' @param df matrix or data.frame: object to be recoded, must include only items
#' columns and no additional information
#' @param nr_code single character, integer or numeric: specifying how should
#' be recognized not-reached responses coded (default is \code{99})
#'
#' @usage recode_nr(df, nr_code = 99)
#'
#' @return The same class as input object, see \code{df}.
#'
#' @author
#' Jan Netik \cr
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
#' # not-reached (minimum at 10th item, maximum at 20th)
#' HCImissed[sample(1:nrow(HCImissed), 1), seq(sample(10:20, 1), 20)] <- NA
#'
#' # missed with random location
#' HCImissed[sample(1:nrow(HCImissed), 1), sample(1:20, 1)] <- NA
#' }
#'
#' summary(HCImissed)
#'
#' HCImissedNR <- recode_nr(HCImissed, nr_code = 99)
#' head(HCImissedNR)
#' summary(HCImissedNR)
#'
#' @export
recode_nr <- function(df, nr_code = 99) {

  if (any(sapply(df, is.factor))) {
    for (i in 1:ncol(df)) {
      if (!is.null(levels(df[, i]))) {
        levels(df[, i]) <- c(levels(df[, i]), nr_code)
      }
    }
  }

  nr_count <- apply(df, 1, function(x) {
    with(rle(is.na(unlist(x))), {
      ifelse(values[length(values)],
             lengths[values][length(lengths[values])],
             NA)
    })
  })

  nr_indices <- lapply(nr_count, function(x) {
    if (!is.na(x)) {
      seq(ncol(df) - x + 1, ncol(df))
    }
  })

  for (i in 1:nrow(df)) {
    df[i, nr_indices[[i]]] <- nr_code
  }

  return(df)

}
