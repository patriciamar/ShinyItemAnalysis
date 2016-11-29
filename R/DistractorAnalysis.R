#' Function for item distractor analysis
#'
#' @aliases DistractorAnalysis
#'
#' @description Performs distractor analysis for each item and optional number of groups.
#'
#' @param data character: data matrix or data frame. See \strong{Details}.
#' @param key character: answer key for the items.
#' @param p.table logical: should the function return the proportions. If \code{FALSE} (default)
#' the counts are returned.
#' @param num.groups numeric: number of groups to that should be respondents splitted.
#'
#' @usage DistractorAnalysis(data, key, p.table = FALSE, num.groups = 3)
#'
#' @details
#' This function is adapted version of \code{distractor.analysis} function from \code{CTT} package.
#' The scores are calculatede using the item data and key. The respondents are then splitted into
#' the \code{num.groups}-quantiles and the number (or proportion) of respondents in each quantile is
#' reported with respect to their answers.
#'
#' The \code{data} is a matrix or data frame whose rows represents unscored item response from a
#' multiple-choice test and columns correspond to the items.
#'
#' The \code{key} must be a vector of the same length as \code{ncol(data)}.
#'
#' @author
#' Adela Drabinova \cr
#' Institute of Computer Science, The Czech Academy of Sciences \cr
#' Faculty of Mathematics and Physics, Charles University \cr
#' adela.drabinova@gmail.com \cr
#'
#' Patricia Martinkova \cr
#' Institute of Computer Science, The Czech Academy of Sciences \cr
#' martinkova@cs.cas.cz \cr
#'
#' @examples
#' \dontrun{
#' # loading difMedicaltest data set
#' data(difMedicaltest, package = "difNLR")
#' data  <- difMedicaltest[, colnames(difMedicaltest) != "gender"]
#' # loading difMedicalkey
#' data(difMedicalkey, package = "difNLR")
#' key  <- difMedicalkey
#'
#' # distractor analysis for difMedicaltest data set
#' DistractorAnalysis(data, key)
#'
#' # distractor analysis for difMedicaltest data set with proportions
#' DistractorAnalysis(data, key, p.table = T)
#'
#' # distractor analysis for difMedicaltest data set for 6 groups
#' DistractorAnalysis(data, key, num.group = 6)
#'
#' }
#'
#'
#' @export



DistractorAnalysis <-  function (data, key, p.table = FALSE, num.groups = 3)
{
  key <- unlist(key)
  if (!(is.logical(p.table))){
    warning("p.table must be logical.")
  }
  data <- as.data.frame(data)
  if (length(key) == 1)
    key <- c(rep(key, ncol(data)))
  scores <- as.data.frame(score(data, key)$score)
  if (missing(key))
    warning("Answer key is not provided")
  else {
    if (!length(key) == ncol(data)) {
      warning("Answer key is not provided or some item keys are missing.")
    }
    key <- c(key)
  }
  score.level <- quantile(scores[, 1], seq(0, 1, by = 1/num.groups))
  score.level <- cut(scores[, 1], score.level, include.lowest = TRUE,
                     labels = paste("Group", 1:num.groups, sep = " "))
  itemtab <- function(response) {
    xtabs( ~ response + score.level)
  }
  itemtabp <- function(response) {
    round(prop.table(xtabs( ~ response + score.level), 2), 3)
  }

  out <- list()
  if (p.table == FALSE)
    for (i in 1:ncol(data)) {
      out[[i]] <- itemtab(data[, i])
    }
  else for (i in 1:ncol(data)) {
    out[[i]] <- itemtabp(data[, i])
  }
  names(out) <- colnames(data)
  out
}
