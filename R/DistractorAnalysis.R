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
#' @param matching numeric: numeric vector. If not provided, total score is calculated and
#' distractor analysis is performed based on it.
#'
#' @usage DistractorAnalysis(data, key, p.table = FALSE, num.groups = 3, matching = NULL)
#'
#' @details
#' This function is adapted version of \code{\link[CTT]{distractor.analysis}} function from \code{CTT} package.
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
#' Adela Hladka \cr
#' Institute of Computer Science, The Czech Academy of Sciences \cr
#' Faculty of Mathematics and Physics, Charles University \cr
#' hladka@cs.cas.cz \cr
#'
#' Patricia Martinkova \cr
#' Institute of Computer Science, The Czech Academy of Sciences \cr
#' martinkova@cs.cas.cz \cr
#'
#' @seealso \code{\link[CTT]{distractor.analysis}}
#'
#' @examples
#' \dontrun{
#' # loading 100-item medical admission test data
#' data(dataMedicaltest, dataMedicalkey)
#' data <- dataMedicaltest[, 1:100]
#' key <- unlist(dataMedicalkey)
#'
#' # distractor analysis for dataMedicaltest data set
#' DistractorAnalysis(data, key)
#'
#' # distractor analysis for dataMedicaltest data set with proportions
#' DistractorAnalysis(data, key, p.table = T)
#'
#' # distractor analysis for dataMedicaltest data set for 6 groups
#' DistractorAnalysis(data, key, num.group = 6)
#' }
#'
#'
#' @export

DistractorAnalysis <-  function(data, key, p.table = FALSE, num.groups = 3, matching = NULL)
{
  if (!(is.logical(p.table))){
    warning("p.table must be logical.")
  }
  data <- as.data.frame(data)

  if (missing(key))
    warning("Answer key is not provided")
  else {
    if (!length(key) == ncol(data)) {
      warning("Answer key is not provided or some item keys are missing.")
    }
    key <- unlist(key)
  }

  if (length(key) == 1)
    key <- c(rep(key, ncol(data)))

  if (is.null(matching)){
    scored.data <- CTT::score(data, key, output.scored = T)$scored
    scored.data[is.na(scored.data)] <- 0
    scores <- apply(scored.data, 1, sum)
    score.level <- quantile(scores, seq(0, 1, by = 1/num.groups), na.rm = T)
    while (length(unique(score.level)) <= num.groups){
      num.groups <- num.groups - 1
      score.level <- quantile(scores, seq(0, 1, by = 1/num.groups), na.rm = T)
    }
    score.level <- cut(scores, score.level, include.lowest = TRUE,
                       labels = paste("Group", 1:num.groups, sep = " "))
  } else {

    scores <- matching
    score.level <- quantile(matching, seq(0, 1, by = 1/num.groups), na.rm = T)

    k <- 6
    if (length(levels(as.factor(scores))) <= length(scores)/k){
      score.level <- as.factor(scores)
      num.groups <- length(levels(scores))
      levels(score.level) <- paste("Group", 1:length(levels(score.level)), sep = " ")
      warning(paste('Critetion variable is probably discrete. Its cut is based on
                    its factors (', length(levels(score.level)), ").", sep = ""))
    } else {
      if (length(unique(score.level)) <= num.groups){

        while (length(unique(score.level)) <= num.groups){
          num.groups <- num.groups - 1
          score.level <- quantile(scores, seq(0, 1, by = 1/num.groups), na.rm = T)
        }
        score.level <- cut(scores, score.level, include.lowest = TRUE,
                           labels = paste("Group", 1:num.groups, sep = " "))
        warning(paste('The cut of criterion variable was not unique. The number of
                      groups was decreased to ', num.groups, ".", sep = ""))
      } else {
        score.level <- cut(scores, score.level, include.lowest = TRUE,
                           labels = paste("Group", 1:num.groups, sep = " "))
      }
    }
  }
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
