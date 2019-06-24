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
#' @param match.discrete logical: is \code{matching} discrete? Default value is \code{FALSE}. See details.
#' @param cut.points numeric: numeric vector specifying cut points of \code{matching}. See details.
#'
#' @usage DistractorAnalysis(data, key, p.table = FALSE, num.groups = 3, matching = NULL,
#' match.discrete = FALSE, cut.points)
#'
#' @details
#' This function is adapted version of \code{\link[CTT]{distractor.analysis}} function from \code{CTT} package.
#'
#' The \code{data} is a matrix or data frame whose rows represents unscored item response from a
#' multiple-choice test and columns correspond to the items.
#'
#' The \code{key} must be a vector of the same length as \code{ncol(data)}.
#'
#' In case, no \code{matching} is provided, the scores are calculated using the item data and key.
#' The respondents are by default splitted into the \code{num.groups}-quantiles and the number (or proportion)
#' of respondents in each quantile is reported with respect to their answers. In case that \code{matching}
#' is discrete (\code{match.discrete = TRUE}), \code{matching} is splitted based on its unique levels. Other
#' cut points can be specified via \code{cut.points} argument.
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
#' dataBin <- dataMedical[, 1:100]
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
#'
#' # distractor analysis for dataMedicaltest using specified matching
#' matching <- round(rowSums(databin), -1)
#' DistractorAnalysis(data, key, matching = matching)
#'
#' # distractor analysis for dataMedicaltest using discrete matching
#' DistractorAnalysis(data, key, matching = matching, match.discrete = T)
#'
#' # distractor analysis for dataMedicaltest using groups specified by cut.points
#' DistractorAnalysis(data, key, cut.points = seq(10, 100, 10))
#' }
#'
#'
#' @export

DistractorAnalysis <-  function(data, key, p.table = FALSE, num.groups = 3, matching = NULL,
                                match.discrete = FALSE, cut.points)
{
  if (!(is.logical(p.table))) warning("p.table must be logical.")

  data <- as.data.frame(data)

  if (missing(key))
    warning("Answer key is not provided")
  else {
    if (!length(key) == ncol(data)) {
      warning("Answer key is not provided or some item keys are missing.")
    }
    key <- unlist(key)
  }

  if (length(key) == 1) key <- c(rep(key, ncol(data)))

  if (is.null(matching)){
    scored.data <- CTT::score(data, key, output.scored = T)$scored
    scored.data[is.na(scored.data)] <- 0
    scores <- rowSums(scored.data)
  } else {
    scores <- as.numeric(paste(matching))
  }

  if (match.discrete){
    score.cut <- sort(unique(scores))
    if (!missing(cut.points)) warning("Cut points specified in group argument are ignored. Used match.discrete = FALSE to use them. ")
    num.groups <- length(score.cut)
    score.level <- scores
  } else {
    if (missing(cut.points)){
      score.cut <- quantile(scores, seq(0, 1, by = 1/num.groups), na.rm = T)
      if (any(duplicated(score.cut))) stop("Cut points based on quantiles are not unique. Consider smaller number of groups specified in num.groups argument.")
    } else {
      if (any(duplicated(cut.points))) warning("Cut points provided in cut.points argument are not unique. ")
      if (any(!(cut.points %in% scores))) stop("Cut points provided in cut.points do not match matching criterion. ")
      score.cut <- as.numeric(paste(cut.points))
      score.cut <- c(min(scores, na.rm = T), max(scores, na.rm = T), score.cut)
      score.cut <- sort(unique(score.cut))
    }
    num.groups <- length(score.cut) - 1
    score.level <- cut(scores, score.cut, include.lowest = TRUE,
                       labels = paste0("Group", 1:num.groups))
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
    } else for (i in 1:ncol(data)) {
    out[[i]] <- itemtabp(data[, i])
  }
  names(out) <- colnames(data)
  out
}
