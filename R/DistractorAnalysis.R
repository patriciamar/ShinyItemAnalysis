#' Distractor analysis
#'
#' @aliases DistractorAnalysis
#'
#' @description Performs distractor analysis for each item and optional number
#'   of groups.
#'
#' @param Data character: data matrix or data.frame. See \strong{Details}.
#' @param key character: answer key for the items.
#' @param p.table logical: should the function return the proportions. If
#'   \code{FALSE} (default) the counts are returned.
#' @param num.groups numeric: number of groups to that should be respondents
#'   splitted.
#' @param matching numeric: numeric vector. If not provided, total score is
#'   calculated and distractor analysis is performed based on it.
#' @param match.discrete logical: is \code{matching} discrete? Default value is
#'   \code{FALSE}. See details.
#' @param cut.points numeric: numeric vector specifying cut points of
#'   \code{matching}. See details.
#' @param data deprecated. Use argument \code{Data} instead.
#'
#' @details This function is adapted version of
#' \code{\link[CTT]{distractor.analysis}} function from \code{CTT} package.
#'
#' The \code{Data} is a matrix or data.frame with rows representing unscored
#' item responses from a multiple-choice test and with columns corresponding to
#' the items.
#'
#' The \code{key} must be a vector of the same length as \code{ncol(Data)}.
#'
#' In case, no \code{matching} is provided, the scores are calculated using the
#' item \code{Data} and \code{key}. The respondents are by default splitted into
#' the \code{num.groups}-quantiles and the number (or proportion) of respondents
#' in each quantile is reported with respect to their answers. In case that
#' \code{matching} is discrete (\code{match.discrete = TRUE}), \code{matching}
#' is splitted based on its unique levels. Other cut points can be specified via
#' \code{cut.points} argument.
#'
#' @author
#' Adela Hladka \cr
#' Institute of Computer Science of the Czech Academy of Sciences \cr
#' Faculty of Mathematics and Physics, Charles University \cr
#' \email{hladka@@cs.cas.cz} \cr
#'
#' Patricia Martinkova \cr
#' Institute of Computer Science of the Czech Academy of Sciences \cr
#' \email{martinkova@@cs.cas.cz} \cr
#'
#' @examples
#' # loading 100-item medical admission test dataset
#' data(dataMedicaltest, dataMedicalkey)
#' data <- dataMedicaltest[, 1:100]
#' dataBin <- dataMedical[, 1:100]
#' key <- unlist(dataMedicalkey)
#'
#' # distractor analysis for dataMedicaltest dataset
#' DistractorAnalysis(data, key)
#' \dontrun{
#' # distractor analysis for dataMedicaltest dataset with proportions
#' DistractorAnalysis(data, key, p.table = TRUE)
#'
#' # distractor analysis for dataMedicaltest dataset for 6 groups
#' DistractorAnalysis(data, key, num.group = 6)
#'
#' # distractor analysis for dataMedicaltest using specified matching
#' matching <- round(rowSums(databin), -1)
#' DistractorAnalysis(data, key, matching = matching)
#'
#' # distractor analysis for dataMedicaltest using discrete matching
#' DistractorAnalysis(data, key, matching = matching, match.discrete = TRUE)
#'
#' # distractor analysis for dataMedicaltest using groups specified by cut.points
#' DistractorAnalysis(data, key, cut.points = seq(10, 100, 10))
#' }
#'
#' @importFrom mirt key2binary
#' @export
DistractorAnalysis <- function(Data, key, p.table = FALSE, num.groups = 3, matching = NULL,
                               match.discrete = FALSE, cut.points, data) {
  if (!missing(data)) {
    stop("Argument 'data' deprecated. Please use argument 'Data' instead. ", call. = FALSE)
  }

  if (!(is.logical(p.table))) warning("p.table must be logical. ")

  Data <- as.data.frame(Data)

  if (missing(key) | is.null(key)) {
    if (all(sapply(Data, is.numeric))) {
      warning("Answer key is not provided. Maximum value is used as key.", call. = FALSE)
      key <- sapply(Data, max, na.rm = TRUE)
    } else if (missing(matching)) {
      stop("Answer key is not provided. Please, specify key to be able to calculate total score or provide matching. ",
        call. = FALSE
      )
    }
  } else {
    if (!length(key) == ncol(Data)) {
      warning("Answer key is not provided or some item keys are missing.", call. = FALSE)
    }
    key <- unlist(key)
  }

  if (length(key) == 1) key <- c(rep(key, ncol(Data)))

  if (is.null(matching)) {
    scored.data <- mirt::key2binary(Data, as.matrix(key))
    scored.data[is.na(scored.data)] <- 0
    scores <- rowSums(scored.data)
  } else {
    scores <- as.numeric(paste(matching))
  }

  if (match.discrete) {
    score.cut <- sort(unique(scores))
    if (!missing(cut.points)) {
      warning("Cut points specified in cut.points argument are ignored. Used match.discrete = FALSE to use them.",
        call. = FALSE
      )
    }
    num.groups <- length(score.cut)
    score.level <- scores
  } else {
    if (missing(cut.points)) {
      score.cut <- quantile(scores, seq(0, 1, by = 1 / num.groups), na.rm = TRUE)
      if (any(duplicated(score.cut))) {
        stop("Cut points based on quantiles are not unique. Consider smaller number of groups specified in num.groups argument.",
          call. = FALSE
        )
      }
    } else {
      if (any(duplicated(cut.points))) {
        warning("Cut points provided in cut.points argument are not unique.", call. = FALSE)
      }
      if (any(range(scores)[1] > cut.points | cut.points > range(scores)[2])) {
        warning("Some of cut points provided in cut.points are out of range of matching criterion.",
          call. = FALSE
        )
      }
      if (all(range(scores)[1] > cut.points | cut.points > range(scores)[2])) {
        stop("All of cut points provided in cut.points are out of range of matching criterion.",
          call. = FALSE
        )
      }
      score.cut <- as.numeric(paste(cut.points))
      score.cut <- c(min(scores, na.rm = T), max(scores, na.rm = T), score.cut)
      score.cut <- sort(unique(score.cut))
    }
    num.groups <- length(score.cut) - 1
    score.level <- cut(scores, score.cut,
      include.lowest = TRUE,
      labels = paste0("Group", 1:num.groups)
    )
  }

  itemtab <- function(response) {
    xtabs(~ response + score.level)
  }
  itemtabp <- function(response) {
    round(prop.table(xtabs(~ response + score.level), 2), 3)
  }
  out <- list()
  if (!p.table) {
    for (i in 1:ncol(Data)) {
      out[[i]] <- itemtab(Data[, i])
    }
  } else {
    for (i in 1:ncol(Data)) {
      out[[i]] <- itemtabp(Data[, i])
    }
  }
  names(out) <- colnames(Data)
  out
}
