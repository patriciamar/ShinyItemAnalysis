#' Distractor analysis
#'
#' @aliases DistractorAnalysis
#'
#' @description Performs distractor analysis for each item and
#'   optional number of groups.
#'
#' @param Data character: data matrix or data.frame with rows
#'   representing unscored item responses from a multiple-choice test
#'   and columns corresponding to the items.
#' @param key character: answer key for the items. The `key` must be a
#'   vector of the same length as `ncol(Data)`. In case it is not
#'   provided, `criterion` needs to be specified.
#' @param item numeric or character: either character `"all"` to
#'   apply for all items (default), or a vector of item names (column
#'   names of `Data`), or item identifiers (integers specifying
#'   the column number).
#' @param p.table logical: should the function return the proportions?
#'   If `FALSE` (default), the counts are returned.
#' @param num.groups numeric: number of groups to which are the
#'   respondents split.
#' @param criterion numeric: numeric vector. If not provided, total
#'   score is calculated and distractor analysis is performed based on
#'   it.
#' @param crit.discrete logical: is `criterion` discrete? Default
#'   value is `FALSE`. See details.
#' @param cut.points numeric: numeric vector specifying cut points of
#'   `criterion`. See details.
#' @param data deprecated. Use argument `Data` instead.
#' @param matching deprecated. Use argument `criterion` instead.
#' @param match.discrete deprecated. Use argument `crit.discrete`
#'   instead.
#'
#' @details This function is an adapted version of the
#'   `distractor.analysis()` function from \pkg{CTT} package. In
#'   case that no `criterion` is provided, the scores are calculated
#'   using the item `Data` and `key`. The respondents are by default
#'   split into the `num.groups`-quantiles and the number (or
#'   proportion) of respondents in each quantile is reported with
#'   respect to their answers. In case that `criterion` is discrete
#'   (`crit.discrete = TRUE`), `criterion` is split based on its
#'   unique levels. Other cut points can be specified via `cut.points`
#'   argument.
#'
#' @author
#' Adela Hladka \cr
#' Institute of Computer Science of the Czech Academy of Sciences \cr
#' \email{hladka@@cs.cas.cz} \cr
#'
#' Patricia Martinkova \cr
#' Institute of Computer Science of the Czech Academy of Sciences \cr
#' \email{martinkova@@cs.cas.cz} \cr
#'
#' @examples
#' # loading 100-item medical admission test dataset
#' data(dataMedicaltest, dataMedicalkey, dataMedical)
#' Data <- dataMedicaltest[, 1:100]
#' Databin <- dataMedical[, 1:100]
#' key <- dataMedicalkey
#'
#' # distractor analysis for all items
#' DistractorAnalysis(Data, key)
#'
#' # distractor analysis for item 1
#' DistractorAnalysis(Data, key, item = 1)
#' \dontrun{
#' # distractor analysis with proportions
#' DistractorAnalysis(Data, key, p.table = TRUE)
#'
#' # distractor analysis for 6 groups
#' DistractorAnalysis(Data, key, num.group = 6)
#'
#' # distractor analysis using specified criterion
#' criterion <- round(rowSums(Databin), -1)
#' DistractorAnalysis(Data, key, criterion = criterion)
#'
#' # distractor analysis using discrete criterion
#' DistractorAnalysis(Data, key, criterion = criterion, crit.discrete = TRUE)
#'
#' # distractor analysis using groups specified by cut.points
#' DistractorAnalysis(Data, key, cut.points = seq(10, 96, 10))
#' }
#'
#' @importFrom mirt key2binary
#' @export
DistractorAnalysis <- function(Data, key, item = "all", p.table = FALSE, num.groups = 3, criterion = NULL,
                               crit.discrete = FALSE, cut.points, data, matching, match.discrete) {
  # deprecated args handling
  if (!missing(data)) {
    warning("Argument 'data' is deprecated; please use 'Data' instead.",
      call. = FALSE
    )
    Data <- data
  }
  if (!missing(matching)) {
    warning("Argument 'matching' is deprecated; please use 'criterion' instead.",
      call. = FALSE
    )
    criterion <- matching
  }
  if (!missing(match.discrete)) {
    warning("Argument 'match.discrete' is deprecated; please use 'crit.discrete' instead.",
      call. = FALSE
    )
    crit.discrete <- match.discrete
  }

  Data <- as.data.frame(Data)
  m <- ncol(Data)
  nams <- colnames(Data)

  if (inherits(item,"character")) {
    if (any(item != "all") & !all(item %in% nams)) {
      stop("Invalid value for 'item'. Item must be either character 'all', or
           numeric vector corresponding to column identifiers, or name of the item.",
           call. = FALSE
      )
    }
    if (any(item == "all")) {
      items <- 1:m
    } else {
      items <- which(nams %in% item)
    }
  } else {
    if (!inherits(item,"integer") & !inherits(item,"numeric")) {
      stop("Invalid value for 'item'. Item must be either character 'all', or
           numeric vector corresponding to column identifiers, or name of the item.",
           call. = FALSE
      )
    } else {
      if (!all(item %in% 1:m)) {
        stop("Invalid number for 'item'.",
             call. = FALSE
        )
      } else {
        items <- item
      }
    }
  }

  if (!(is.logical(p.table))) {
    warning("'p.table' must be logical.", call. = FALSE)
  }

  if (missing(key) | is.null(key)) {
    if (all(sapply(Data, is.numeric))) {
      warning("Answer key is not provided. Maximum value is used as key.", call. = FALSE)
      key <- sapply(Data, max, na.rm = TRUE)
    } else if (missing(criterion)) {
      stop("Answer key is not provided. Please, specify 'key' to be able to calculate total score or provide 'criterion'. ",
        call. = FALSE
      )
    }
  } else {
    key <- unlist(key)
    if (!length(key) == ncol(Data)) {
      warning("Answer key is not provided or some item keys are missing.", call. = FALSE)
    }
  }

  if (length(key) == 1) {
    key <- c(rep(key, ncol(Data)))
  }

  if (is.null(criterion)) {
    scored.data <- mirt::key2binary(Data, as.matrix(key))
    scored.data[is.na(scored.data)] <- 0
    scores <- rowSums(scored.data)
  } else {
    scores <- as.numeric(paste(criterion))
  }

  if (crit.discrete) {
    score.cut <- sort(unique(scores))
    if (!missing(cut.points)) {
      warning("Cut points specified in 'cut.points' are ignored. Used 'crit.discrete = FALSE' to use them.",
        call. = FALSE
      )
    }
    num.groups <- length(score.cut)
    score.level <- scores
  } else {
    if (missing(cut.points)) {
      score.cut <- quantile(scores, seq(0, 1, by = 1 / num.groups), na.rm = TRUE)
      if (any(duplicated(score.cut))) {
        stop("Cut points based on quantiles are not unique. Consider smaller number of groups specified in 'num.groups'.",
          call. = FALSE
        )
      }
    } else {
      if (any(duplicated(cut.points))) {
        warning("Cut points provided in 'cut.points' argument are not unique.", call. = FALSE)
      }
      if (any(range(scores)[1] > cut.points | cut.points > range(scores)[2])) {
        if (is.null(criterion)) {
          warning("Some of cut points provided in 'cut.points' are out of range of computed total scores.",
                  call. = FALSE
          )
        } else {
          warning("Some of cut points provided in 'cut.points' are out of range of criterion.",
                  call. = FALSE
          )
        }
      }
      if (all(range(scores)[1] > cut.points | cut.points > range(scores)[2])) {
        stop("All of cut points provided in cut.points are out of range of criterion.",
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
    prop.table(xtabs(~ response + score.level), 2)
  }

  res <- list()
  if (!p.table) {
    res <- lapply(items, function(i) itemtab(Data[, i]))
  } else {
    res <- lapply(items, function(i) itemtabp(Data[, i]))
  }
  res <- Filter(Negate(function(i) is.null(unlist(i))), res)
  names(res) <- nams[items]
  return(res)
}
