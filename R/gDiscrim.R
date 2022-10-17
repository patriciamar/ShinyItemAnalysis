#' Compute generalized item discrimination
#'
#' Generalized version of discrimination index ULI. The function enumerates the
#' ability of an item to distinguish between individuals from upper (U) vs.
#' lower (L) ability groups, i.e. between respondents with high vs. low overall
#' score on the test. Number of groups, as well as upper and lower groups can be
#' specified by user. You can also manually supply the maximal and minimal
#' scores when the theoretical range of item score is known. Note that if the
#' *observed* item range is zero `NaN` is returned.
#'
#' @param Data matrix or data.frame of items to be examined. Rows represent
#'   respondents, columns represent items.
#' @param k numeric: number of groups to which may be `Data` divided by the
#'   total score. Default value is 3.  See **Details**.
#' @param l numeric: lower group. Default value is 1. See **Details**.
#' @param u numeric: upper group. Default value is 3. See **Details**.
#' @param maxscore numeric: maximal score in ordinal items. If missing, vector
#'   of obtained maximal scores is imputed. See **Details**.
#' @param minscore numeric: minimal score in ordinal items. If missing, vector
#'   of obtained minimal scores is imputed. See **Details**.
#' @param x deprecated. Use argument `Data` instead.
#' @inheritDotParams base::findInterval -x -vec
#'
#' @details The function computes total test scores for all respondents and then
#'   divides the respondents into `k` groups. The lower and upper groups
#'   are determined by `l` and `u` parameters, i.e., l-th and u-th
#'   group where the ordering is defined by increasing total score.
#'
#'   In ordinal items, difficulty is calculated as difference of average score
#'   divided by range (maximal possible score `maxscore` minus minimal
#'   possible score `minscore` for given item).
#'
#'   Discrimination is calculated as difference in difficulty between upper and
#'   lower group.
#'
#' @author Adela Hladka \cr Institute of Computer Science of the Czech Academy
#'   of Sciences \cr \email{hladka@@cs.cas.cz}
#'
#'   Lubomir Stepanek \cr Institute of Computer Science of the Czech Academy of
#'   Sciences
#'
#'   Jana Vorlickova \cr Institute of Computer Science of the Czech Academy of
#'   Sciences
#'
#'   Patricia Martinkova \cr Institute of Computer Science of the Czech Academy
#'   of Sciences \cr \email{martinkova@@cs.cas.cz}
#'
#'   Jan Netik \cr Institute of Computer Science of the Czech Academy of
#'   Sciences \cr \email{netik@@cs.cas.cz}
#'
#' @references Martinkova, P., Stepanek, L., Drabinova, A., Houdek, J.,
#'   Vejrazka, M., & Stuka, C. (2017). Semi-real-time analyses of item
#'   characteristics for medical school admission tests. In: Proceedings of the
#'   2017 Federated Conference on Computer Science and Information Systems.
#'   https://doi.org/10.15439/2017F380
#'
#' @note `gDiscrim` is used by [DDplot()] function.
#'
#' @seealso [DDplot()]
#'
#' @examples
#' # binary dataset
#' dataBin <- dataMedical[, 1:100]
#' # ordinal dataset
#' dataOrd <- dataMedicalgraded[, 1:100]
#'
#' # ULI for the first 5 items of binary dataset
#' # compare to psychometric::discrim(dataBin)
#' gDiscrim(dataBin)[1:5]
#' # generalized ULI using 5 groups, compare 4th and 5th for binary dataset
#' gDiscrim(dataBin, k = 5, l = 4, u = 5)[1:5]
#'
#' # ULI for first 5 items for ordinal dataset
#' gDiscrim(dataOrd)[1:5]
#' # generalized ULI using 5 groups, compare 4th and 5th for binary dataset
#' gDiscrim(dataOrd, k = 5, l = 4, u = 5)[1:5]
#' # maximum (4) and minimum (0) score are same for all items
#' gDiscrim(dataOrd, k = 5, l = 4, u = 5, maxscore = 4, minscore = 0)[1:5]
#' @export
#'
gDiscrim <- function(Data, k = 3, l = 1, u = 3, maxscore, minscore, x, ...) {
  # deprecated args handling
  if (!missing(x)) {
    warning("Argument 'x' is deprecated; please use 'Data' instead.",
      call. = FALSE
    )
    Data <- x
  }

  if (u > k) {
    stop("'u' need to be lower or equal to 'k'", call. = FALSE)
  }
  if (l > k) {
    stop("'l' need to be lower than 'k'", call. = FALSE)
  }
  if (l <= 0) {
    stop("'l' need to be greater than 0", call. = FALSE)
  }
  if (l >= u) {
    stop("'l' should be lower than 'u'", call. = FALSE)
  }
  if (missing(maxscore)) {
    maxscore <- sapply(Data, max, na.rm = TRUE)
  } else {
    if (length(maxscore) == 1) {
      maxscore <- rep(maxscore, ncol(Data))
    }
  }
  obtainedmax <- sapply(Data, max, na.rm = TRUE)
  if (!all(maxscore >= obtainedmax)) {
    warning("'maxscore' is lower than maximum score in the dataset for some item")
  }

  if (missing(minscore)) {
    minscore <- sapply(Data, min, na.rm = TRUE)
  } else {
    if (length(minscore) == 1) {
      minscore <- rep(minscore, ncol(Data))
    }
  }
  obtainedmin <- sapply(Data, min, na.rm = TRUE)
  if (!all(minscore <= obtainedmin)) {
    warning("'minscore' is higher than minimum score in the dataset for some item")
  }
  if (!all(minscore <= maxscore)) {
    warning("'minscore' is higher than 'maxscore' for some item")
  }

  d <- na.exclude(Data)

  # total scores
  ts <- rowSums(d)


  # get quantiles (without p = 0 & p = 1) according to k
  breakpoints <- quantile(ts, seq(0, 1, length.out = k + 1)[-c(1, k + 1)], names = FALSE)

  # assert dataset is breakable
  if (length(unique(breakpoints)) < k -1) stop("Dataset cannot be splitted, there are too few unique cutpoints.", call. = FALSE)

  # cut total scores into k groups
  grp <- findInterval(ts, breakpoints, ...)

  # range of scores
  rng <- maxscore - minscore

  # split data into groups defined above
  d_split <- split(d, grp)

  # get means of items for individual groups
  avgs <- lapply(d_split, function(x) {
    sapply(x, mean)
  })

  # difference between item means
  avg_diff <- avgs[[u]] - avgs[[l]]

  if (any(avg_diff == 0 & rng == 0)) {
    warning(
      "Both the groups' difficulty difference and item range are zero in item(s):\n",
      paste(names(which(avg_diff == 0 & rng == 0)), collapse = ", "), ".\n",
      "Try to supply `minscore` and `maxscore` manually according to plausible range.",
      call. = FALSE
    )
  }

  # divide by range for ordinal data
  # (binary data have range = 1; or possibly 0, when var(item) = 0)
  return(avg_diff / rng)
}
