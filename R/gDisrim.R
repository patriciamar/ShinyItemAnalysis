#' Compute generalized item discrimination
#'
#' @aliases gDiscrim
#'
#' @description gDiscrim function computes various generalizations of
#'   discrimination index ULI. It enumerates the ability of item to distinguish
#'   between individuals from upper (U) vs. lower (L) ability groups, i.e.
#'   between respondents with high vs. low overall score on the test. Number of
#'   groups, as well as upper and lower groups can be specified by user. Maximal
#'   and minimal score in ordinal datasets can be specified by user.
#'
#' @param Data matrix or data.frame of items to be examined. Rows represent
#'   respondents, columns represent items.
#' @param k numeric: number of groups to which may be \code{Data} divided by
#'   the total score. Default value is 3.  See \strong{Details}.
#' @param l numeric: lower group. Default value is 1. See \strong{Details}.
#' @param u numeric: upper group. Default value is 3. See \strong{Details}.
#' @param maxscore numeric: maximal score in ordinal items. If missing, vector
#'   of obtained maximal scores is imputed. See \strong{Details}.
#' @param minscore numeric: minimal score in ordinal items. If missing, vector
#'   of obtained minimal scores is imputed. See \strong{Details}.
#' @param x deprecated. Use argument \code{Data} instead.
#'
#' @details The function computes total test scores for all respondents and then
#'   divides the respondents into \code{k} groups. The lower and upper groups
#'   are determined by \code{l} and \code{u} parameters, i.e., l-th and u-th
#'   group where the ordering is defined by increasing total score.
#'
#'   In ordinal items, difficulty is calculated as difference of average score
#'   divided by range (maximal possible score \code{maxscore} minus minimal
#'   possible score \code{minscore} for given item).
#'
#'   Discrimination is calculated as difference in difficulty between upper and
#'   lower group.
#'
#' @author
#' Adela Hladka \cr
#' Institute of Computer Science of the Czech Academy of Sciences \cr
#' Faculty of Mathematics and Physics, Charles University \cr
#' \email{hladka@@cs.cas.cz} \cr
#'
#' Lubomir Stepanek \cr
#' First Faculty of Medicine, Charles University \cr
#'
#' Jana Vorlickova \cr
#' Institute of Computer Science of the Czech Academy of Sciences \cr
#'
#' Patricia Martinkova \cr
#' Institute of Computer Science of the Czech Academy of Sciences \cr
#' \email{martinkova@@cs.cas.cz} \cr
#'
#' @references Martinkova, P., Stepanek, L., Drabinova, A., Houdek, J.,
#' Vejrazka, M., & Stuka, C. (2017). Semi-real-time analyses of item
#' characteristics for medical school admission tests. In: Proceedings of the
#' 2017 Federated Conference on Computer Science and Information Systems.
#' https://doi.org/10.15439/2017F380
#'
#' @note
#' \code{gDiscrim} is used by \code{\link{DDplot}} function.
#'
#' @seealso
#' \code{\link{DDplot}}
#'
#' @examples
#' # loading 100-item medical admission test datasets
#' data(dataMedical, dataMedicalgraded)
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

gDiscrim <- function(Data, k = 3, l = 1, u = 3, maxscore, minscore, x) {
  if (!missing(x)) {
    stop("Argument 'x' deprecated. Please use argument 'Data' instead. ", call. = FALSE)
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

  Data <- na.exclude(Data)
  n <- ncol(Data)
  N <- nrow(Data)

  ni <- as.integer(N / k)
  MaxMin <- maxscore - minscore
  MaxSum <- sum(sapply(Data, max, na.rm = TRUE))
  MinSum <- sum(sapply(Data, min, na.rm = TRUE))
  TOT <- rowSums(Data, na.rm = TRUE) / (MaxSum - MinSum)
  tmpx <- Data[order(TOT), ]
  tmpxU <- tmpx[as.integer((u - 1) * N / k + 1):as.integer(u * N / k), ]
  tmpxL <- tmpx[as.integer((l - 1) * N / k + 1):as.integer(l * N / k), ]
  Ui <- colSums(tmpxU, na.rm = TRUE) / MaxMin
  Li <- colSums(tmpxL, na.rm = TRUE) / MaxMin
  discrim <- (Ui - Li) / ni
  return(discrim[1:n])
}
