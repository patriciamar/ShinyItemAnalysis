#' Generalized Item Discrimination
#'
#' @aliases gDiscrim
#'
#' @description gDiscrim function computes various generalizations of discrimination index ULI.
#' It enumerates the ablitity of item to distinguish between individuals from upper (U) vs. lower (L)
#' ability groups, i.e. between respondents with high vs. low overall score on the test.
#' Number of groups, as well as upper and lower groups can be specified by user.
#' Maximal and minimal score in ordinal data sets can be specified by user.
#'
#' @param x matrix or data.frame of items to be examined. Rows represent persons, columns
#' reperesent items.
#' @param k numeric: number of groups to which may be data.frame x divided by the total score.
#' Default value is 3.  See \strong{Details}.
#' @param l numeric: lower group. Default value is 1. See \strong{Details}.
#' @param u numeric: upper group. Default value is 3. See \strong{Details}.
#' @param maxscore numeric: maximal score in ordinal items. If missing, vector of obtained maximal scores is imputed. See \strong{Details}.
#' @param minscore numeric: minimal score in ordinal items. If missing, vector of obtained minimal scores is imputed. See \strong{Details}.
#'
#' @usage gDiscrim(x, k = 3, l = 1, u = 3, maxscore, minscore)
#'
#' @details The function computes total test scores for all respondents and then divides
#' the respondents into \code{k} groups. The lower and upper groups are determined by \code{l} and \code{u}
#' parameters, i.e.  l-th and u-th group where the ordering is defined by increasing total score.
#'
#' In ordinal items, difficulty is calculated as difference of average score divided by range
#' (maximal possible score \code{maxscore} minus minimal possible score \code{minscore} for given item).
#'
#' Discrimination is calculated as difference in difficulty between upper and lower group.
#'
#' @author
#' Adela Drabinova \cr
#' Institute of Computer Science, The Czech Academy of Sciences \cr
#' Faculty of Mathematics and Physics, Charles University \cr
#' drabinova@cs.cas.cz \cr
#'
#' Lubos Stepanek \cr
#' First Faculty of Medicine, Charles University \cr
#' lubomir.stepanek@lf1.cuni.cz \cr
#'
#' Jana Vorlickova \cr
#' Institute of Computer Science, The Czech Academy of Sciences \cr
#' Faculty of Mathematics and Physics, Charles University \cr
#'
#' Patricia Martinkova \cr
#' Institute of Computer Science, The Czech Academy of Sciences \cr
#' martinkova@cs.cas.cz \cr
#'
#' @references
#' Martinkova, P., Stepanek, L., Drabinova, A., Houdek, J., Vejrazka, M., & Stuka, C. (2017).
#' Semi-real-time analyses of item characteristics for medical school admission tests.
#' In: Proceedings of the 2017 Federated Conference on Computer Science and Information Systems.
#' http://dx.doi.org/10.15439/2017F380
#'
#' @note
#' \code{gDiscrim} is used by \code{\link{DDplot}} function.
#'
#' @seealso
#' \code{\link{DDplot}}
#'
#' @examples
#' \dontrun{
#' # loading 100-item medical admission test data sets
#' data(dataMedical, dataMedicalgraded)
#' # binary data set
#' dataBin <- dataMedical[, 1:100]
#' # ordinal data set
#' dataOrd <- dataMedicalgraded[, 1:100]
#'
#' # ULI for first 5 items for binary data set
#' # compare to psychometric::discrim(x)
#' gDiscrim(dataBin)[1:5]
#' # generalized ULI using 5 groups, compare 4th and 5th for binary data set
#' gDiscrim(dataBin, k = 5, l = 4, u = 5)[1:5]
#'
#' # ULI for first 5 items for ordinal data set
#' gDiscrim(dataOrd)[1:5]
#' # generalized ULI using 5 groups, compare 4th and 5th for binary data set
#' gDiscrim(dataOrd, k = 5, l = 4, u = 5)[1:5]
#' # maximum (4) and minimum (0) score are same for all items
#' gDiscrim(dataOrd,  k = 5, l = 4, u = 5, maxscore = 4, minscore = 0)[1:5]
#' }
#' @export

gDiscrim <- function (x, k = 3, l = 1, u = 3, maxscore, minscore)
{
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
    maxscore <- apply(x,2,max,na.rm=T)
  }  else {
    if (length(maxscore) == 1){
      maxscore <- rep(maxscore, ncol(x))
    }
  }
  obtainedmax <- apply(x,2,max,na.rm=T)
  if ( !all(maxscore >= obtainedmax)) {
    warning("'maxscore' is lower than maximum score in the data set for some item")
  }

  if (missing(minscore)) {
    minscore <- apply(x,2,min,na.rm=T)
  } else {
    if (length(minscore) == 1){
      minscore <- rep(minscore, ncol(x))
    }
  }
  obtainedmin <- apply(x,2,min,na.rm=T)
  if ( !all(minscore <= obtainedmin)) {
    warning("'minscore' is higher than minimum score in the data set for some item")
  }
  if ( !all(minscore <= maxscore)) {
    warning("'minscore' is higher than 'maxscore' for some item")
  }

  x <- na.exclude(as.matrix(x))
  n <- ncol(x)
  N <- nrow(x)
  ni <- as.integer(N/k)
  MaxMin <- maxscore - minscore
  MaxSum <- sum(apply(x,2,max,na.rm=T))
  MinSum <- sum(apply(x,2,min,na.rm=T))
  TOT <- apply(x, 1, sum)/(MaxSum-MinSum)
  tmpx <- x[order(TOT), ]
  tmpxU <- tmpx[as.integer((u - 1) * N/k + 1):as.integer(u *
                                                           N/k), ]
  tmpxL <- tmpx[as.integer((l - 1) * N/k + 1):as.integer(l *
                                                           N/k), ]
  Ui <- apply(tmpxU, 2, sum)/MaxMin
  Li <- apply(tmpxL, 2, sum)/MaxMin
  discrim <- (Ui - Li)/ni
  return(discrim[1:n])
}

