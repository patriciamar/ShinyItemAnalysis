#' Generalized Item Discrimination
#'
#' @aliases gDiscrim
#'
#' @description Generalized item discrimination functions is generalized version of
#' \code{\link{discrim}} function from \code{psychometric} package. It computes discrimination of an item,
#' i.e. the ablitity for a specific items to distinguish among upper and lower ability individuals on a test,
#' where number of groups, upper and lower group can be specified by user.
#'
#' @param x matrix or data.frame of items to be examined. Rows represent persons, columns
#' reperesent items.
#' @param k numeric: number of groups to which may be data.frame x divided by the total score.
#' Default value is 3.  See \strong{Details}.
#' @param l numeric: lower group. Default value is 1. See \strong{Details}.
#' @param u numeric: upper group. Default value is 3. See \strong{Details}.
#'
#' @usage gDiscrim(x, k = 3, l = 1, u = 3)
#'
#' @details The function takes data on individuals, computes their total test score and then divides
#' individuals into \code{k} groups. The lower and upper group are determined by \code{l} and \code{u}
#' parameters, i.e.  l-th and u-th group where the ordering is defined by increasing total score.
#'
#' @author
#' Adela Drabinova \cr
#' Institute of Computer Science, The Czech Academy of Sciences \cr
#' Faculty of Mathematics and Physics, Charles University \cr
#' adela.drabinova@gmail.com \cr
#'
#' Lubos Stepanek \cr
#' First Faculty of Medicine, Charles University \cr
#' stepanek.lub@seznam.cz \cr
#'
#' Patricia Martinkova \cr
#' Institute of Computer Science, The Czech Academy of Sciences \cr
#' martinkova@cs.cas.cz \cr
#'
#' @references
#' Martinkova, P., Stepanek, L., Drabinova, A., Houdek, J., Vejrazka, M., & Stuka, C. (2017).
#' Semi-real-time analyses of item characteristics for medical school admission tests.
#' In: Proceedings of the 2017 Federated Conference on Computer Science and Information Systems.
#'
#' @note
#' \code{gDiscrim} is used by \code{\link{DDplot}} function.
#'
#' @seealso
#' \code{\link{discrim}}, \code{\link{DDplot}}
#'
#' @examples
#' \dontrun{
#' # loading 100-item medical admission test data set
#' data(dataMedical)
#' x <- dataMedical[, 1:100]
#'
#' # discrimination as in discrim() function from psychometric package
#' # compare to psychometric::discrim(x)
#' gDiscrim(x)
#'
#' # 5 groups, compare 4th and 5th
#' gDiscrim(x, k = 5, l = 4, u = 5)
#' }
#' @export


gDiscrim <- function(x, k = 3, l = 1, u = 3){
  if(u > k){
    stop("'u' need to be lower or equal to 'k'", call. = FALSE)
  }
  if(l > k){
    stop("'l' need to be lower than 'k'", call. = FALSE)
  }
  if(l <= 0){
    stop("'l' need to be greater than 0", call. = FALSE)
  }
  if(l >= u){
    stop("'l' should be lower than 'u'", call. = FALSE)
  }

  x <- na.exclude(as.matrix(x))
  n <- ncol(x)
  N <- nrow(x)

  ni <- as.integer(N/k)
  TOT <- apply(x, 1, sum)

  tmpx <- cbind(x, TOT)[order(TOT), ]

  tmpxU <- tmpx[as.integer((u - 1)*N/k + 1):as.integer(u*N/k), ]
  tmpxL <- tmpx[as.integer((l - 1)*N/k + 1):as.integer(l*N/k), ]

  Ui <- apply(tmpxU, 2, sum)
  Li <- apply(tmpxL, 2, sum)

  discrim <- (Ui - Li)/ni
  return(discrim[1:n])
}

