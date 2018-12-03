#' Item Analysis
#'
#' @aliases ItemAnalysis
#'
#' @description ItemAnalysis function computes various traditional item analysis indices
#' including difficulty, discrimination and item validity.
#' For ordinal items the difficulty and discrimination indices take into account
#' minimal item score as well as range.
#'
#' @param data matrix or data.frame of items to be examined. Rows represent respondents, columns
#' reperesent items.
#' @param y vector of criterion values.
#' @param k numeric: number of groups to which may be data.frame x divided by the total score.
#' Default value is 3.  See \strong{Details}.
#' @param l numeric: lower group. Default value is 1. See \strong{Details}.
#' @param u numeric: upper group. Default value is 3. See \strong{Details}.
#' @param maxscore numeric or vector: maximal score in ordinal items. If missing, vector of obtained maximal scores is imputed. See \strong{Details}.
#' @param minscore numeric or vector: minimal score in ordinal items. If missing, vector of obtained minimal scores is imputed. See \strong{Details}.
#' @param cutscore numeric or vector: cut score used for binarization of ordinal data.
#' If missing, vector of maximal scores is imputed. See \strong{Details}.
#' @param add.bin logical: If TRUE, indices are printed also for binarized data. See \strong{Details}.
#'
#' @usage ItemAnalysis(data, y = NULL, k = 3, l = 1, u = 3,
#'                     maxscore, minscore, cutscore, add.bin=FALSE)
#'
#' @details ItemAnalysis function computes various traditional item analysis indices
#' including difficulty indices based on ratio of correct answers, sample SD,
#' discrimination indices ULI based on difference in ratio of correct answer
#' in Upper and Lower group, RIT (correlation between item score and overall test score),
#' RIR (correlation between item score and overall test score),
#' item validity indices based on correlation of Item score with criterion,
#' and item reliability indices based on Cronbach's alpha without given item.
#' For ordinal items the difficulty and discrimination indices take into account
#' minimal item score as well as range.
#'
#' For caluclation discimination ULI index, it is possible to
#' specify number of groups \code{k}, and which two groups \code{l} and \code{u}
#' are to be compared.
#'
#' In ordinal items, difficulty is calculated as difference of average score divided by range
#' (maximal possible score \code{maxscore} minus minimal possible score \code{minscore}).
#' If \code{add.bin} is set to \code{TRUE}, item analysis of binarized data is
#' included in the output table. In such a case, \code{cutscore} is used for binarization.
#' When binarizing the data, values greater or equal to cut-score are set to 1,
#' other values are set to 0.
#'
#' @author
#' Patricia Martinkova \cr
#' Institute of Computer Science, The Czech Academy of Sciences \cr
#' martinkova@cs.cas.cz \cr
#'
#' Jana Vorlickova \cr
#' Institute of Computer Science, The Czech Academy of Sciences \cr
#' Faculty of Mathematics and Physics, Charles University \cr
#'
#' Adela Drabinova \cr
#' Institute of Computer Science, The Czech Academy of Sciences \cr
#' Faculty of Mathematics and Physics, Charles University \cr
#' drabinova@cs.cas.cz \cr
#'
#' @references
#' Martinkova, P., Stepanek, L., Drabinova, A., Houdek, J., Vejrazka, M., & Stuka, C. (2017).
#' Semi-real-time analyses of item characteristics for medical school admission tests.
#' In: Proceedings of the 2017 Federated Conference on Computer Science and Information Systems.
#' http://dx.doi.org/10.15439/2017F380
#'
#' Allen, M. J. & Yen, W. M. (1979). Introduction to measurement theory. Monterey, CA: Brooks/Cole.
#'
#' @seealso
#' \code{\link{DDplot}}, \code{\link{gDiscrim}}
#'
#' @examples
#' \dontrun{
#' ## Examples on 100-item medical admission test data set (binary)
#' library(ShinyItemAnalysis)
#' dim(dataMedical)
#' names(dataMedical)
#'
#' dataBin <- dataMedical[,1:100]
#' StudySuccessBin <- dataMedical[,102]
#'
#' head(ItemAnalysis(dataBin))
#' head(ItemAnalysis(dataBin, y = StudySuccessBin))
#'
#' ## Examples on 100-item medical admission test data set (ordinal)
#' dim(dataMedicalgraded)
#' names(dataMedicalgraded)
#'
#' dataOrd <- dataMedicalgraded[,1:100]
#' StudySuccess <- dataMedicalgraded[,102]
#'
#' head(ItemAnalysis(dataOrd, y = StudySuccess))
#' head(ItemAnalysis(dataOrd, y = StudySuccess,
#'      k = 5, l = 4, u = 5, maxscore = 4, minscore = 0, cutscore = 4, add.bin=TRUE) )
#' }
#' @export

ItemAnalysis <- function (data, y = NULL,  k = 3, l = 1, u = 3, maxscore, minscore, cutscore, add.bin=FALSE)
{

  if (!is.matrix(data) & !is.data.frame(data)) {
    stop("'data' must be data frame or matrix ",
         call. = FALSE)
  }
  if (missing(maxscore)) {
    maxscore <- apply(data,2,max,na.rm=T)
  }
  if (missing(minscore)) {
    minscore <- rep(0, dim(data)[2])
  }
  if (missing(cutscore)) {
    cutscore <- apply(data,2,max,na.rm=T)
  } else {
    if (length(cutscore) == 1){
      cutscore <- rep(cutscore, ncol(data))
    }
  }
  if (add.bin ==TRUE) {
    dataBin  <- data
    for(i in 1:dim(data)[2]){
      dataBin[data[,i] >= cutscore[i], i] <- 1
      dataBin[data[,i] < cutscore[i], i] <- 0
    }
    head(dataBin)
    minscoreB <- apply(dataBin,2,min,na.rm=T)
    maxscoreB <- apply(dataBin,2,max,na.rm=T)
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


  data <- na.exclude(as.matrix(data))
  n <- ncol(data)
  N <- nrow(data)
  TOT <- apply(data, 1, sum)
  TOT.woi <- TOT - (data)
  mean <- apply(data, 2, mean)
  obtainedmin <- apply(data,2,min,na.rm=T)
  obtainedmax <- apply(data,2,max,na.rm=T)

  # ratio of full scores
  dataTOT <- rbind(data, maxscore)
  correct <- (apply(apply(dataTOT, 2, function (x) x==max(x)), 2, sum)-1)/nrow(data)

  # ULI ordinal
  ni <- as.integer(N/k)
  Max <- c(maxscore)
  MaxSum <- sum(apply(data,2,max,na.rm=T))
  TOT <- apply(data, 1, sum)/MaxSum
  tmpx <- data[order(TOT), ]
  tmpxU <- tmpx[as.integer((u - 1) * N/k + 1):as.integer(u *
                                                           N/k), ]
  tmpxL <- tmpx[as.integer((l - 1) * N/k + 1):as.integer(l *
                                                           N/k), ]
  Ui <- apply(tmpxU, 2, sum)/Max
  Li <- apply(tmpxL, 2, sum)/Max
  ULIord <- (Ui - Li)/ni

  ni <- as.integer(N/3)
  tmpxU <- tmpx[(N + 1 - ni):N, ]
  tmpxL <- tmpx[1:ni, ]
  Ui <- apply(tmpxU, 2, sum)/Max
  Li <- apply(tmpxL, 2, sum)/Max
  discrim <- (Ui - Li)/ni

  #ULI binary
  if (add.bin == TRUE){
    ni <- as.integer(N/k)
    TOT <- apply(dataBin, 1, sum)
    tmpx <- dataBin[order(TOT), ]
    tmpxU <- tmpx[as.integer((u - 1) * N/k + 1):as.integer(u *
                                                             N/k), ]
    tmpxL <- tmpx[as.integer((l - 1) * N/k + 1):as.integer(l *
                                                             N/k), ]
    Ui <- apply(tmpxU, 2, sum)
    Li <- apply(tmpxL, 2, sum)
    ULIbin <- (Ui - Li)/ni


    ni <- as.integer(N/3)
    tmpxU <- tmpx[(N + 1 - ni):N, ]
    tmpxL <- tmpx[1:ni, ]
    Ui <- apply(tmpxU, 2, sum)
    Li <- apply(tmpxL, 2, sum)
    discrimBin <- (Ui - Li)/ni

  } else {
    ULIbin <- NA
    discrimBin <- NA
  }


  # RIR ordinal
  TOTord <- apply(data, 1, sum)
  TOT.woi <- TOTord - (data)
  rix.woi <- diag(cor(data, TOT.woi, use = "complete"))

  # RIT ordinal
  rix <- cor(data, TOTord, use = "complete")

  # RIR and RIT binary
  if (add.bin == TRUE){
    TOTbin <- apply(dataBin, 1, sum)
    TOT.woi.bin <- TOTbin - (dataBin)
    rix.woi.bin <- diag(cor(dataBin, TOT.woi.bin, use = "complete"))
    rix.bin <- cor(dataBin, TOTbin, use = "complete")
    rix.bin[(maxscoreB-minscoreB) < 1] <- 0
    rix.woi.bin[(maxscoreB-minscoreB) < 1] <- 0
  } else {
    rix.woi.bin <-NA
    rix.bin <- NA
  }

  # Average scaled score
  difc <- (mean - minscore)/(maxscore-minscore)

  # SD
  sx <- apply(data, 2, sd)
  vx <- ((N - 1)/N) * sx^2

  # Item-criterion correlation
  if (is.null(y)) {
    riy <- NA
  }   else {
    y <- as.numeric(y)
    riy <- cor(data, y, use = "complete")
  }
  i.val <- riy * sqrt(vx)
  i.rel <- rix * sqrt(vx)
  i.rel.woi <- rix.woi * sqrt(vx)

  # Item analysis of binarized data
  if (add.bin==TRUE){
    sx.bin <- apply(dataBin, 2, sd)
    vx.bin <- ((N - 1)/N) * sx.bin^2
    if (is.null(y)) {
      riy.bin <- NA
    }   else {
      y <- as.numeric(y)
      riy.bin <- cor(dataBin, y, use = "complete")
    }
    i.val.bin <- riy.bin * sqrt(vx.bin)
    i.rel.bin <- rix.bin * sqrt(vx.bin)
    i.rel.woi.bin <- rix.woi.bin * sqrt(vx.bin)
  } else {
    sx.bin <- NA
    i.val.bin <- NA
    i.rel.bin <- NA
    i.rel.woi.bin <- NA
    riy.bin <- NA
  }

  # Alpha without item
  alphaDrop <- apply(data,2, function (x) {
    withoutItem <- data[, apply(data, 2, function (y) !all(y==x))]
    var <- var(withoutItem)
    N <- ncol( withoutItem )
    TOT <- apply( withoutItem , 1, sum)
    alpha <- N/(N-1)*(1-(sum(diag(var))/var(TOT)))
  })

  if(add.bin==TRUE){
    alphaDrop.bin <- apply(dataBin,2, function (x) {
      withoutItem <- dataBin[, apply(dataBin, 2, function (y) !all(y==x))]
      var <- var(withoutItem)
      N <- ncol( withoutItem )
      TOT <- apply( withoutItem , 1, sum)
      alpha <- N/(N-1)*(1-(sum(diag(var))/var(TOT)))
    })
  } else{
    alphaDrop.bin <- NA
  }

  mat <- data.frame( Mean= mean, Scaled.Score = difc, Sample.SD = sx, Sample.SD.bin = sx.bin, CorrAnsw = correct,
                     Min.score = minscore, Max.score = maxscore, Obt.min = obtainedmin, Obt.max = obtainedmax,
                     CutScore= cutscore,ULI.Ord = ULIord, ULI.bin = ULIbin, ULI.default = discrim, ULI.default.bin = discrimBin,
                     Item.total = rix,Item.total.bin = rix.bin,
                     Item.Tot.woi = rix.woi, Item.Tot.woi.bin = rix.woi.bin,
                     Item.Criterion = riy,  Item.Criterion.bin = riy.bin, Item.Reliab = i.rel, Item.Reliab.bin = i.rel.bin,
                     Item.Rel.woi = i.rel.woi, Item.Rel.woi.bin = i.rel.woi.bin, Item.Validity = i.val, Item.Validity.bin = i.val.bin,
                     Alpha.Drop = alphaDrop, Alpha.Drop.bin = alphaDrop.bin)

  names(mat) <- c("Difficulty"  , "Scaled Score" ,  "Sample SD" , "Sample SD binary" , "Correct Answers",
                  "Min score","Max score" ,"Obtained min","Obtained max" ,"Cut score" , "ULI" , "ULI binary", "ULI default",
                  "ULI default binary",
                  "RIT" ,"RIT binary" , "RIR" , "RIR binary"  , "Item Criterion" , "Item.Criterion.bin", "Item.Reliab",
                  "Item.Reliab.bin", "Item.Rel.woi" , "Item.Rel.woi.bin" ,"Item.Validity" ,"Item.Validity.bin","Alpha drop", "Alpha drop Binary" )

  if (add.bin == TRUE){
  return(mat)
    }
  matOrd <- mat[,c(2:3, 6:11,13,15,17,19,21,23, 25,27)]
  return(matOrd)
}
