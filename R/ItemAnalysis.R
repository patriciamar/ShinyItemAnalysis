#' Compute traditional item analysis indices
#'
#' @aliases ItemAnalysis
#'
#' @description \code{ItemAnalysis} function computes various traditional item
#'   analysis indices including difficulty, discrimination and item validity.
#'   For ordinal items the difficulty and discrimination indices take into
#'   account minimal item score as well as range.
#'
#' @param Data matrix or data.frame of items to be examined. Rows represent
#'   respondents, columns represent items.
#' @param criterion vector of criterion values.
#' @param k numeric: number of groups to which may be data.frame x divided by
#'   the total score. Default value is 3.  See \strong{Details}.
#' @param l numeric: lower group. Default value is 1. See \strong{Details}.
#' @param u numeric: upper group. Default value is 3. See \strong{Details}.
#' @param maxscore numeric or vector: maximal score in ordinal items. If
#'   missing, vector of obtained maximal scores is imputed. See
#'   \strong{Details}.
#' @param minscore numeric or vector: minimal score in ordinal items. If
#'   missing, vector of obtained minimal scores is imputed. See
#'   \strong{Details}.
#' @param cutscore numeric or vector: cut-score used for binarization of ordinal
#'   data. If missing, vector of maximal scores is imputed. See
#'   \strong{Details}.
#' @param bin logical: If TRUE, indices are printed also for binarized data. See
#'   \strong{Details}.
#' @param data deprecated. Use argument \code{Data} instead.
#' @param y deprecated. Use argument \code{criterion} instead.
#' @param add.bin deprecated. Use argument \code{bin} instead.
#'
#' @details For ordinal items the difficulty and discrimination indices take
#'   into account minimal item score as well as range.
#'
#'   For calculation of discrimination ULI index, it is possible to specify the
#'   number of groups \code{k}, and which two groups \code{l} and \code{u} are
#'   to be compared.
#'
#'   In ordinal items, difficulty is calculated as difference of average score
#'   divided by range (maximal possible score \code{maxscore} minus minimal
#'   possible score \code{minscore}).
#'
#'   If \code{bin} is set to \code{TRUE}, item analysis of binarized data is
#'   included in the output table. In such a case, \code{cutscore} is used for
#'   binarization. When binarizing the \code{Data}, values greater or equal to
#'   cut-score are set to \code{1}, other values are set to \code{0}.
#'
#' @return \code{ItemAnalysis} function computes various traditional item
#'   analysis indices. Output is a \code{data.frame} with following columns:
#'   \item{\code{Difficulty}}{average score of the item divided by its range. }
#'   \item{\code{Mean}}{average item score. } \item{\code{SD}}{standard
#'   deviation of the item score. } \item{\code{SD.bin}}{standard deviation of
#'   the item score for binarized data. }
#'   \item{\code{Prop.max.score}}{proportion of maximal scores. }
#'   \item{\code{Min.score}}{minimal score specified in \code{minscore}; if not
#'   provided, observed minimal score. } \item{\code{Max.score}}{maximal score
#'   specified in \code{maxscore}; if not provided, observed maximal score. }
#'   \item{\code{obs.min}}{observed minimal score. }
#'   \item{\code{obs.max}}{observed maximal score. }
#'   \item{\code{Cut.Score}}{cut-score specified in \code{cutscore}. }
#'   \item{\code{gULI}}{generalized ULI. } \item{\code{gULI.bin}}{generalized
#'   ULI for binarized data. } \item{\code{ULI}}{discrimination with ULI using
#'   the usual parameters (3 groups, comparing 1st and 3rd). }
#'   \item{\code{ULI.bin}}{discrimination with ULI using the usual parameters
#'   for binarized data (3 groups, comparing 1st and 3rd). }
#'   \item{\code{RIT}}{item-total correlation (correlation between item score
#'   and overall test score). } \item{\code{RIT.bin}}{item-total correlation for
#'   binarized data. } \item{\code{RIR}}{item-rest correlation (correlation
#'   between item score and overall test score without the given item). }
#'   \item{\code{RIR.bin}}{item-rest correlation for binarized data. }
#'   \item{\code{Corr.criterion}}{correlation between item score and criterion
#'   \code{criterion}. } \item{\code{Corr.criterion.bin}}{correlation between
#'   item score and criterion \code{criterion} for binarized data. }
#'   \item{\code{Index.val}}{item validity index calculated as \code{cor(item,
#'   criterion) * sqrt(((N - 1) / N) * var(item))}, see Allen and Yen (1979,
#'   Ch.6.4). } \item{\code{Index.val.bin}}{item validity index for binarized
#'   data. } \item{\code{Index.rel}}{item reliability index calculated as
#'   \code{cor(item, test) * sqrt(((N - 1) / N) * var(item))}, see Allen and Yen
#'   (1979, Ch.6.4). } \item{\code{Index.rel.bin}}{item reliability index for
#'   binarized data. } \item{\code{Index.rel.drop}}{item reliability index
#'   'drop' (scored without item). } \item{\code{Index.rel.drop.bin}}{item
#'   reliability index 'drop' (scored without item) for binarized data. }
#'   \item{\code{Alpha.drop}}{Cronbach's alpha without given item. In case of
#'   two-item dataset, \code{NA}s are returned.}
#'   \item{\code{Alpha.drop.bin}}{Cronbach's alpha without given item, for
#'   binarized data. In case of two-item dataset, \code{NA}s are returned.}
#'   \item{\code{Perc.miss}}{Percentage of missed responses on the particular
#'   item. } \item{\code{Perc.nr}}{Percentage of respondents that did not
#'   reached the item nor the subsequent ones, see \code{\link{recode_nr}}
#'   function for further details. } With \code{bin = TRUE}, indices based on
#'   binarized dataset are also provided and marked with \code{bin} suffix.
#'
#' @author
#' Patricia Martinkova \cr
#' Institute of Computer Science of the Czech Academy of Sciences \cr
#' \email{martinkova@@cs.cas.cz}
#'
#' Jan Netik \cr
#' Institute of Computer Science of the Czech Academy of Sciences \cr
#' \email{netik@@cs.cas.cz}
#'
#' Jana Vorlickova \cr
#' Institute of Computer Science of the Czech Academy of Sciences
#'
#' Adela Hladka \cr
#' Institute of Computer Science of the Czech Academy of Sciences \cr
#' \email{hladka@@cs.cas.cz}
#'
#' @references Martinkova, P., Stepanek, L., Drabinova, A., Houdek, J.,
#'   Vejrazka, M., & Stuka, C. (2017). Semi-real-time analyses of item
#'   characteristics for medical school admission tests. In: Proceedings of the
#'   2017 Federated Conference on Computer Science and Information Systems.
#'   https://doi.org/10.15439/2017F380
#'
#'   Allen, M. J. & Yen, W. M. (1979). Introduction to measurement theory.
#'   Monterey, CA: Brooks/Cole.
#'
#' @seealso \code{\link{DDplot}}, \code{\link{gDiscrim}},
#'   \code{\link{recode_nr}}
#'
#' @examples
#' \dontrun{
#' # loading 100-item medical admission test datasets
#' data(dataMedical, dataMedicalgraded)
#' # binary dataset
#' dataBin <- dataMedical[, 1:100]
#' # ordinal dataset
#' dataOrd <- dataMedicalgraded[, 1:100]
#' # study success is the same for both data sets
#' StudySuccess <- dataMedical[, 102]
#'
#' # item analysis for binary data
#' head(ItemAnalysis(dataBin))
#' # item analysis for binary data using also study success
#' head(ItemAnalysis(dataBin, criterion = StudySuccess))
#'
#' # item analysis for binary data
#' head(ItemAnalysis(dataOrd))
#' # item analysis for binary data using also study success
#' head(ItemAnalysis(dataOrd, criterion = StudySuccess))
#' # including also item analysis for binarized data
#' head(ItemAnalysis(dataOrd,
#'   criterion = StudySuccess, k = 5, l = 4, u = 5,
#'   maxscore = 4, minscore = 0, cutscore = 4, bin = TRUE
#' ))
#' }
#' @export

ItemAnalysis <- function(Data, criterion = "none", k = 3, l = 1, u = 3,
                         maxscore = NULL, minscore = NULL, cutscore = NULL, bin = FALSE,
                         data, y, add.bin) {

  # deprecated args handling
  if (!missing(data)) {
    warning("Argument 'data' is deprecated; please use 'Data' instead.",
      call. = FALSE
    )
    Data <- data
  }

  if (!missing(y)) {
    warning("Argument 'y' is deprecated; please use 'criterion' instead.",
      call. = FALSE
    )
    criterion <- y
  }

  if (!missing(add.bin)) {
    warning("Argument 'add.bin' is deprecated; please use 'bin' instead.",
      call. = FALSE
    )
    bin <- add.bin
  }


  if (!inherits(Data, c("matrix", "data.frame"))) {
    stop("'Data' must be data.frame or matrix. ",
      call. = FALSE
    )
  }

  data_with_nas <- Data
  Data <- as.data.frame(na.omit(Data))

  N <- nrow(Data)
  n <- ncol(Data)

  if (is.null(maxscore)) {
    maxscore <- sapply(Data, max, na.rm = TRUE)
  }
  if (is.null(minscore)) {
    minscore <- sapply(Data, min, na.rm = TRUE)
  }
  if (is.null(cutscore)) {
    cutscore <- sapply(Data, max, na.rm = TRUE)
  } else {
    if (length(cutscore) == 1) {
      cutscore <- rep(cutscore, n)
    }
  }

  if (bin) {
    dataBin <- Data
    dataBin[] <- dataBin == matrix(rep(cutscore, each = N), ncol = n, nrow = N)
    dataBin[] <- as.data.frame(sapply(dataBin, as.numeric))
    # minscoreB <- sapply(dataBin, min, na.rm = TRUE)
    # maxscoreB <- sapply(dataBin, max, na.rm = TRUE)
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

  # total score
  total_score_ord <- rowSums(Data, na.rm = TRUE)
  # total score without item
  total_score_ord_without_item <- total_score_ord - Data

  if (bin) {
    total_score_bin <- rowSums(dataBin, na.rm = TRUE)
    total_score_bin_without_item <- total_score_bin - dataBin
  }

  # average item score
  average_item_score <- colMeans(Data, na.rm = TRUE)
  # observed min/max
  observed_min <- sapply(Data, min, na.rm = TRUE)
  observed_max <- sapply(Data, max, na.rm = TRUE)

  # proportion of maximum possible scores
  prop_max_score <- sapply(1:n, function(i) sum(Data[, i] == maxscore[i], na.rm = TRUE)) / N

  # gULI ordinal
  gULI_ord <- as.numeric(
    gDiscrim(Data,
      minscore = minscore, maxscore = maxscore,
      k = k, l = l, u = u
    )
  )
  # ULI ordinal
  ULI_ord <- as.numeric(
    gDiscrim(Data,
      minscore = minscore, maxscore = maxscore,
      k = 3, l = 1, u = 3
    )
  )

  if (bin) {
    # gULI binary
    gULI_bin <- as.numeric(
      gDiscrim(dataBin,
        k = k, l = l, u = u
      )
    )
    # ULI binary
    ULI_bin <- as.numeric(
      gDiscrim(dataBin,
        k = 3, l = 1, u = 3
      )
    )
  } else {
    gULI_bin <- NA
    ULI_bin <- NA
  }

  # RIR ordinal
  RIR_ord <- diag(cor(Data, total_score_ord_without_item, use = "complete"))
  # RIT ordinal
  RIT_ord <- as.vector(cor(Data, total_score_ord, use = "complete"))

  # RIR and RIT binary
  if (bin) {
    RIR_bin <- diag(cor(dataBin, total_score_bin_without_item, use = "complete"))
    RIT_bin <- as.vector(cor(dataBin, total_score_bin, use = "complete"))
    # RIR_bin[(maxscoreB - minscoreB) < 1] <- 0
  } else {
    RIR_bin <- NA
    RIT_bin <- NA
  }

  # Average scaled item score (difficulty)
  difficulty <- (average_item_score - minscore) / (maxscore - minscore)

  # SD and norming term
  SD_ord <- sapply(Data, sd)
  vx_ord <- ((N - 1) / N) * SD_ord^2

  # Item-criterion correlation
  if (any(criterion == "none", na.rm = TRUE)) {
    corr_criterion_ord <- NA
    index_validity_ord <- NA
  } else {
    criterion <- as.numeric(criterion)
    corr_criterion_ord <- cor(data_with_nas, criterion, use = "complete")
    index_validity_ord <- corr_criterion_ord * sqrt(vx_ord)
  }
  index_RIT_ord <- RIT_ord * sqrt(vx_ord)
  index_RIR_ord <- RIR_ord * sqrt(vx_ord)

  # Item analysis of binarized data
  if (bin) {
    SD_bin <- apply(dataBin, 2, sd)
    vx_bin <- ((N - 1) / N) * SD_bin^2
    if (any(criterion == "none", na.rm = TRUE)) {
      corr_criterion_bin <- NA
      index_validity_bin <- NA
    } else {
      criterion <- as.numeric(criterion)
      corr_criterion_bin <- cor(dataBin, criterion, use = "complete")
      index_validity_bin <- corr_criterion_bin * sqrt(vx_bin)
    }
    index_RIT_bin <- RIT_bin * sqrt(vx_bin)
    index_RIR_bin <- RIR_bin * sqrt(vx_bin)
  } else {
    SD_bin <- NA
    index_validity_bin <- NA
    index_RIT_bin <- NA
    index_RIR_bin <- NA
    corr_criterion_bin <- NA
  }

  # Alpha without item
  alpha_drop_ord <- if (n > 2) {
    sapply(1:n, function(i) {
      withoutItem <- Data[, -i]
      var <- var(withoutItem)
      N <- ncol(withoutItem)
      TOT <- rowSums(withoutItem, na.rm = TRUE)
      alpha <- N / (N - 1) * (1 - (sum(diag(var)) / var(TOT)))
    })
  } else {
    # psych solution - uses cov between the remaining and the dropped item
    # covar <- cov(Data)
    # covar[1, 2] / c(covar[2, 2], covar[1, 1])

    # return NAs, as the alpha for one item does not seem to be well defined
    rep(NA_real_, n)
  }

  if (bin) {
    alpha_drop_bin <- if (n > 2) {
      sapply(1:n, function(i) {
        withoutItem <- dataBin[, -i]
        var <- var(withoutItem)
        N <- ncol(withoutItem)
        TOT <- rowSums(withoutItem)
        alpha <- N / (N - 1) * (1 - (sum(diag(var)) / var(TOT)))
      })
    } else {
      # return NAs, as the alpha for one item does not make sense
      rep(NA_real_, n)
    }
  } else {
    alpha_drop_bin <- NA
  }

  # missed items (NAs)
  missed <- sapply(
    data_with_nas,
    function(x) {
      sum(is.na(x)) / length(x) * 100
    }
  )

  # not-reached items (coded as 99)
  prop_nr <- sapply(
    recode_nr(data_with_nas),
    function(x) {
      sum(x == 99, na.rm = TRUE) / length(x) * 100
    }
  )

  mat <- data.frame(
    Difficulty = difficulty, Mean = average_item_score, SD = SD_ord, SD.bin = SD_bin, Prop.max.score = prop_max_score,
    Min.score = minscore, Max.score = maxscore, Obs.min = observed_min, Obs.max = observed_max,
    Cut.score = cutscore,
    gULI = gULI_ord, gULI.bin = gULI_bin, ULI = ULI_ord, ULI.bin = ULI_bin,
    RIT = RIT_ord, RIT.Bin = RIT_bin, RIR = RIR_ord, RIR.Bin = RIR_bin,
    Corr.criterion = corr_criterion_ord, Corr.criterion.bin = corr_criterion_bin,
    Index.val = index_validity_ord, Index.val.bin = index_validity_bin,
    Index.rel = index_RIT_ord, Index.rel.bin = index_RIT_ord,
    Index.rel.drop = index_RIR_ord, Index.rel.drop.bin = index_RIR_bin,
    Alpha.drop = alpha_drop_ord, Alpha.drop.bin = alpha_drop_bin,
    Perc.miss = missed,
    Perc.nr = prop_nr
  )

  var.ord <- c(
    "Difficulty", "Mean", "SD", "Prop.max.score",
    "Min.score", "Max.score", "Obs.min", "Obs.max",
    "gULI", "ULI", "RIT", "RIR",
    "Corr.criterion", "Index.val", "Index.rel", "Index.rel.drop",
    "Alpha.drop", "Perc.miss", "Perc.nr"
  )

  var.criterion <- c("Corr.criterion", "Corr.criterion.bin", "Index.val", "Index.val.bin")

  if (!bin) {
    mat <- mat[, var.ord]
  }

  if (any(criterion == "none", na.rm = TRUE)) {
    mat <- mat[, !(colnames(mat) %in% var.criterion)]
  }

  return(mat)
}
