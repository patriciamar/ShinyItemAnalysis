#' Plot difficulties and discriminations/item validity
#'
#' @aliases DDplot
#'
#' @description Plots difficulty and (generalized) discrimination or criterion
#'   validity for items of the multi-item measurement test using the
#'   \pkg{ggplot2} package. Difficulty and discrimination/validity indices are
#'   plotted for each item, items are ordered by their difficulty.
#'
#' @param Data numeric: binary or ordinal data \code{matrix} or
#'   \code{data.frame} which rows represent examinee answers (\code{1} correct,
#'   \code{0} incorrect, or ordinal item scores) and columns correspond to the
#'   items.
#' @param item.names character: the names of items. If not specified, the names
#'   of \code{Data} columns are used.
#' @param discrim character: type of discrimination index to be calculated.
#'   Possible values are \code{"ULI"} (default), \code{"RIT"}, \code{"RIR"}, and
#'   \code{"none"}. See \strong{Details}.
#' @param k numeric: number of groups to which data may be divided by the total
#'   score to estimate discrimination using \code{discrim = "ULI"}. Default
#'   value is 3.  See \strong{Details}.
#' @param l numeric: lower group. Default value is 1. See \strong{Details}.
#' @param u numeric: upper group. Default value is 3. See \strong{Details}.
#' @param maxscore numeric: maximal scores of items. If single number is
#'   provided, the same maximal score is used for all items. If missing, vector
#'   of achieved maximal scores is calculated and used in calculations.
#' @param minscore numeric: minimal scores of items. If single number is
#'   provided, the same maximal score is used for all items. If missing, vector
#'   of achieved maximal scores is calculated and used in calculations.
#' @param bin logical: should the ordinal data be binarized? Default value is
#'   \code{FALSE}. In case that \code{bin = TRUE}, all values of \code{Data}
#'   equal or greater than \code{cutscore} are marked as \code{1} and all values
#'   lower than \code{cutscore} are marked as \code{0}.
#' @param cutscore numeric: cut-score used to binarize \code{Data}. If numeric,
#'   the same cut-score is used for all items. If missing, vector of maximal
#'   scores is used in calculations.
#' @param average.score logical: should average score of the item be displayed
#'   instead of difficulty? Default value is \code{FALSE}. See \strong{Details}.
#' @param thr numeric: value of discrimination threshold. Default value is 0.2.
#'   With \code{thr = NULL}, no horizontal line is displayed in the plot.
#' @param criterion numeric or logical vector: values of criterion. If supplied,
#'   \code{disrim} argument is ignored and item-criterion correlation (validity)
#'   is displayed instead. Default value is \code{"none"}.
#' @param val_type character: criterion validity measure. Possible values are
#'   \code{"simple"} (correlation between item score and validity criterion;
#'   default) and \code{"index"} (item validity index calculated as
#'   \code{cor(item, criterion) * sqrt(((N - 1) / N) * var(item))}, where N is
#'   number of respondents, see Allen & Yen, 1979, Ch. 6.4, for details). The
#'   argument is ignored if user does not supply any \code{criterion}.
#' @param data deprecated. Use argument \code{Data} instead.
#'
#' @details Discrimination is calculated using method specified in
#' \code{discrim}. Default option \code{"ULI"} calculates difference in ratio of
#' correct answers in upper and lower third of students. \code{"RIT"} index
#' calculates correlation between item score and test total score. \code{"RIR"}
#' index calculates correlation between item score and total score for the rest
#' of the items. With option \code{"none"}, only difficulty is displayed.
#'
#' \code{"ULI"} index can be generalized using arguments \code{k}, \code{l} and
#' \code{u}. Generalized ULI discrimination is then computed as follows: The
#' function takes data on individuals, computes their total test score and then
#' divides individuals into \code{k} groups. The lower and upper group are
#' determined by \code{l} and \code{u} parameters, i.e.  l-th and u-th group
#' where the ordering is defined by increasing total score.
#'
#' For ordinal data, difficulty is defined as relative score (achieved -
#' minimal)/(maximal - minimal). Minimal score can be specified by
#' \code{minscore}, maximal score can be specified by \code{maxscore}. Average
#' score of items can be displayed with argument \code{average.score = TRUE}.
#' Note that for binary data difficulty estimate is the same as average score of
#' the item.
#'
#' Note that all correlations are estimated using Pearson correlation
#' coefficient.
#'
#' @author
#' Adela Hladka \cr
#' Institute of Computer Science of the Czech Academy of Sciences \cr
#' \email{hladka@@cs.cas.cz} \cr
#'
#' Lubomir Stepanek \cr
#' Charles University \cr
#'
#' Jana Vorlickova \cr
#' Institute of Computer Science of the Czech Academy of Sciences \cr
#'
#' Patricia Martinkova \cr
#' Institute of Computer Science of the Czech Academy of Sciences \cr
#' \email{martinkova@@cs.cas.cz} \cr
#'
#' @references
#' Allen, M. J., & Yen, W. M. (1979). Introduction to measurement theory.
#' Monterey, CA: Brooks/Cole.
#'
#' Martinkova, P., Stepanek, L., Drabinova, A., Houdek, J., Vejrazka, M., &
#' Stuka, C. (2017). Semi-real-time analyses of item characteristics for medical
#' school admission tests. In: Proceedings of the 2017 Federated Conference on
#' Computer Science and Information Systems.
#'
#' @seealso
#' \code{\link[ShinyItemAnalysis]{gDiscrim}} for calculation of generalized ULI \cr
#' \code{\link[ggplot2]{ggplot}} for general function to plot a \code{"ggplot"} object
#'
#' @examples
#' # loading 100-item medical admission test datasets
#' data(dataMedical, dataMedicalgraded)
#' # binary dataset
#' dataBin <- dataMedical[, 1:100]
#' # ordinal dataset
#' dataOrd <- dataMedicalgraded[, 1:100]
#'
#' # DDplot of binary dataset
#' DDplot(dataBin)
#' \dontrun{
#' # DDplot of binary dataset without threshold
#' DDplot(dataBin, thr = NULL)
#' # compared to DDplot using ordinal dataset and 'bin = TRUE'
#' DDplot(dataOrd, bin = TRUE)
#' # compared to binarized dataset using bin = TRUE and cut-score equal to 3
#' DDplot(dataOrd, bin = TRUE, cutscore = 3)
#'
#' # DDplot of binary data using generalized ULI
#' # discrimination based on 5 groups, comparing 4th and 5th
#' # threshold lowered to 0.1
#' DDplot(dataBin, k = 5, l = 4, u = 5, thr = 0.1)
#'
#' # DDplot of ordinal dataset using ULI
#' DDplot(dataOrd)
#' # DDplot of ordinal dataset using generalized ULI
#' # discrimination based on 5 groups, comparing 4th and 5th
#' # threshold lowered to 0.1
#' DDplot(dataOrd, k = 5, l = 4, u = 5, thr = 0.1)
#' # DDplot of ordinal dataset using RIT
#' DDplot(dataOrd, discrim = "RIT")
#' # DDplot of ordinal dataset using RIR
#' DDplot(dataOrd, discrim = "RIR")
#' # DDplot of ordinal dataset displaying only difficulty
#' DDplot(dataBin, discrim = "none")
#'
#' # DDplot of ordinal dataset displaying difficulty estimates
#' DDplot(dataOrd)
#' # DDplot of ordinal dataset displaying average item scores
#' DDplot(dataOrd, average.score = TRUE)
#'
#' # item difficulty / criterion validity plot for data with criterion
#' data(GMAT, package = "difNLR")
#' DDplot(GMAT[, 1:20], criterion = GMAT$criterion, val_type = "simple")
#' }
#' @importFrom ggplot2 geom_col ylab scale_y_continuous scale_fill_manual unit aes_string stat_summary scale_colour_manual
#' @export

DDplot <- function(Data, item.names, discrim = "ULI", k = 3, l = 1, u = 3,
                   maxscore, minscore, bin = FALSE, cutscore, average.score = FALSE,
                   thr = 0.2, criterion = "none", val_type = "simple", data) {

  # deprecated args handling
  if (!missing(data)) {
    warning("Argument 'data' is deprecated; please use 'Data' instead.",
      call. = FALSE
    )
    Data <- data
  }

  if (!is.matrix(Data) & !is.data.frame(Data)) {
    stop("'Data' must be data.frame or matrix. ", call. = FALSE)
  }
  if (any(criterion != "none", na.rm = TRUE)) {
    if (!is.null(dim(criterion))) {
      stop("'criterion' must be numeric or logical vector. ", call. = FALSE)
    } else if (length(criterion) != nrow(Data)) {
      stop("'criterion' must be numeric or logical vector of the same length as a number of observations in 'data'. ", call. = FALSE)
    }
  }
  if (missing(maxscore)) {
    maxscore <- sapply(Data, max, na.rm = TRUE)
  }
  if (missing(minscore)) {
    minscore <- sapply(Data, min, na.rm = TRUE)
  }
  if (missing(cutscore)) {
    cutscore <- sapply(Data, max, na.rm = TRUE)
  } else {
    if (length(cutscore) == 1) {
      cutscore <- rep(cutscore, ncol(Data))
    }
  }
  Data <- Data[complete.cases(Data), ]
  if (bin) {
    data2 <- Data
    for (i in 1:dim(Data)[2]) {
      Data[data2[, i] >= cutscore[i], i] <- 1
      Data[data2[, i] < cutscore[i], i] <- 0
    }
    head(Data)
    minscore <- sapply(Data, min, na.rm = TRUE)
    maxscore <- sapply(Data, max, na.rm = TRUE)
  }
  if (missing(item.names)) {
    item.names <- colnames(Data)
  }
  if (u > k) {
    stop("'u' needs to be lower or equal to 'k'. ", call. = FALSE)
  }
  if (l > k) {
    stop("'l' needs to be lower than 'k'. ", call. = FALSE)
  }
  if (l <= 0) {
    stop("'l' needs to be greater than 0. ", call. = FALSE)
  }
  if (l >= u) {
    stop("'l' needs be lower than 'u'. ", call. = FALSE)
  }
  if (!is.null(thr)) {
    if (!is.numeric(thr)) {
      stop("'thr' needs to be either NULL or numeric. ", call. = FALSE)
    } else if (thr < 0 | thr > 1) {
      warning("'thr' needs value between 0 and 1. Current threshold is not displayed in the plot. ",
        call. = FALSE
      )
    }
  }

  diffName <- c("Difficulty", "Difficulty", "Average score")
  discName <- c("Discrimination ULI", "Discrimination RIR", "Discrimination RIT", "Criterion validity", "Validity index")
  xlabel <- c(
    "Item (ordered by difficulty)",
    "Item (ordered by difficulty)",
    "Item (ordered by average item score)"
  )
  average <- colMeans(Data, na.rm = TRUE)
  if (discrim == "ULI") {
    disc <- as.numeric(gDiscrim(Data,
      minscore = minscore, maxscore = maxscore,
      k = k, l = l, u = u
    ))
    i <- 1
  }
  if (discrim == "RIR") {
    TOT <- rowSums(Data)
    TOT.woi <- TOT - Data
    disc <- diag(cor(Data, TOT.woi, use = "complete"))
    i <- 2
  }
  if (discrim == "RIT") {
    TOT <- rowSums(Data)
    disc <- t(cor(Data, TOT, use = "complete"))
    i <- 3
  }

  # when criterion is not 'none', 'disc' var is used to store item-crit cor
  if (any(criterion != "none", na.rm = TRUE)) {
    item_crit_cor <- t(cor(Data, criterion, use = "complete"))

    if (val_type == "simple") {
      disc <- item_crit_cor
      i <- 4
    } else if (val_type == "index") {
      N <- nrow(Data)
      sx <- sapply(Data, sd)
      vx <- ((N - 1) / N) * sx^2
      disc <- item_crit_cor * sqrt(vx)
      i <- 5
    } else {
      stop(
        "'val_type' needs to be either 'simple' (item-criterion correlation), or 'index' (item validity index). ",
        call. = FALSE
      )
    }
  }

  if (!all((maxscore - minscore) != 0)) {
    warning("'cutscore' is equal to 'minscore' for some item. ")

    difc <- (average - minscore) / (maxscore - minscore)
    difc[(maxscore - minscore) == 0] <- 1
    disc[(maxscore - minscore) == 0] <- 0
  } else {
    if (average.score) {
      difc <- average
    } else {
      difc <- (average - minscore) / (maxscore - minscore)
    }
  }
  if (max(maxscore - minscore) > 1) {
    j <- ifelse(average.score, 3, 2)
  } else {
    j <- 1
  }
  if (discrim != "none" | any(criterion != "none", na.rm = TRUE)) {
    if (any(disc < 0)) {
      ifelse(any(criterion != "none", na.rm = TRUE),
        warning("Item-criterion correlation is lower than 0. ", call. = FALSE),
        warning("Estimated discrimination is lower than 0. ", call. = FALSE)
      )
    }

    value <- c(rbind(difc, disc)[, order(difc)])
    parameter <- rep(c(diffName[j], discName[i]), ncol(Data))
    parameter <- factor(parameter, levels = parameter[1:2])
    item <- factor(rep(item.names[order(difc)], each = 2), levels = item.names[order(difc)])
    df <- data.frame(item, parameter, value)
    col <- c("red", "darkblue")

    g <-
      ggplot(df, aes(item,
        value,
        fill = parameter,
        color = parameter
      )) +
      geom_col(
        position = "dodge",
        alpha = 0.7,
        width = 0.8
      ) +
      xlab(xlabel[j]) +
      ylab(paste0(diffName[j], "/", discName[i])) +
      scale_y_continuous(
        expand = c(0, 0),
        limits = c(
          min(min(df$value) - 0.01, 0),
          max(max(df$value) + 0.01 * maxscore, 1)
        )
      ) +
      scale_fill_manual(breaks = parameter, values = col) +
      scale_colour_manual(breaks = parameter, values = col) +
      theme_app() +
      theme(
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        legend.position = c(0.01, 0.98),
        legend.justification = c(0, 1),
        legend.spacing.x = unit(0.1, "cm")
      )

    if (!is.null(thr)) {
      g <- g + geom_hline(yintercept = thr, col = "gray30")
    }
  } else {
    value <- difc[order(difc)]
    parameter <- rep(c(diffName[j]), ncol(Data))
    item <- factor(item.names[order(difc)], levels = item.names[order(difc)])
    df <- data.frame(item, parameter, value)
    col <- c("red", "darkblue")
    g <- ggplot(df, aes_string(
      x = "item",
      y = "value",
      fill = "parameter",
      color = "parameter"
    )) +
      stat_summary(
        fun = mean, position = "dodge", geom = "bar",
        alpha = 0.7, width = 0.8
      ) +
      xlab(xlabel[j]) +
      ylab(diffName[j]) +
      scale_y_continuous(
        expand = c(0, 0),
        limits = c(
          min(min(df$value) - 0.01, 0),
          max(max(df$value) + 0.01, 1)
        )
      ) +
      scale_fill_manual(
        breaks = parameter,
        values = col
      ) +
      scale_colour_manual(breaks = parameter, values = col) +
      theme_app() +
      theme(
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        legend.position = c(0.01, 0.98),
        legend.justification = c(0, 1),
        legend.spacing.x = unit(0.1, "cm")
      )
  }
  g
}
