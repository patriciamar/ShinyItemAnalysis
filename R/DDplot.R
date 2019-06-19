#' Graphical representation of difficulty and (generalized) discrimination in item analysis
#'
#' @aliases DDplot
#'
#' @description Plots difficulty and (generalized) discrimination for items ordered by difficulty.
#'
#' @param data numeric: binary or ordinal data matrix or data frame. See \strong{Details}.
#' @param item.names character: the names of items.
#' @param k numeric: number of groups to which may be data.frame x divided by the total score.
#' Default value is 3.  See \strong{Details}.
#' @param l numeric: lower group. Default value is 1. See \strong{Details}.
#' @param u numeric: upper group. Default value is 3. See \strong{Details}.
#' @param discrim character: type of discrimination index to be calculated. Deafult value is "ULI". See \strong{Details}.
#' @param maxscore vector or numeric: maximal scores of items. If numeric, the same maximal score is used for all items. If missing, vector of achieved maximal scores is calculated and used in calculations.
#' @param minscore vector or numeric: minimal scores of items. If numeric, the same minimal score is used for all items. If missing, vector of achieved minimal scores is calculated and used in calculations.
#' @param bin logical: should the ordinal data be binarized. Deafult value is FALSE. See \strong{Details}.
#' @param cutscore vector or numeric: cutscore used to binarize the data.set. If numeric, the same cutscore is used for all items. If missing, vector of maximal scores is used in calculations.
#' @param average.score logical: should average score of the item disaplyed instead of difficulty. Default
#' value is FALSE. See \strong{Details}.
#'
#' @usage DDplot(data, item.names, k = 3, l = 1, u = 3,
#' discrim = "ULI", maxscore, minscore, bin = FALSE, cutscore, average.score = FALSE)
#'
#' @details
#' The \code{data} is a matrix or data frame whose rows represents examinee answers
#' (\code{1} correct, \code{0} incorrect, or ordinal item scores) and columns correspond to the items.
#'
#' The \code{item.names} argument stands for names of items. If not specified, the names of dataset columns are used.
#' Difficulty and discrimination indices are plotted for each item, items are ordered by their difficulty.
#'
#' Discrimination is calculated using method specified in \code{discrim}. Default option "ULI"
#' calculates difference in ratio of correct answers in upper and lower third of students.
#' "RIT" index caluclates correlation between item score and test total score.
#' "RIR" index caclulates correlation between item score and total score for the rest of the items.
#' With option "none", only difficulty is displayed.
#'
#' "ULI" index can be generalized using arguments \code{k}, \code{l} and \code{u}. Generalized ULI
#' discrimination is then computed as follows: The function takes data on individuals,
#' computes their total test score and then divides individuals into \code{k} groups. The lower and
#' upper group are determined by \code{l} and \code{u} parameters, i.e.  l-th and u-th group where
#' the ordering is defined by increasing total score.
#'
#' For ordinal data, difficulty is defined as relative score (achieved - minimal)/(maximal - minimal).
#' Minimal score can be specified by \code{minscore}, maximal score can be specified by \code{maxscore}.
#' Average score of items can be displayed with argument \code{average.score = T}. Note that for binary
#' data difficulty estimate is the same as average score of the item.
#'
#' Binarization of data is allowed in \code{bin}, for this purpose \code{cutscore} is used.
#'
#'
#' @author
#' Adela Hladka \cr
#' Institute of Computer Science, The Czech Academy of Sciences \cr
#' Faculty of Mathematics and Physics, Charles University \cr
#' hladka@cs.cas.cz \cr
#'
#' Lubomir Stepanek \cr
#' First Faculty of Medicine, Charles University \cr
#'
#' Jana Vorlickova \cr
#' Institute of Computer Science, The Czech Academy of Sciences \cr
#' Faculty of Mathematics and Physics, Charles University \cr
#'
#' Patricia Martinkova \cr
#' Institute of Computer Science, The Czech Academy of Sciences \cr
#' martinkova@cs.cas.cz \cr
#'
#'
#' @references
#' Martinkova, P., Stepanek, L., Drabinova, A., Houdek, J., Vejrazka, M., & Stuka, C. (2017).
#' Semi-real-time analyses of item characteristics for medical school admission tests.
#' In: Proceedings of the 2017 Federated Conference on Computer Science and Information Systems.
#'
#' @note
#' Generalized discrimination is calculated by \code{\link{gDiscrim}} function.
#'
#' @seealso
#' \code{\link{gDiscrim}}, \code{\link{discrim}}
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
#' # DDplot of binary data set
#' DDplot(dataBin)
#' # compared to DDplot using ordinal data set and 'bin = TRUE'
#' DDplot(dataOrd, bin = TRUE)
#' # compared to binarized data set using bin = TRUE and cutscore equal to 3
#' DDplot(dataOrd, bin = TRUE, cutscore = 3)
#'
#' # DDplot of binary data using generalized ULI
#' # discrimination based on 5 groups, comparing 4th and 5th
#' DDplot(dataBin, k = 5, l = 4, u = 5)
#'
#' # DDplot of ordinal data set using ULI
#' DDplot(dataOrd)
#' # DDplot of ordinal data set using generalized ULI
#' # discrimination based on 5 groups, comparing 4th and 5th
#' DDplot(dataOrd, k = 5, l = 4, u = 5)
#' # DDplot of ordinal data set using RIT
#' DDplot(dataOrd, discrim = "RIT")
#' # DDplot of ordinal data set using RIR
#' DDplot(dataOrd, discrim = "RIR")
#' # DDplot of ordinal data set disaplaying only difficulty
#' DDplot(dataBin, discrim = "none")
#'
#' # DDplot of ordinal data set disaplaying difficulty estimates
#' DDplot(dataOrd)
#' # DDplot of ordinal data set disaplaying average item scores
#' DDplot(dataOrd, average.score = TRUE)
#' }
#' @export
#' @import difNLR
#' difR
#' shiny
#' @importFrom corrplot corrplot
#' @importFrom CTT score
#' @importFrom cowplot plot_grid
#' @importFrom deltaPlotR deltaPlot
#' @importFrom ggplot2 aes aes_string coord_flip element_blank element_line element_rect element_text geom_abline
#' ggplot_build position_dodge geom_histogram geom_hline geom_line geom_point geom_ribbon geom_text ggplot
#' ggsave ggtitle labs scale_color_manual scale_colour_manual scale_fill_manual scale_linetype_manual
#' scale_shape_manual scale_size_continuous scale_x_continuous scale_x_discrete scale_y_continuous
#' scale_y_reverse
#' stat_function stat_summary theme theme_bw unit xlab xlim ylab ylim
#' @importFrom graphics lines plot plot.new
#' @importFrom grDevices dev.off hcl png rainbow recordPlot
#' @importFrom ltm ltm rasch tpm
#' @importFrom mirt fscores itemfit mirt
#' @importFrom moments kurtosis skewness
#' @importFrom nnet multinom
#' @importFrom psych alpha polychoric
#' @importFrom psychometric item.exam
#' @importFrom reshape2 dcast melt
#' @importFrom rmarkdown render
#' @importFrom shinyjs show hide useShinyjs
#' @importFrom stats aggregate coef complete.cases cor deriv deriv3 fitted glm median na.exclude nls quantile relevel sd vcov xtabs
#' @importFrom stringr str_sub
#' @importFrom utils data head read.csv


DDplot <- function (data, item.names, k = 3, l = 1, u = 3, discrim = "ULI",
                    maxscore, minscore, bin = FALSE, cutscore, average.score = FALSE)
{
  if(!is.matrix(data) & !is.data.frame(data)) {
    stop("'data' must be data frame or matrix ", call. = FALSE)
  }
  if(missing(maxscore)) {
    maxscore <- apply(data, 2, max, na.rm = T)
  }
  if(missing(minscore)) {
    minscore <- apply(data, 2, min, na.rm = T)
  }
  if(missing(cutscore)) {
    cutscore <- apply(data, 2, max, na.rm = T)
  } else {
    if(length(cutscore) == 1){
      cutscore <- rep(cutscore, ncol(data))
    }
  }
  data <- data[complete.cases(data), ]
  if(bin == TRUE) {
    data2 <- data
    for(i in 1:dim(data)[2]){
      data[data2[, i] >= cutscore[i], i] <- 1
      data[data2[, i] < cutscore[i], i] <- 0

    }
    head(data)
    minscore <- apply(data, 2, min, na.rm = T)
    maxscore <- apply(data, 2, max, na.rm = T)
  }
  if(missing(item.names)) {
    item.names <- colnames(data)
  }
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
  diffName <- c("Difficulty", "Difficulty", "Average score")
  discName <- c("Discrimination ULI", "Discrimination RIR", "Discrimination RIT")
  xlabel <- c("Item (ordered by difficulty)",
              "Item (ordered by difficulty)",
              "Item (ordered by average item score)")
  average <- apply(data, 2, mean)
  if(discrim == "ULI") {
    disc <- as.numeric(gDiscrim(data, minscore = minscore, maxscore = maxscore,
                                k = k, l = l, u = u))
    i <- 1
  }
  if(discrim ==  "RIR") {
    TOT <- apply(data, 1, sum)
    TOT.woi <- TOT - (data)
    disc <- diag(cor(data, TOT.woi, use = "complete"))
    i <-2
  }
  if(discrim ==  "RIT") {
    TOT <- apply(data, 1, sum)
    disc <- t(cor(data, TOT, use = "complete"))
    i <-3
  }
  if(!all((maxscore - minscore) != 0)){
    warning("'cutscore' is equal to 'minscore' for some item")

    difc <- (average - minscore)/(maxscore - minscore)
    difc[(maxscore - minscore) == 0] <- 1
    disc[(maxscore - minscore) == 0] <- 0
  } else {

    if(average.score) {
      difc <- average
    } else {
      difc <- (average - minscore)/(maxscore - minscore)
    }
  }
  if(max(maxscore - minscore) > 1){
    j <- ifelse(average.score, 3, 2)
  } else {
    j <- 1
  }
  if(discrim != "none") {
    if(any(disc < 0)) {
      warning("Estimated discrimination is lower than 0.",
              call. = F)
    }
    value <- c(rbind(difc, disc)[, order(difc)])
    parameter <- rep(c(diffName[j], discName[i]), ncol(data))
    item <- factor(rep(item.names[order(difc)], each = 2), levels = item.names[order(difc)])
    df <- data.frame(item, parameter, value)
    col <- c("red", "darkblue")
    ggplot(df, aes_string(x = "item", y = "value", fill = "parameter", color = "parameter")) +
      stat_summary(fun.y = mean, position = "dodge", geom = "bar", alpha = 0.7, width = 0.8) +
      geom_hline(yintercept = 0.2) +
      xlab(xlabel[j]) +
      ylab(paste0(diffName[j], "/", discName[i])) +
      scale_y_continuous(expand = c(0, 0),
                         limits = c(min(min(df$value) - 0.01, 0),
                                    max(max(df$value) + 0.01*maxscore, 1))) +
      scale_fill_manual(breaks = parameter, values = col) +
      scale_colour_manual(breaks = parameter, values = col) +
      theme_app() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
            legend.position = c(0.01, 0.98),
            legend.justification = c(0, 1),
            legend.spacing.x = unit(0.1, 'cm'))
  } else {
    value <- difc[order(difc)]
    parameter <- rep(c(diffName[j]), ncol(data))
    item <- factor(item.names[order(difc)], levels = item.names[order(difc)])
    df <- data.frame(item, parameter, value)
    col <- c("red", "darkblue")
    ggplot(df, aes_string(x = "item", y = "value", fill = "parameter", color = "parameter")) +
      stat_summary(fun.y = mean, position = "dodge", geom = "bar", alpha = 0.7, width = 0.8) +
      xlab(xlabel[j]) + ylab(diffName[j]) +
      scale_y_continuous(expand = c(0, 0),
                         limits = c(min(min(df$value) - 0.01, 0),
                                    max(max(df$value) + 0.01, 1))) +
      scale_fill_manual(breaks = parameter,
                        values = col) +
      scale_colour_manual(breaks = parameter, values = col) +
      theme_app() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
            legend.position = c(0.01, 0.98),
            legend.justification = c(0, 1),
            legend.spacing.x = unit(0.1, 'cm'))
  }
}

