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
#' @param bin logical: should the ordinal data be binarized. Deafult value is FALSE. See \strong{Details}.
#' @param maxscore vector or numeric: maximal scores of items. If numeric, the same maximal score is used for all items. If missing, vector of achieved maximal scores is calculated and used in calculations.
#' @param minscore vector or numeric: minimal scores of items. If numeric, the same minimal score is used for all items. If missing, vector of achieved minimal scores is calculated and used in calculations.
#' @param cutscore vector or numeric: cutscore used to binarize the data.set. If numeric, the same cutscore is used for all items. If missing, vector of maximal scores is used in calculations.
#'
#' @usage DDplot(data, item.names, k = 3, l = 1, u = 3,
#'               discrim = "ULI", bin = FALSE, maxscore, minscore, cutscore)
#'
#' @details
#' The \code{data} is a matrix or data frame whose rows represents examinee answers
#' ("1" correct, "0" incorrect, or ordinal item scores) and columns correspond to the items. The \code{item.names}
#' argument stands for names of items. If not specified, the names of dataset columns are used.
#' Difficulty and discrimination indices are plotted for each item, items are ordered by their difficulty.
#' Discrimination is calculated using method specified in \strong{discrim}. ULI calculates difference
#' in ratio of correct answers in upper and lower third of students.
#' RIT index caluclates correlation between item score and test total score.
#' RIR index caclulates correlation between item score and total score for the rest of the items.
#' Generalized ULI discrimination is computed as follows: The function takes data on individuals,
#' computes their total test score and then divides individuals into \code{k} groups. The lower and
#' upper group are determined by \code{l} and \code{u} parameters, i.e.  l-th and u-th group where
#' the ordering is defined by increasing total score.
#' For ordinal data, difficulty is defined as relative score (achieved - minimal)/(maximal - minimal).
#' Minimal score can be specified by \code{minscore}, maximal score can be specified by \code{maxscore}.
#' Binarization of data is allowed in \strong{bin}, for this purpose \strong{cutscore} is used.
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
#' Jana Vorlickova  \cr
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
#' # Binary data set
#' data(dataMedical)
#' dataBin <- dataMedical[, 1:100]
#' # Ordinal data set
#' data(dataMedicalgraded)
#' dataOrd <- dataMedicalgraded[, 1:100]
#'
#' # Difficulty/Discrimination plot of dataMedical data set
#' DDplot(dataBin)
#' # Compared to DDplot using ordinal data set and 'bin=TRUE'
#' DDplot(dataOrd, discrim = "ULI", bin=TRUE)
#'
#' # Difficulty/Discrimination plot of dataMedical data set
#' # discrimination based on 5 groups, comparing 4th and 5th
#' DDplot(dataBin, k = 5, l = 4, u = 5)
#'
# Ordinal data set
# Difficulty/Discrimination ULI plot of dataMedicalgraded data set
#' DDplot(dataOrd)
#'
#' # Difficulty/Discrimination plot of dataMedicalgraded data set
#' # discrimination ULI based on 5 groups, comparing 4th and 5th
#' DDplot(dataOrd, k = 5, l = 4, u = 5)
#'
#' # Difficulty/Discrimination RIR plot of dataMedicalgraded data set
#' DDplot(dataOrd, discrim = "RIT")
#'
#' # Difficulty/Discrimination RIR plot of binary dataMedicalgraded data set
#' # using bin=TRUE and cutscore equal to 3.
#' DDplot(dataOrd, discrim = "ULI", bin=TRUE, cutscore=3)
#'
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
#' @importFrom grDevices dev.off png rainbow recordPlot
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


DDplot <- function (data, item.names, k = 3, l = 1, u = 3,  discrim = "ULI", bin = FALSE,
                    maxscore, minscore, cutscore)
{
  if(!is.matrix(data) & !is.data.frame(data)) {
    stop("'data' must be data frame or matrix ",
         call. = FALSE)
  }
  if(missing(maxscore)) {
    maxscore <- apply(data,2,max,na.rm=T)
  }
  if(missing(minscore)) {
    minscore <- apply(data,2,min,na.rm=T)
  }


  if(missing(cutscore)) {
    cutscore <- apply(data,2,max,na.rm=T)
  } else {
    if(length(cutscore) == 1){
      cutscore <- rep(cutscore, ncol(data))
    }
  }

  data <- data[complete.cases(data),]

  if(bin == TRUE) {
    data2 <- data
    for(i in 1:dim(data)[2]){
      data[data2[,i] >= cutscore[i], i] <- 1
      data[data2[,i] < cutscore[i], i] <- 0

    }
    head(data)
    minscore <- apply(data,2,min,na.rm=T)
    maxscore <- apply(data,2,max,na.rm=T)
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
  diffName <- c("Difficulty", "Average Scaled Score")
  discName <- c("Discrimination ULI", "Discrimination RIR", "Discrimination RIT")
  xlabel <- c("Item (ordered by difficulty)", "Item (ordered by scaled score)")
  average <- apply(data,2,mean)


  if(discrim == "ULI") {
    disc <- as.numeric(gDiscrim(data, minscore = minscore, maxscore=maxscore, k=k, l=l,u=u))
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
    disc<- t(cor(data, TOT, use = "complete"))
    i <-3
  }
  if(!all((maxscore-minscore) != 0)){
    warning("'cutscore' is equal to 'minscore' for some item")

    difc <- (average - minscore)/(maxscore-minscore)
    difc[(maxscore-minscore) == 0] <- 1
    disc[(maxscore-minscore) == 0] <- 0
  } else {
    difc <- (average - minscore)/(maxscore-minscore)
  }
  if(max(maxscore-minscore) > 1){
    j <- 2
  } else {
    j <- 1
  }


  if(discrim !=  "none") {
    if(any(disc < 0)) {
      warning("Estimated discrimination is lower than 0.",
              call. = F)
    }
    value <- c(rbind(difc, disc)[, order(difc)])
    parameter <- rep(c(diffName[j],discName[i]), ncol(data))
    item <- factor(rep(item.names[order(difc)], each = 2), levels = item.names[order(difc)])
    df <- data.frame(item, parameter, value)
    col <- c("red", "darkblue")
    ggplot(df, aes_string(x = "item", y = "value", fill = "parameter",
                          color = "parameter")) + stat_summary(fun.y = mean, position = "dodge",
                                                               geom = "bar", alpha = 0.7, width = 0.8) + geom_hline(yintercept = 0.2) +
      xlab(xlabel[j]) + ylab( paste(diffName[j],"/",discName[i], sep="")) +
      scale_y_continuous(expand = c(0, 0), limits = c(min(min(df$value) -
                                                            0.01, 0), 1)) + scale_fill_manual(breaks = parameter,
                                                                                              values = col) + scale_colour_manual(breaks = parameter,
                                                                                                                                  values = col) + theme_app() + theme(axis.text.x = element_text(angle = 90,
                                                                                                                                                                                                 vjust = 0.5), legend.position = c(0.01, 0.98), legend.justification = c(0,
                                                                                                                                                                                                                                                                         1))
  } else {
    value <- difc[order(difc)]
    parameter <- rep(c(diffName[j]), ncol(data))
    item <- factor(item.names[order(difc)], levels = item.names[order(difc)])
    df <- data.frame(item, parameter, value)
    col <- c("red", "darkblue")
    ggplot(df, aes_string(x = "item", y = "value", fill = "parameter",
                          color = "parameter")) + stat_summary(fun.y = mean, position = "dodge",
                                                               geom = "bar", alpha = 0.7, width = 0.8) +
      xlab(xlabel[j]) + ylab(diffName[j]) +
      scale_y_continuous(expand = c(0, 0), limits = c(min(min(df$value) -
                                                            0.01, 0), 1)) + scale_fill_manual(breaks = parameter,
                                                                                              values = col) + scale_colour_manual(breaks = parameter,
                                                                                                                                  values = col) + theme_app() + theme(axis.text.x = element_text(angle = 90,
                                                                                                                                                                                                 vjust = 0.5), legend.position = c(0.01, 0.98), legend.justification = c(0,1))
  }
}

