#' Graphical representation of difficulty and (generalized) discrimination in item analysis
#'
#' @aliases DDplot
#'
#' @description Plots difficulty and (generalized) discrimination for items ordered by difficulty.
#'
#' @param data numeric: binary data matrix or data frame. See \strong{Details}.
#' @param item.names character: the names of items.
#' @param k numeric: number of groups to which may be data.frame x divided by the total score.
#' Default value is 3.  See \strong{Details}.
#' @param l numeric: lower group. Default value is 1. See \strong{Details}.
#' @param u numeric: upper group. Default value is 3. See \strong{Details}.
#'
#' @usage DDplot(data, item.names, k = 3, l = 1, u = 3)
#'
#' @details
#' The \code{data} is a matrix or data frame whose rows represents examinee answers
#' ("1" correct, "0" incorrect) and columns correspond to the items. The \code{item.names}
#' argument stands for names of items. If not specified, the names of dataset columns are used.
#' Generalized discrimination is computed as follows: The function takes data on individuals,
#' computes their total test score and then divides individuals into \code{k} groups. The lower and
#' upper group are determined by \code{l} and \code{u} parameters, i.e.  l-th and u-th group where
#' the ordering is defined by increasing total score.
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
#' Generalized discrimination is calculated by \code{\link{gDiscrim}} function, generalized version
#' of \code{\link{discrim}} function in \code{psychometric} package.
#'
#' @seealso
#' \code{\link{gDiscrim}}, \code{\link{discrim}}

#'
#' @examples
#' \dontrun{
#' # loading 100-item medical admission test data set
#' data(dataMedical)
#' data <- dataMedical[, 1:100]
#'
#' # Difficulty/Discrimination plot of dataMedical data set
#' DDplot(data)
#'
#' # Difficulty/Discrimination plot of dataMedical data set
#' # discrimination based on 5 groups, comparing 4th and 5th
#' DDplot(data, k = 5, l = 4, u = 5)
#' }
#' @export
#' @import difNLR
#' difR
#' shiny
#' grid
#' WrightMap
#' @importFrom corrplot corrplot
#' @importFrom CTT score
#' @importFrom deltaPlotR deltaPlot
#' @importFrom ggplot2 aes aes_string element_blank element_line element_rect element_text geom_abline
#' ggplot_build position_dodge geom_histogram geom_hline geom_line geom_point geom_ribbon geom_text ggplot
#' ggsave ggtitle labs scale_color_manual scale_colour_manual scale_fill_manual scale_linetype_manual
#' scale_shape_manual scale_size_continuous scale_x_continuous scale_x_discrete scale_y_continuous
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
#' @importFrom stats aggregate coef complete.cases deriv deriv3 fitted glm median na.exclude nls quantile relevel sd vcov xtabs
#' @importFrom stringr str_sub
#' @importFrom utils data read.csv


DDplot <- function(data, item.names, k = 3, l = 1, u = 3){
  if (is.matrix(data) | is.data.frame(data)) {
    if (!all(apply(data, 2, function(i) {
      length(levels(as.factor(i))) == 2
    })))
      stop("'data' must be data frame or matrix of binary vectors",
           call. = FALSE)
  }
  else {
    stop("'data' must be data frame or matrix of binary vectors",
         call. = FALSE)
  }

  if(missing(item.names)){
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
  # difficulty and discrimination
  difc <- psychometric::item.exam(data, discr = T)[, "Difficulty"]
  disc <- ShinyItemAnalysis::gDiscrim(data, k = k, l = l, u = u)

  value <- c(rbind(difc, disc)[, order(difc)])
  parameter <- rep(c("Difficulty", "Discrimination"), ncol(data))
  # ordered by difficulty
  item <- factor(rep(item.names[order(difc)], each = 2),
                    levels = item.names[order(difc)])
  df <- data.frame(item, parameter, value)

  # plot
  col <- c("red", "darkblue")
  ggplot(df,
         aes_string(x = "item", y = "value", fill = "parameter", color = "parameter")) +
    stat_summary(fun.y = mean,
                 position = "dodge",
                 geom = "bar",
                 alpha = 0.7,
                 width = 0.8) +
    geom_hline(yintercept = 0.2) +
    xlab("Item (ordered by difficulty)") +
    ylab("Difficulty/Discrimination") +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 1)) +
    scale_fill_manual(breaks = parameter,
                      values = col) +
    scale_colour_manual(breaks = parameter,
                        values = col) +
    theme_bw() +
    theme(axis.line  = element_line(colour = "black"),
          text = element_text(size = 14),
          axis.text.x = element_text(angle = 90, vjust = 0.5),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          legend.title = element_blank(),
          legend.position = c(0, 1),
          legend.justification = c(0, 1),
          legend.background = element_blank(),
          legend.key = element_rect(colour = "white"))
}

