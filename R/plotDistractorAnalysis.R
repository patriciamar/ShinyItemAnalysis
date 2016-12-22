# data The unscored item response from a multiple-choice test
# key The answer key for the items
# num.groups The number of groups for distractor analysis
# item item indicator
# multiple.answers AB BC etc combinations




#' Function for graphical representation of item distractor analysis
#'
#' @aliases plotDistractorAnalysis
#'
#' @description Plots graphical representation of item distractor analysis with proportions and
#' optional number of groups.
#'
#' @param data character: data matrix or data frame. See \strong{Details}.
#' @param key character: answer key for the items.
#' @param num.groups numeric: number of groups to that should be respondents splitted.
#' @param item numeric: the number of item to be plotted.
#' @param multiple.answers logical: should be all combinations plotted (default) or should be
#' answers splitted into distractors. See \strong{Details}.
#'
#' @usage plotDistractorAnalysis(data, key, num.groups = 3, item = 1, multiple.answers = TRUE)
#'
#' @details
#' This function is graphical representation of \code{DistractorAnalysis} function.
#' The scores are calculatede using the item data and key. The respondents are then splitted into
#' the \code{num.groups}-quantiles and the proportion of respondents in each quantile is
#' reported with respect to their answers, using all reported combinations (default) or distractors.
#' These proportions are plotted.
#'
#' The \code{data} is a matrix or data frame whose rows represents unscored item response from a
#' multiple-choice test and columns correspond to the items.
#'
#' The \code{key} must be a vector of the same length as \code{ncol(data)}.
#'
#' If \code{multiple.answers = TRUE} (default) all reported combinations of answers are plotted.
#' If \code{multiple.answers = FALSE} all combinations are splitted into distractors and only these
#' are then plotted with correct combination.
#'
#' @author
#' Adela Drabinova \cr
#' Institute of Computer Science, The Czech Academy of Sciences \cr
#' Faculty of Mathematics and Physics, Charles University \cr
#' adela.drabinova@gmail.com \cr
#'
#' Patricia Martinkova \cr
#' Institute of Computer Science, The Czech Academy of Sciences \cr
#' martinkova@cs.cas.cz \cr
#'
#' @examples
#' \dontrun{
#' # loading data
#' data(dataMedical, dataMedicaltest, dataMedicalkey)
#'
#' # Difficulty/Discriminaton plot for medical admission test
#' DDplot(dataMedical)
#' # item 48 is very hard, thus does not discriminate well
#' # item 57 discriminates well
#' # item 32 does not discriminate well
#'
#' plotDistractorAnalysis(dataMedicaltest, dataMedicalkey, item = 48, multiple.answers = F)
#' # correct answer B does not function well
#' plotDistractorAnalysis(dataMedicaltest, dataMedicalkey, item = 57, multiple.answers = F)
#' # all options function well, thus the whole item discriminates well
#' plotDistractorAnalysis(dataMedicaltest, dataMedicalkey, item = 32, multiple.answers = F)
#' # functions well, thus the whole item discriminates well
#'
#' # distractor analysis plot for item 48, 57 and 32, all combinations
#' plotDistractorAnalysis(dataMedicaltest, dataMedicalkey, item = 48)
#' plotDistractorAnalysis(dataMedicaltest, dataMedicalkey, item = 57)
#' plotDistractorAnalysis(dataMedicaltest, dataMedicalkey, item = 32)
#'
#' # distractor analysis plot for item 57, all combinations and 6 groups
#' plotDistractorAnalysis(dataMedicaltest, dataMedicalkey, num.group = 6, item = 57)
#' }
#'
#'
#' @export


plotDistractorAnalysis <-  function (data, key, num.groups = 3, item = 1, multiple.answers = TRUE)
{
  key <- unlist(key)
  # distractor analysis
  tabDA <- DistractorAnalysis(data = data, key = key, p.table = TRUE, num.groups = num.groups)
  x <- tabDA[[item]]
  # only rows where is possitive proportion of correct answers
  if (dim(x)[2] != 1){
    x <- x[!(apply(x, 1, function(y) all(y == 0))), ]
  }

  x <- melt(x, id = "response")
  x$response <- as.factor(x$response)
  levels(x$response)[which(levels(x$response) == "")] <- "NaN"
  x$response <- relevel(x$response, as.character(key[item]))

  if (multiple.answers){
    # all combinations
    df <- x

    CA <- CAall <- as.character(key[item])
    col <- rainbow(n = length(levels(df$response)))
    names(col) <- levels(df$response)
  } else {
    # only distractors and correct combination
    # split combinations to possible choices (i.e. AB to A and B)
    levels(x$response)[which(levels(x$response) == "NaN")] <- "x"
    y <- x[rep(1:nrow(x), nchar(as.character(x$response))), ]
    y$response <- as.factor(unlist(strsplit(as.character(x$response), "")))
    # sum over choices
    df <- aggregate(value ~ response + score.level, data = y, sum)
    # adding correct combination
    CAdf <- x[x$response == as.character(key[item]), ]
    CAdf$response <- paste(key[item], "-correct", sep = "")
    df <- rbind(df, CAdf)
    CA <-  unique(CAdf$response)

    levels(df$response)[which(levels(df$response) == "x")] <- "NaN"

    # plot settings
    col <- rainbow(n = (length(levels(df$response)) + 1))
    names(col) <- levels(df$response)
    col[CA] <- "black"

    df$response <- relevel(df$response, CA)
    CAall <- c(CA, unlist(strsplit(as.character(key[item]), "")))
  }

  # plot settings
  linetype <- rep(2, length(levels(df$response)))
  shape <- rep(1, length(levels(df$response)))
  names(linetype) <- names(shape) <- levels(df$response)
  linetype[CAall] <- 1
  shape[CAall] <- 19

  # plot
  ggplot(df, aes_string(x = "score.level",
                        y = "value",
                        group = "response",
                        colour = "response",
                        linetype = "response",
                        shape = "response"),
         size = 1) +
    geom_line() +
    geom_point(size = 3) +
    xlab("Group by Total Score") +
    ylab("Option Selection Percentage") +
    scale_y_continuous(limits = c(0, 1)) +
    scale_x_discrete(labels = 1:num.groups, expand = c(0, 0.2)) +
    scale_linetype_manual(values = linetype) +
    scale_shape_manual(values = shape) +
    scale_color_manual(values = col) +
    theme_bw() +
    theme(axis.line  = element_line(colour = "black"),
          text = element_text(size = 14),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          legend.title = element_blank(),
          legend.position = c(0, 1),
          legend.justification = c(0, 1),
          legend.background = element_blank(),
          legend.key = element_rect(colour = "white"),
          legend.key.width = unit(1, "cm"),
          plot.title = element_text(face = "bold")) +
    ggtitle(paste("Item", item))

}
