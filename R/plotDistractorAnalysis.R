#' Plot item distractor analysis
#'
#' @aliases plotDistractorAnalysis
#'
#' @description Plots graphical representation of item distractor analysis with
#'   proportions and optional number of groups.
#'
#' @param data character: data matrix or data frame. See \strong{Details}.
#' @param key character: answer key for the items.
#' @param num.groups numeric: number of groups to that should be respondents
#'   splitted.
#' @param item numeric: the number of item to be plotted.
#' @param item.name character: the name of item.
#' @param multiple.answers logical: should be all combinations plotted (default)
#'   or should be answers splitted into distractors. See \strong{Details}.
#' @param matching numeric: numeric vector. If not provided, total score is
#'   calculated and distractor analysis is performed based on it.
#' @param match.discrete logical: is \code{matching} discrete? Default value is
#'   \code{FALSE}. See details.
#' @param cut.points numeric: numeric vector specifying cut points of
#'   \code{matching}. See details.
#'
#' @details This function is graphical representation of
#' \code{\link{DistractorAnalysis}} function. In case, no \code{matching} is
#' provided, the scores are calculated using the item data and key. The
#' respondents are by default splitted into the \code{num.groups}-quantiles and
#' the proportions of respondents in each quantile are displayed with respect to
#' their answers. In case that \code{matching} is discrete (\code{match.discrete
#' = TRUE}), \code{matching} is splitted based on its unique levels. Other cut
#' points can be specified via \code{cut.points} argument.
#'
#' The \code{data} is a matrix or data frame whose rows represents unscored item
#' response from a multiple-choice test and columns correspond to the items.
#'
#' The \code{key} must be a vector of the same length as \code{ncol(data)}. In
#' case it is not provided, \code{matching} need to be specified.
#'
#' If \code{multiple.answers = TRUE} (default) all reported combinations of
#' answers are plotted. If \code{multiple.answers = FALSE} all combinations are
#' splitted into distractors and only these are then plotted with correct
#' combination.
#'
#' @author
#' Adela Hladka \cr
#' Institute of Computer Science of the Czech Academy of Sciences \cr
#' Faculty of Mathematics and Physics, Charles University \cr
#' \email{hladka@@cs.cas.cz} \cr
#'
#' Patricia Martinkova \cr
#' Institute of Computer Science of the Czech Academy of Sciences \cr
#' \email{martinkova@@cs.cas.cz} \cr
#'
#' @seealso \code{\link{DistractorAnalysis}}, \code{\link[CTT]{distractor.analysis}}
#'
#' @examples
#'
#' # loading 100-item medical admission test data
#' data(dataMedical, dataMedicaltest, dataMedicalkey)
#' data <- dataMedicaltest[, 1:100]
#' dataBin <- dataMedical[, 1:100]
#' key <- unlist(dataMedicalkey)
#'
#' # distractor plot for items 48, 57 and 32 displaying distractors only
#' plotDistractorAnalysis(data, key, item = 48, multiple.answers = FALSE)
#' # correct answer B does not function well
#' plotDistractorAnalysis(data, key, item = 57, multiple.answers = FALSE)
#' # all options function well, thus the whole item discriminates well
#' plotDistractorAnalysis(data, key, item = 32, multiple.answers = FALSE)
#' # functions well, thus the whole item discriminates well
#' \dontrun{
#' # distractor plot for items 48, 57 and 32 displaying all combinations
#' plotDistractorAnalysis(data, key, item = 48)
#' plotDistractorAnalysis(data, key, item = 57)
#' plotDistractorAnalysis(data, key, item = 32)
#'
#' # distractor plot for item 57 with all combinations and 6 groups
#' plotDistractorAnalysis(data, key, item = 57, num.group = 6)
#'
#' # distractor plot for item 57 using specified matching and key option
#' matching <- round(rowSums(dataBin), -1)
#' plotDistractorAnalysis(data, key, item = 57, matching = matching)
#' # distractor plot for item 57 using specified matching without key option
#' plotDistractorAnalysis(data, item = 57, matching = matching)
#'
#' # distractor plot for item 57 using discrete matching
#' plotDistractorAnalysis(data, key,
#'   item = 57, matching = matching,
#'   match.discrete = TRUE
#' )
#'
#' # distractor plot for item 57 using groups specified by cut.points
#' plotDistractorAnalysis(data, key, item = 57, cut.points = seq(10, 100, 10))
#' }
#' @export

plotDistractorAnalysis <- function(data, key, num.groups = 3, item = 1, item.name, multiple.answers = TRUE,
                                   matching = NULL, match.discrete = FALSE, cut.points) {
  if (missing(key)) {
    if (all(sapply(data, is.numeric))) {
      warning("Answer key is not provided. Maximum value is used as key.", call. = FALSE)
      key <- sapply(data, max, na.rm = T)
    } else if (missing(matching)) {
      stop("Answer key is not provided. Please, specify key to be able to calculate total score or provide matching. ",
        call. = FALSE
      )
    } else {
      key <- NULL
    }
  } else {
    if (!length(key) == ncol(data)) {
      stop("Answer key is not provided or some item keys are missing.", call. = FALSE)
    }
    key <- unlist(key)
  }

  # distractor analysis
  tabDA <- DistractorAnalysis(
    data = data, key = key, p.table = TRUE, num.groups = num.groups, matching = matching,
    match.discrete = match.discrete, cut.points = cut.points
  )

  x <- tabDA[[item]]

  # only rows where is possitive proportion of correct answers
  if (dim(x)[2] != 1) {
    x <- x[!(apply(x, 1, function(y) all(y == 0))), ]
  }

  x <- as.data.frame(x) # table coerces nicely into data.frame, no neet for reshape
  colnames(x)[colnames(x) == "Freq"] <- "value"

  x <- x[complete.cases(x), ]
  x$response <- as.factor(x$response)
  levels(x$response)[which(levels(x$response) == "")] <- "NaN"
  if (!is.null(key)) {
    x$response <- relevel(x$response, as.character(key[item]))
  }

  if (multiple.answers) {
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
    CA <- unique(CAdf$response)

    levels(df$response)[which(levels(df$response) == "x")] <- "NaN"

    # plot settings
    col <- rainbow(n = (length(levels(df$response)) + 1))
    names(col) <- levels(df$response)
    col[CA] <- "black"

    df$response <- relevel(df$response, CA)
    CAall <- c(CA, unlist(strsplit(as.character(key[item]), "")))
  }

  if (missing(item.name)) {
    item.name <- paste("Item", item)
  }

  # plot settings
  if (is.null(key)) {
    linetype <- rep(1, length(levels(df$response)))
    shape <- rep(19, length(levels(df$response)))
    names(linetype) <- names(shape) <- levels(df$response)
  } else {
    linetype <- rep(2, length(levels(df$response)))
    shape <- rep(1, length(levels(df$response)))
    names(linetype) <- names(shape) <- levels(df$response)
    linetype[CAall] <- 1
    shape[CAall] <- 19
  }

  xlab <- ifelse(is.null(matching), "Group by total score", "Group by criterion variable")

  df$score.level <- as.factor(df$score.level)
  num.groups <- length(unique(df$score.level))

  # plot
  g <- ggplot(df, aes_string(
    x = "score.level",
    y = "value",
    group = "response",
    colour = "response",
    linetype = "response",
    shape = "response"
  ),
  size = 1
  ) +
    geom_line() +
    geom_point(size = 3) +
    xlab(xlab) +
    ylab("Option selection percentage") +
    scale_y_continuous(limits = c(0, 1)) +
    scale_x_discrete(labels = 1:num.groups, expand = c(0, 0.15)) +
    scale_linetype_manual(values = linetype) +
    scale_shape_manual(values = shape) +
    scale_color_manual(values = col) +
    theme_app() +
    theme(
      legend.position = c(0.01, 0.98),
      legend.justification = c(0, 1),
      legend.key.width = unit(1, "cm")
    ) +
    ggtitle(item.name)

  if (length(col) > 11) {
    g <- g + guides(
      linetype = guide_legend(ncol = 2),
      shape = guide_legend(ncol = 2),
      color = guide_legend(ncol = 2)
    )
  }
  return(g)
}
