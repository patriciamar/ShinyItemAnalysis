#' Plot item distractor analysis
#'
#' @aliases plotDistractorAnalysis
#'
#' @description Plots graphical representation of item distractor
#'   analysis with proportions and optional number of groups.
#'
#' @param Data character: data matrix or data.frame with rows
#'   representing unscored item response from a multiple-choice test
#'   and columns corresponding to the items.
#' @param key character: answer key for the items. The `key` must be a
#'   vector of the same length as `ncol(Data)`. In case it is not
#'   provided, `criterion` needs to be specified.
#' @param num.groups numeric: number of groups to which are the
#'   respondents splitted.
#' @param item numeric: the number of the item to be plotted.
#' @param item.name character: the name of the item.
#' @param multiple.answers logical: should be all combinations plotted
#'   (default) or should be answers splitted into distractors. See
#'   **Details**.
#' @param criterion numeric: numeric vector. If not provided, total
#'   score is calculated and distractor analysis is performed based on
#'   it.
#' @param crit.discrete logical: is `criterion` discrete? Default
#'   value is `FALSE`.
#' @param cut.points numeric: numeric vector specifying cut points of
#'   `criterion`.
#' @param data deprecated. Use argument `Data` instead.
#' @param matching deprecated. Use argument `criterion` instead.
#' @param match.discrete deprecated. Use argument `crit.discrete`
#'   instead.
#'
#' @details This function is a graphical representation of the
#'   [DistractorAnalysis()] function. In case that no `criterion` is
#'   provided, the scores are calculated using the item `Data` and
#'   `key`. The respondents are by default split into the
#'   `num.groups`-quantiles and the proportions of respondents in each
#'   quantile are displayed with respect to their answers. In case
#'   that `criterion` is discrete (`crit.discrete = TRUE`),
#'   `criterion` is split based on its unique levels. Other cut points
#'   can be specified via `cut.points` argument.
#'
#'   If `multiple.answers = TRUE` (default) all reported combinations
#'   of answers are plotted. If `multiple.answers = FALSE` all
#'   combinations are split into distractors and only these are then
#'   plotted with correct combination.
#'
#' @author
#' Adela Hladka \cr
#' Institute of Computer Science of the Czech Academy of Sciences \cr
#' \email{hladka@@cs.cas.cz}
#'
#' Patricia Martinkova \cr
#' Institute of Computer Science of the Czech Academy of Sciences \cr
#' \email{martinkova@@cs.cas.cz}
#'
#' @seealso [DistractorAnalysis()],
#'   [CTT::distractor.analysis()]
#'
#' @examples
#' # loading 100-item medical admission test datasets
#' data(dataMedical, dataMedicaltest, dataMedicalkey)
#' Data <- dataMedicaltest[, 1:100]
#' DataBin <- dataMedical[, 1:100]
#' key <- dataMedicalkey
#'
#' # distractor plot for items 48, 57 and 32 displaying distractors only
#' # correct answer B does not function well:
#' plotDistractorAnalysis(Data, key, item = 48, multiple.answers = FALSE)
#'
#' # all options function well, thus the whole item discriminates well:
#' plotDistractorAnalysis(Data, key, item = 57, multiple.answers = FALSE)
#'
#' # functions well, thus the whole item discriminates well:
#' plotDistractorAnalysis(Data, key, item = 32, multiple.answers = FALSE)
#'
#' \dontrun{
#' # distractor plot for items 48, 57 and 32 displaying all combinations
#' plotDistractorAnalysis(Data, key, item = c(48, 57, 32))
#'
#' # distractor plot for item 57 with all combinations and 6 groups
#' plotDistractorAnalysis(Data, key, item = 57, num.group = 6)
#'
#' # distractor plot for item 57 using specified criterion and key option
#' criterion <- round(rowSums(DataBin), -1)
#' plotDistractorAnalysis(Data, key, item = 57, criterion = criterion)
#' # distractor plot for item 57 using specified criterion without key option
#' plotDistractorAnalysis(Data, item = 57, criterion = criterion)
#'
#' # distractor plot for item 57 using discrete criterion
#' plotDistractorAnalysis(Data, key,
#'   item = 57, criterion = criterion,
#'   crit.discrete = TRUE
#' )
#'
#' # distractor plot for item 57 using groups specified by cut.points
#' plotDistractorAnalysis(Data, key, item = 57, cut.points = seq(10, 96, 10))
#' }
#'
#' @importFrom grDevices rainbow
#' @importFrom ggplot2 scale_linetype_manual scale_shape_manual ggtitle guides
#'   guide_legend margin
#'
#' @export
plotDistractorAnalysis <- function(Data, key, num.groups = 3, item = 1, item.name, multiple.answers = TRUE,
                                   criterion = NULL, crit.discrete = FALSE, cut.points, data, matching, match.discrete) {
  # deprecated args handling
  if (!missing(data)) {
    warning("Argument 'data' is deprecated; please use 'Data' instead.",
      call. = FALSE
    )
    Data <- data
  }
  if (!missing(matching)) {
    warning("Argument 'matching' is deprecated; please use 'criterion' instead.",
      call. = FALSE
    )
    criterion <- matching
  }
  if (!missing(match.discrete)) {
    warning("Argument 'match.discrete' is deprecated; please use 'crit.discrete' instead.",
      call. = FALSE
    )
    crit.discrete <- match.discrete
  }

  if (missing(key)) {
    if (all(sapply(Data, is.numeric))) {
      warning("Answer key is not provided. Maximum value is used as 'key'.", call. = FALSE)
      key <- sapply(Data, max, na.rm = TRUE)
    } else if (missing(criterion)) {
      stop("Answer key is not provided. Please, specify 'key' to be able to calculate total score or provide 'criterion'. ",
        call. = FALSE
      )
    } else {
      key <- NULL
    }
  } else {
    key <- unlist(key)
    if (!length(key) == ncol(Data)) {
      stop("Answer key is not provided or some item keys are missing.", call. = FALSE)
    }
  }

  Data <- as.data.frame(Data)
  m <- ncol(Data)
  nams <- colnames(Data)

  if (class(item) == "character") {
    if (any(item != "all") & !all(item %in% nams)) {
      stop("Invalid value for 'item'. Item must be either character 'all', or
           numeric vector corresponding to column identifiers, or name of the item.",
           call. = FALSE
      )
    }
    if (any(item == "all")) {
      items <- 1:m
    } else {
      items <- which(nams %in% item)
    }
  } else {
    if (class(item) != "integer" & class(item) != "numeric") {
      stop("Invalid value for 'item'. Item must be either character 'all', or
           numeric vector corresponding to column identifiers, or name of the item.",
           call. = FALSE
      )
    } else {
      if (!all(item %in% 1:m)) {
        stop("Invalid number for 'item'.",
             call. = FALSE
        )
      } else {
        items <- item
      }
    }
  }

  plots <- list()
  for (i in items) {
    # distractor analysis
    x <- DistractorAnalysis(
      Data = Data, key = key, item = i, p.table = TRUE, num.groups = num.groups, criterion = criterion,
      crit.discrete = crit.discrete#, cut.points = cut.points
    )[[1]]

    # only rows where is positive proportion of correct answers
    if (dim(x)[2] != 1) {
      x <- x[!(apply(x, 1, function(y) all(y == 0))), ]
    }

    x <- as.data.frame(x) # table coerces nicely into data.frame, no need for reshape
    colnames(x)[colnames(x) == "Freq"] <- "value"

    x <- x[complete.cases(x), ]
    x$response <- as.factor(x$response)
    levels(x$response)[which(levels(x$response) == "")] <- "NaN"
    if (!is.null(key)) {
      x$response <- relevel(x$response, as.character(key[i]))
    }

    if (multiple.answers) {
      # all combinations
      df <- x
      CA <- CAall <- as.character(key[i])
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
      CAdf <- x[x$response == as.character(key[i]), ]
      CAdf$response <- paste(key[i], "-correct", sep = "")
      df <- rbind(df, CAdf)
      CA <- unique(CAdf$response)

      levels(df$response)[which(levels(df$response) == "x")] <- "NaN"

      df$response <- relevel(df$response, CA)
      CAall <- c(CA, unlist(strsplit(as.character(key[i]), "")))

      # plot settings
      col <- rainbow(n = length(levels(df$response)))
      names(col) <- levels(df$response)
      col[CA] <- "black"
    }

    # if (missing(item.name)) {
      item.name <- nams[i]
    # }

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

    xlab <- ifelse(is.null(criterion), "Group by total score", "Group by criterion variable")

    df$score.level <- as.factor(df$score.level)
    num.groups <- length(unique(df$score.level))
    colnames(df) <- c("Response", "Group", "Proportion")
    levels(df$Group) <- 1:length(levels(df$Group))

    # plot
    g <- ggplot(df, aes(
      x = .data$Group,
      y = .data$Proportion,
      group = .data$Response,
      colour = .data$Response,
      linetype = .data$Response,
      shape = .data$Response
    )) +
      geom_line(size = 0.8) +
      geom_point(size = 3) +
      xlab(xlab) +
      ylab("Option selection proportion") +
      scale_y_continuous(limits = c(0, 1)) +
      scale_x_discrete(labels = 1:num.groups, expand = c(0, 0.15)) +
      scale_linetype_manual(values = linetype) +
      scale_shape_manual(values = shape) +
      scale_color_manual(values = col) +
      theme_app() +
      theme(
        legend.spacing.y = unit(0, "pt"),
        legend.position = c(.02, .98),
        legend.margin = margin(),
        legend.justification = c(0, 1),
        legend.key.width = unit(1, "cm"),
        legend.background = element_rect("#FFFFFFCC"), # alpha .8 opacity
        legend.key = element_rect("#FFFFFF00") # transparent
      ) +
      ggtitle(item.name)

    if (length(col) > 11) {
      g <- g + guides(
        linetype = guide_legend(ncol = 2),
        shape = guide_legend(ncol = 2),
        color = guide_legend(ncol = 2)
      )
    }
    plots[[i]] <- g
  }
  plots <- plots[items]
  names(plots) <- nams[items]
  return(plots)
}
