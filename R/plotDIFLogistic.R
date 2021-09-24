#' Function for characteristic curve of 2PL logistic DIF model
#'
#' @aliases plotDIFLogistic
#'
#' @description Plots characteristic curve of 2PL logistic DIF model
#'
#' @param x an object of \code{"Logistic"} class. See \strong{Details}.
#' @param item numeric: number of item to be plotted
#' @param item.name character: the name of item to be used as title of plot.
#' @param group.names character: names of reference and focal group.
#' @param Data numeric: the data matrix. See \strong{Details}.
#' @param group numeric: the vector of group membership. See \strong{Details}.
#' @param match character or numeric: specifies observed score used for
#'   matching. Can be either \code{"score"}, or numeric vector of the same
#'   length as number of observations in \code{Data}. See \strong{Details}.
#' @param draw.empirical logical: whether empirical probabilities should be
#'   calculated and plotted. Default value is \code{TRUE}.
#'
#' @usage plotDIFLogistic(x, item = 1, item.name, group.names = c("Reference",
#'   "Focal"), Data, group, match, draw.empirical = TRUE)
#'
#' @details This function plots characteristic curves of 2PL logistic DIF model
#' fitted by \code{difLogistic()} function from difR package using ggplot2.
#'
#' \code{Data} and \code{group} are used to calculate empirical probabilities
#' for reference and focal group. \code{match} should be the same as in
#' \code{x$match}. In case that an observed score is used as a matching variable
#' instead of the total score or the standardized score, \code{match} needs to
#' be a numeric vector of the same the same length as the number of observations
#' in \code{Data}.
#'
#' @author
#' Adela Hladka \cr
#' Institute of Computer Science of the Czech Academy of Sciences \cr
#' \email{hladka@@cs.cas.cz}
#'
#' Patricia Martinkova \cr
#' Institute of Computer Science of the Czech Academy of Sciences \cr
#' \email{martinkova@@cs.cas.cz} \cr
#'
#' @examples
#' # loading libraries
#' library(difR)
#'
#' # loading data based on GMAT
#' data(GMAT, package = "difNLR")
#' Data <- GMAT[, 1:20]
#' group <- GMAT[, 21]
#'
#' # DIF detection using difLogistic() function
#' x <- difLogistic(Data, group, focal.name = 1)
#' # Characteristic curve by logistic regression model
#' plotDIFLogistic(x, item = 1, Data = Data, group = group)
#'
#' # Using name of column as item identifier
#' plotDIFLogistic(x, item = "Item1", Data = Data, group = group)
#'
#' # Renaming reference and focal group
#' plotDIFLogistic(x, item = 1, group.names = c("Group 1", "Group 2"), Data = Data, group = group)
#'
#' # Not plotting empirical probabilities
#' plotDIFLogistic(x, item = 1, draw.empirical = FALSE)
#' @seealso \code{\link[difR]{difLogistic}}, \code{\link[ggplot2]{ggplot}}
#'
#' @importFrom ggplot2 stat_function scale_colour_manual scale_linetype_manual
#'   guides guide_legend ggtitle
#'
#' @export
plotDIFLogistic <- function(x, item = 1, item.name, group.names = c("Reference", "Focal"),
                            Data, group, match, draw.empirical = TRUE) {
  res <- x
  i <- ifelse(is.character(item) | is.factor(item),
    (1:length(res$names))[res$names == item],
    item
  )
  if (missing(item.name)) {
    if (is.character(item) | is.factor(item)) {
      item.name <- paste(item)
    } else {
      item.name <- paste("Item", item)
    }
  }

  if (any(is.na(res$logitPar[i, ]))) {
    stop("Selected item is an anchor item!",
      call. = FALSE
    )
  }
  coef <- res$logitPar[i, ]

  if (missing(Data) & draw.empirical) {
    stop("'Data' needs to be specified! ", .call = FALSE)
  }
  if (missing(group) & draw.empirical) {
    stop("'group' needs to be specified! ", .call = FALSE)
  }

  if (missing(match)) {
    match <- res$match
  }

  if (res$purification & res$DIFitems[1] != "No DIF item detected") {
    ANCHOR <- c(1:nrow(res$logitPar))[-res$DIFitems]
  } else {
    ANCHOR <- c(1:nrow(res$logitPar))
  }

  if (match[1] == "score") {
    xlab <- "Total score"
    if (draw.empirical) {
      MATCHCRIT <- rowSums(Data[, ANCHOR])
    } else {
      MATCHCRIT <- c(0, nrow(res$logitPar))
    }
  } else if (match[1] == "zscore") {
    xlab <- "Standardized total score"
    if (draw.empirical) {
      MATCHCRIT <- scale(apply(as.data.frame(Data[, ANCHOR]), 1, sum))
    } else {
      MATCHCRIT <- c(0, nrow(res$logitPar))
    }
  } else if (length(match) != nrow(Data)) {
    stop("'match' needs to be either 'score', 'zscore' or numeric vector of the same length as number of observations in 'Data'. ", .call = FALSE)
  } else {
    MATCHCRIT <- match
    xlab <- "Observed score"
  }

  LR_plot <- function(x, group, b0, b1, b2, b3) {
    return(1 / (1 + exp(-(b0 + b1 * x + b2 * group + b3 * x * group))))
  }

  if (draw.empirical) {
    score_R <- MATCHCRIT[group == 0]
    score_F <- MATCHCRIT[group == 1]

    empirical_R <- data.frame(
      score = as.numeric(levels(as.factor(score_R))),
      probability = tapply(Data[group == 0, i], as.factor(score_R), mean)
    )
    empirical_F <- data.frame(
      score = as.numeric(levels(as.factor(score_F))),
      probability = tapply(Data[group == 1, i], as.factor(score_F), mean)
    )
    empirical <- data.frame(rbind(
      cbind(empirical_R, Group = "gr1"),
      cbind(empirical_F, Group = "gr2")
    ))
    empirical$size <- c(table(score_R), table(score_F))
    colnames(empirical) <- c("Score", "Probability", "Group", "Count")
  }

  max_score <- max(MATCHCRIT, na.rm = TRUE) + 0.1
  min_score <- min(MATCHCRIT, na.rm = TRUE) - 0.1

  col <- c("dodgerblue2", "goldenrod2")
  alpha <- .5
  shape <- 21
  size <- .8
  linetype <- c("solid", "dashed")

  g <- ggplot() +
    ### lines
    xlim(min_score, max_score) +
    stat_function(aes(colour = "gr1", linetype = "gr1"),
      fun = LR_plot,
      args = list(
        group = 0,
        b0 = coef[1],
        b1 = coef[2],
        b2 = coef[3],
        b3 = coef[4]
      ),
      size = size, geom = "line"
    ) +
    stat_function(aes(colour = "gr2", linetype = "gr2"),
      fun = LR_plot,
      args = list(
        group = 1,
        b0 = coef[1],
        b1 = coef[2],
        b2 = coef[3],
        b3 = coef[4]
      ),
      size = size, geom = "line"
    ) +
    ### style
    scale_colour_manual(
      values = col,
      breaks = c("gr1", "gr2"),
      labels = group.names
    ) +
    scale_linetype_manual(
      values = linetype,
      breaks = c("gr1", "gr2"),
      labels = group.names
    ) +
    guides(colour = guide_legend(title = "Group", order = 2)) +
    guides(linetype = guide_legend(title = "Group", order = 2)) +
    ### theme
    xlab(xlab) +
    ylab("Probability of correct answer") +
    scale_y_continuous(limits = c(0, 1)) +
    theme_app() +
    theme(
      legend.box.just = "top",
      legend.position = c(0.01, 0.98),
      legend.justification = c(0, 1),
      legend.key.width = unit(1, "cm"),
      legend.box = "horizontal"
    ) +
    ggtitle(item.name)

  if (draw.empirical) {
    g <- g +
      ### points
      geom_point(
        data = empirical,
        aes_string(x = "Score", y = "Probability", colour = "Group", fill = "Group", size = "Count"),
        alpha = alpha, shape = shape
      ) +
      guides(size = guide_legend(title = "Count", order = 1)) +
      scale_fill_manual(
        values = col,
        breaks = c("gr1", "gr2"),
        labels = group.names
      ) +
      guides(fill = guide_legend(title = "Group", order = 2))
  }

  return(g)
}
