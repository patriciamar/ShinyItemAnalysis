#' Plot item characteristic curve of DIF IRT model
#'
#' @aliases plotDIFirt
#'
#' @description Plots characteristic curve of IRT model.
#'
#' @param parameters numeric: data matrix or data frame. See \strong{Details}.
#' @param test character: type of statistic to be shown. See \strong{Details}.
#' @param item either character ("all"), or numeric vector, or single number
#'   corresponding to column indicators. See \strong{Details}.
#' @param item.name character: the name of item.
#' @param same.scale logical: are the item \code{parameters} on the same scale?
#'   (default is "FALSE"). See \strong{Details}.
#'
#' @details This function plots characteristic curve of DIF IRT model.
#'
#' The \code{parameters} matrix has a number of rows equal to twice the number
#' of items in the data set. The first J rows refer to the item parameter
#' estimates in the reference group, while the last J ones correspond to the
#' same items in the focal group. The number of columns depends on the selected
#' IRT model: 2 for the 1PL model, 5 for the 2PL model, 6 for the constrained
#' 3PL model and 9 for the unconstrained 3PL model. The columns of
#' \code{irtParam()} have to follow the same structure as the output of
#' \code{itemParEst()}, \code{difLord()} or \code{difRaju()} command from the
#' \code{difR} package.
#'
#' Two possible type of \code{test} statistics can be visualized - \code{"Lord"}
#' gives only characteristic curves, \code{"Raju"} also highlights area between
#' these curves.
#'
#' For default option \code{"all"}, all characteristic curves are plotted.
#'
#' @author
#' Adela Hladka \cr
#' Institute of Computer Science of the Czech Academy of Sciences \cr
#' \email{hladka@@cs.cas.cz} \cr
#'
#' Patricia Martinkova \cr Institute of Computer Science of the Czech Academy of
#' Sciences \cr \email{martinkova@@cs.cas.cz} \cr
#'
#' @seealso \code{\link[difR]{itemParEst}}, \code{\link[difR]{difLord}},
#'   \code{\link[difR]{difRaju}}
#'
#' @examples
#' # loading libraries
#' library(difR)
#' library(ltm)
#'
#' # loading data based on GMAT2
#' data(GMAT2, package = "difNLR")
#'
#' # Estimation of 2PL IRT model and Lord's statistic
#' # by difR package
#' fitLord <- difLord(GMAT2, group = 21, focal.name = 1, model = "2PL")
#' # plot of item 1 and Lord's statistic
#' plotDIFirt(fitLord$itemParInit, item = 1)
#'
#' # Estimation of 2PL IRT model and Raju's statistic
#' # by difR package
#' fitRaju <- difRaju(GMAT2, group = 21, focal.name = 1, model = "2PL")
#' # plot of item 1 and Lord's statistic
#' plotDIFirt(fitRaju$itemParInit, test = "Raju", item = 1)
#'
#' @importFrom difR itemRescale
#' @importFrom ggplot2 stat_function scale_colour_manual scale_linetype_manual
#'   ggtitle ggplot_build geom_ribbon
#'
#' @export
plotDIFirt <- function(parameters, test = "Lord", item = "all", item.name, same.scale = FALSE) {
  if (!(test %in% c("Lord", "Raju"))) {
    stop("'test' must be either 'Lord' or 'Raju'",
      call. = FALSE
    )
  }
  if (!(ncol(parameters) %in% c(2, 5, 6, 9))) {
    stop("Invalid dimension of 'parameters'",
      call. = FALSE
    )
  }
  if ((nrow(parameters) %% 2) != 0) {
    stop("Invalid dimension of 'parameters'",
      call. = FALSE
    )
  }

  m <- nrow(parameters) / 2

  if (class(item) == "character") {
    if (item != "all") {
      stop("'item' must be either numeric vector or character string 'all' ",
        call. = FALSE
      )
    }
  } else {
    if (class(item) != "integer" & class(item) != "numeric") {
      stop("'item' must be either numeric vector or character string 'all' ",
        call. = FALSE
      )
    }
  }
  if (class(item) == "numeric" & !all(item %in% 1:m)) {
    stop("invalid number of 'item'",
      call. = FALSE
    )
  }
  if (class(item) == "integer" & !all(item %in% 1:m)) {
    stop("'item' must be either numeric vector or character string 'all' ",
      call. = FALSE
    )
  }

  if (item == "all") {
    items <- 1:m
  } else {
    items <- item
  }

  if (missing(item.name)) {
    item.names <- paste("Item", 1:m)
  } else {
    item.names <- rep(NA, m)
    item.names[items] <- item.name
  }

  mR <- parameters[1:m, ]
  mF <- parameters[(m + 1):(2 * m), ]

  if (!same.scale) {
    mF <- itemRescale(mR, mF)
  }

  if (is.null(dim(mR))) {
    mR <- as.data.frame(t(mR))
    mF <- as.data.frame(t(mF))
  }

  CC_plot <- function(x, a, b, c) {
    return(c + (1 - c) / (1 + exp(-(a * (x - b)))))
  }

  coefR <- switch(as.character(ncol(mR)),
    "2" = data.frame(a = 1, mR[, 1], c = 0),
    "5" = data.frame(mR[, 1:2], c = 0),
    "6" = mR[, c(1, 2, 6)],
    "9" = mR[, 1:3]
  )
  coefF <- switch(as.character(ncol(mF)),
    "2" = data.frame(a = 1, mF[, 1], c = 0),
    "5" = data.frame(mF[, 1:2], c = 0),
    "6" = mF[, c(1, 2, 6)],
    "9" = mF[, 1:3]
  )

  col <- c("dodgerblue2", "goldenrod2")
  alpha <- .5
  shape <- 21
  size <- .8
  linetype <- c(2, 1)

  df <- data.frame(x = c(-3, 3), y = c(0, 1))
  gg <- list()
  for (i in items) {
    gg[[i]] <- ggplot(df, aes_string("x", "y")) +
      xlim(-3, 3) +
      ### lines
      stat_function(aes(colour = "Reference", linetype = "Reference"),
        fun = CC_plot,
        args = list(
          a = coefR[i, 1],
          b = coefR[i, 2],
          c = coefR[i, 3]
        ),
        size = size, geom = "line"
      ) +
      stat_function(aes(colour = "Focal", linetype = "Focal"),
        fun = CC_plot,
        args = list(
          a = coefF[i, 1],
          b = coefF[i, 2],
          c = coefF[i, 3]
        ),
        size = size, geom = "line"
      ) +
      ### style
      scale_colour_manual(
        name = "Group",
        breaks = c("Reference", "Focal"),
        values = col
      ) +
      scale_fill_manual(values = col) +
      scale_linetype_manual(
        name = "Group",
        breaks = c("Reference", "Focal"),
        values = linetype
      ) +
      ### theme
      xlab("Ability") +
      ylab("Probability of correct answer") +
      scale_y_continuous(limits = c(0, 1)) +
      theme_app() +

      ### legend
      theme(
        legend.box.just = "top",
        legend.position = c(0.01, 0.98),
        legend.justification = c(0, 1),
        legend.key.width = unit(1, "cm"),
        legend.box = "horizontal"
      ) +
      ggtitle(item.names[i])


    if (test == "Raju") {
      gg1 <- ggplot_build(gg[[i]])

      # extract data for the loess lines from the 'data' slot
      df2 <- data.frame(
        x = gg1$data[[1]]$x,
        ymin = gg1$data[[1]]$y,
        ymax = gg1$data[[2]]$y
      )

      # use the loess data to add the 'ribbon' to plot
      gg[[i]] <- gg[[i]] + geom_ribbon(
        data = df2,
        aes_string(
          x = "x",
          ymin = "ymin",
          ymax = "ymax"
        ),
        fill = "grey",
        alpha = 0.4,
        inherit.aes = FALSE
      )
    }
  }

  return(gg)
}
