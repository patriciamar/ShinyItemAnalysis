#' Plot Wright map using ggplot
#'
#' @aliases ggWrightMap
#'
#' @description This function allows to generate Wright map (also called
#'   item-person map) using \code{ggplot()} function from the \pkg{ggplot2}
#'   package and \code{plot_grid()} function from the \pkg{cowplot} package.
#'   Wright map is used to display histogram of factor scores and the item
#'   difficulty parameters estimated by the Rasch IRT model.
#'
#' @param theta numeric: vector of ability estimates.
#' @param b numeric: vector of difficulty estimates.
#' @param binwidth numeric: the width of the bins of histogram.
#' @param color character: color of histogram.
#' @param size text size in pts.
#' @param item.names names of items to be displayed.
#' @param ylab.theta character: description of y-axis for the histogram.
#' @param ylab.b character: description of y-axis for the plot of difficulty
#'   estimates.
#'
#' @inheritDotParams cowplot::plot_grid rel_widths
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
#' @references
#' Wright, B. & Stone, M. (1979). Best test design. MESA Press: Chicago, IL
#'
#' @examples
#' library(mirt)
#'
#' data(HCI)
#'
#' # fit Rasch model with the mirt package
#' fit <- mirt(HCI[, 1:20], model = 1, itemtype = "Rasch")
#' # factor scores
#' theta <- as.vector(fscores(fit))
#' # difficulty estimates using IRT parametrization
#' b <- coef(fit, simplify = TRUE, IRTpars = TRUE)$items[, "b"]
#'
#' # Wright map
#' ggWrightMap(theta, b)
#'
#' # Wright map with modified item names
#' item.names <- paste("Item", 1:20)
#' ggWrightMap(theta, b, item.names = item.names)
#'
#' # Wright map with modified descriptions of y-axis and relative widths of plots
#' ggWrightMap(theta, b,
#'   ylab.theta = "Latent trait", ylab.b = "Difficulty estimates",
#'   rel_widths = c(2, 1)
#' )
#' @export

ggWrightMap <- function(theta, b, binwidth = 0.5, color = "blue", size = 15, item.names,
                        ylab.theta = "Respondent latent trait", ylab.b = "Item difficulty",
                        ...) {
  if (missing(theta)) {
    stop("'theta' needs to be specified", call. = FALSE)
  }
  if (missing(b)) {
    stop("'theta' needs to be specified", call. = FALSE)
  }
  if (missing(item.names)) {
    ITEM.NAMES <- 1:length(b)
  } else {
    ITEM.NAMES <- item.names
  }

  df.theta <- data.frame(theta = theta)

  theta.cut.points <- seq(
    min(c(theta, b), na.rm = TRUE) - binwidth / 2,
    max(c(theta, b), na.rm = TRUE) + binwidth / 2, binwidth / 2
  )
  b.cut.points <- cut(b, theta.cut.points, include.lowest = TRUE)
  levels(b.cut.points) <- theta.cut.points[-length(theta.cut.points)] + diff(theta.cut.points) / 2
  b.cut.points <- as.numeric(paste(b.cut.points))

  df.b <- data.frame(item = as.character(ITEM.NAMES), b = b, y = b.cut.points)
  df.b$x <- 0
  for (i in unique(df.b$y)) {
    n <- nrow(df.b[df.b$y == i, ])
    df.b[df.b$y == i, "x"] <- 1:n
  }

  df.b$item <- as.character(df.b$item)
  maxn <- max(nchar(df.b$item))

  if (missing(item.names)) {
    while (any(nchar(df.b$item) < maxn)) {
      df.b$item <- ifelse(nchar(df.b$item) < maxn, paste0("0", df.b$item), df.b$item)
    }
  } else {
    df.b$item <- as.character(df.b$item)
    while (any(nchar(df.b$item) < maxn)) {
      df.b$item <- ifelse(nchar(df.b$item) < maxn, paste0(df.b$item, " "), df.b$item)
    }
  }

  df.b$item[df.b$x > 1] <- paste("|", df.b$item[df.b$x > 1])

  lim.x.min <- min(c(theta, b), na.rm = TRUE) - binwidth
  lim.x.max <- max(c(theta, b), na.rm = TRUE) + binwidth

  g1 <- ggplot(df.theta, aes_string(x = "theta")) +
    geom_histogram(binwidth = binwidth, fill = color, col = "black", na.rm = TRUE) +
    xlim(lim.x.min, lim.x.max) +
    coord_flip() +
    scale_y_reverse() +
    xlab(ylab.theta) +
    theme_app(base_size = size) +
    theme(
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank()
    )

  g2 <- ggplot(df.b, aes_string(x = "x", y = "y", label = "item")) +
    geom_text(hjust = 0, vjust = 0.5, na.rm = TRUE) +
    scale_y_continuous(position = "right", limits = c(lim.x.min, lim.x.max)) +
    scale_x_continuous(limits = c(min(df.b$x), max(df.b$x) + 0.75)) +
    ylab(ylab.b) +
    theme_app(base_size = size) +
    theme(
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank()
    )

  plot_grid(g1, g2, ...)
}
