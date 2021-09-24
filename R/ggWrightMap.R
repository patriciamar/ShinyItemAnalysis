#' Plot Wright map using \code{ggplot2}
#'
#' @aliases ggWrightMap
#'
#' @description This function allows to generate Wright map (also called
#'   item-person map) using \code{ggplot()} function from the \pkg{ggplot2}
#'   package. Wright map is used to display histogram of factor scores and the
#'   item difficulty parameters estimated by the Rasch IRT model.
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
#' @param rel_widths numeric: vector of length 2 specifying ratio of "facet's"
#'   widths.
#'
#' @importFrom ggplot2 ggplotGrob unit
#' @importFrom grid grid.newpage gTree grid.draw
#' @importFrom mirt fscores
#'
#' @author
#' Adela Hladka \cr
#' Institute of Computer Science of the Czech Academy of Sciences \cr
#' \email{hladka@@cs.cas.cz}
#'
#' Jan Netik \cr
#' Institute of Computer Science of the Czech Academy of Sciences \cr
#' \email{netik@@cs.cas.cz}
#'
#' Patricia Martinkova \cr
#' Institute of Computer Science of the Czech Academy of Sciences \cr
#' \email{martinkova@@cs.cas.cz}
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

ggWrightMap <- function(theta, b, binwidth = 0.5, color = "blue", size = 15,
                        item.names, ylab.theta = "Respondent latent trait",
                        ylab.b = "Item difficulty", rel_widths = c(1, 1)) {
  plts <- gg_wright_internal(
    theta, b, binwidth, color, size, item.names, ylab.theta, ylab.b, rel_widths
  )

  # arrange plots using bare grid
  grid.newpage()

  grobs <- lapply(plts, ggplotGrob)

  new_layout <- data.frame(list(
    t = c(1, 1),
    l = c(1, 2),
    b = c(1, 1),
    r = c(1, 2),
    z = c(1, 2),
    clip = rep("off", 2),
    name = rep("arrange", 2)
  ))

  # make gtable
  g <- gTree(
    grobs = grobs, layout = new_layout, widths = unit(rel_widths, "null"),
    heights = unit(1, "null"), respect = FALSE, name = "arrange", rownames = NULL,
    colnames = NULL, vp = NULL, cl = "gtable"
  )

  grid.draw(g)
  invisible(g)
}



#' ggWrightMap internals
#'
#' @keywords internal
#'
#' @importFrom ggplot2 unit aes_string geom_histogram xlim ylab coord_flip
#'   scale_y_reverse geom_text scale_y_continuous
#'
#' @noRd
gg_wright_internal <- function(theta, b, binwidth = 0.5, color = "blue",
                               size = 15, item.names,
                               ylab.theta = "Respondent latent trait",
                               ylab.b = "Item difficulty",
                               rel_widths = c(1, 1)) {
  if (missing(theta)) {
    stop("'theta' needs to be specified", call. = FALSE)
  }
  if (missing(b)) {
    stop("'theta' needs to be specified", call. = FALSE)
  }
  if (length(rel_widths) != 2) {
    stop("'rel_widths' needs to be a numeric vector of length 2, e.g., 'c(1, 2)'.", call. = FALSE)
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

  list(g1, g2)
}
