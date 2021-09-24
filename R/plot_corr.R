#' Compute and plot an item correlation matrix
#'
#' @description Computes and visualizes an item correlation matrix (also known
#'   as a heatmap), offering several correlation "types" and optional clustering
#'   (with possible cluster outlining). The function relies on
#'   \code{\link{ggplot2}} package, providing a high customisability using "the
#'   grammar of graphics" (see the examples below).
#'
#' @param Data \code{matrix}, \code{data.frame} or \code{tibble}: either a
#'   \code{data.frame} with scored items (as columns, one observation per row),
#'   or a correlation matrix.
#'
#' @param cor character: correlation "type" used to correlation matrix
#'   computation; available options are \code{"poly"}, \code{"tetra"},
#'   \code{"pearson"}, \code{"spearman"}, or \code{"none"} (in case you provide
#'   the correlation matrix directly instead). You can use an unambiguous
#'   abbreviation.
#'
#' @param clust_method character: optional clustering method, available options
#'   are: \code{"ward.D"}, \code{"ward.D2"}, \code{"single"}, \code{"complete"},
#'   \code{"average"} (= UPGMA), \code{"mcquitty"} (= WPGMA), \code{"median"} (=
#'   WPGMC), \code{"centroid"} (= UPGMC) or \code{"none"} (clustering disabled).
#'   See \code{\link{hclust}} for a detailed description of available options.
#'
#' @param n_clust integer: the number of clusters you want to be outlined. When
#'   set to zero, clustering is disabled, ignoring the \code{clust_method}
#'   argument.
#'
#' @param shape character: tile appearance; either \code{circle} (default) to
#'   map the correlation coefficient to circle size and color, or \code{square}
#'   to draw square-shaped tiles with only shade denoting the coefficient
#'   magnitude. You can use an unambiguous abbreviation of the two.
#'
#' @param labels logical: when \code{TRUE}, the correlation coefficients are
#'   plotted onto tiles.
#'
#' @param labels_size numeric: label size in points (pts).
#'
#' @param line_size numeric: cluster outline width.
#'
#' @param line_col character: color of the outline, either a HEX code (e.g.
#'   "#123456"), or one of \code{R}'s standard colors (see the
#'   \code{\link{colors}}).
#'
#' @param line_size numeric: cluster outline width.
#'
#' @param line_alpha numeric 0-1: the opacity of the outline.
#'
#' @param fill character: the color used to fill the outlined clusters.
#'
#' @param fill_alpha numeric 0-1: the opacity of the fill color.
#'
#' @inheritDotParams psych::polychoric -x -y -na.rm
#'
#' @details Correlation heatmap displays selected type of correlations between
#'   items.The color of tiles indicates how much and in which way the items are
#'   correlated - red color means positive correlation and blue color means
#'   negative correlation. Correlation heatmap can be reordered using
#'   hierarchical clustering method specified with \code{clust_method} argument.
#'   When the desired number of clusters (argument \code{n_clust}) is not zero
#'   and some clustering is demanded, the rectangles outlining the found
#'   clusters are drawn.
#'
#' @return An object of class \code{ggplot} and/or \code{gg}.
#'
#' @author
#' Jan Netik \cr
#' Institute of Computer Science of the Czech Academy of Sciences \cr
#' \email{netik@@cs.cas.cz}
#'
#' Patricia Martinkova \cr
#' Institute of Computer Science of the Czech Academy of Sciences \cr
#' \email{martinkova@@cs.cas.cz}
#'
#' @importFrom ggplot2 ggplot aes geom_tile labs scale_x_discrete
#'   scale_y_discrete scale_fill_gradient2 coord_fixed theme_minimal theme
#'   element_text element_blank annotate scale_size_area scale_color_gradient2
#' @importFrom stats hclust as.dist cutree
#' @importFrom psych polychoric tetrachoric alpha
#' @importFrom tibble as_tibble
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr mutate
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @examples
#' # use first 20 columns from HCI dataset (the remainder are not items)
#' HCI <- HCI[, 1:20]
#'
#' # use Pearson product-moment correlation coefficient for matrix computation
#' plot_corr(HCI, cor = "pearson")
#' \dontrun{
#' # use tetrachoric correlation and reorder the resulting heatmap
#' # using Ward's method
#' HCI %>% plot_corr(cor = "tetra", clust_method = "ward.D")
#'
#' # outline 3 Ward's clusters with bold yellow line and add labels
#' HCI %>%
#'   plot_corr(
#'     n_clust = 3, clust_method = "ward.D", line_col = "yellow",
#'     line_size = 1.5, labels = TRUE
#'   )
#'
#' # add title and position the legend below the plot
#' library(ggplot2)
#' HCI %>% plot_corr(n_clust = 3) +
#'   ggtitle("HCI heatmap") +
#'   theme(legend.position = "bottom")
#'
#' # mimic the look of corrplot package
#' plot_corr(HCI, cor = "poly", clust_method = "complete", shape = "sq") +
#'   scale_fill_gradient2(
#'     limits = c(-.1, 1),
#'     breaks = seq(-.1, 1, length.out = 12),
#'     guide = guide_colorbar(
#'       barheight = .8, barwidth = .0275,
#'       default.unit = "npc",
#'       title = NULL, frame.colour = "black", ticks.colour = "black"
#'     )
#'   ) + theme(axis.text = element_text(colour = "red", size = 12))
#' }
#'
#' @export
plot_corr <- function(Data, cor = "pearson", clust_method = "none", n_clust = 0,
                      shape = "circle",
                      labels = FALSE, labels_size = 3,
                      line_size = .5, line_col = "black", line_alpha = 1,
                      fill = NA, fill_alpha = NA, ...) {
  if (is_corr(Data)) {
    message("The input was recognized as a correlation matrix. Assuming 'cor = none'.")
    cor <- "none"
  }

  cor <- tryCatch(match.arg(cor, c(
    "polychoric", "tetrachoric", "pearson", "spearman", "none"
  )),
  error = function(e) {
    stop("'cor' should be one of 'polychoric', 'tetrachoric', 'pearson', 'spearman' or 'none'.",
      call. = FALSE
    )
  }
  )

  shape <- tryCatch(match.arg(shape, c("circle", "square")),
    error = function(e) {
      stop("'shape' should be one of 'circle' or 'square'.",
        call. = FALSE
      )
    }
  )
  cormat <- switch(cor,
    "polychoric" = tryCatch(polychoric(Data, na.rm = TRUE, ...)$rho,
      error = function(e) {
        message(
          "Your items have more than 8 response categories, use of polychoric corr. is discouraged.\n",
          "Choose different correlation or stick with polychoric by specifying `max.cat = n`,\n",
          "where `n` is greater than the number of response categories of your items."
        )
      }
    ),
    "tetrachoric" = tryCatch(tetrachoric(Data, na.rm = TRUE, ...)$rho,
      error = function(e) {
        if (max(Data, na.rm = TRUE) > 1) {
          stop("Tetrachoric correlation requires dichotomous data.",
            call. = FALSE
          )
        } else {
          stop(e)
        }
      }
    ),
    "pearson" = cor(Data, method = "pearson", use = "pairwise.complete.obs"),
    "spearman" = cor(Data, method = "spearman", use = "pairwise.complete.obs"),
    "none" = Data
  )

  n <- nrow(cormat)
  m <- ncol(cormat)

  if (is.null(dimnames(cormat))) {
    warning("Estimated correlation matrix has no names, using integers.",
      call. = FALSE
    )
    nms <- seq_len(ncol(cormat))
    dimnames(cormat) <- list(nms, nms)
  }

  if (n_clust > n) {
    stop("There are only ", n, " items available, cannot display ", n_clust, " clusters.",
      call. = FALSE
    )
  }

  if (clust_method != "none") {
    tree <- stats::hclust(as.dist(1 - cormat), method = clust_method)
    ord <- tree$order
    new_ord <- colnames(cormat)[ord]

    if (n_clust != 0) {
      hc <- stats::cutree(tree, k = n_clust)
      clustab <- table(hc)[unique(hc[ord])]
      cu <- c(0, cumsum(clustab))
    }
  } else {
    if (n_clust != 0) {
      warning("Showing a plain heatmap, because no clustering method was selected.",
        call. = FALSE
      )
    }
    new_ord <- colnames(cormat)
  }

  # .data is a pronoun for cormat in non-standard evaluation
  plt <- cormat %>%
    as_tibble(rownames = "x") %>%
    pivot_longer(cols = -.data$x, names_to = "y", values_to = "r") %>%
    mutate(corr. = gsub("0\\.", "\\.", round(.data$r, digits = 2))) %>%
    ggplot(aes(.data$x, .data$y, label = .data$corr.)) +
    scale_x_discrete(limits = new_ord, position = "top") +
    scale_y_discrete(limits = rev(new_ord)) + # make diagonal as usual
    scale_size_area(guide = "none") +
    labs(col = "corr.", fill = "corr.") +
    coord_fixed() +
    theme_minimal() +
    theme(
      axis.text.x.top = element_text(angle = 90, vjust = .5, hjust = 0),
      axis.title = element_blank()
    )

  if (shape == "circle") {
    plt <- plt + geom_point(aes(size = .data$r, col = .data$r)) +
      scale_color_gradient2(
        midpoint = 0,
        limit = c(-1, 1)
      )
  } else {
    plt <- plt + geom_tile(aes(fill = .data$r)) +
      scale_fill_gradient2(
        midpoint = 0,
        limit = c(-1, 1)
      )
  }

  if (clust_method != "none" & n_clust != 0) {
    plt <- plt +
      annotate("rect",
        fill = ggplot2::alpha(fill, fill_alpha), # cannot use alpha in NAMESPACE due to psych conflict
        col = ggplot2::alpha(line_col, line_alpha),
        size = line_size,
        xmin = cu[-(n_clust + 1)] + 0.5,
        xmax = cu[-1] + 0.5,
        ymin = n - cu[-(n_clust + 1)] + 0.5,
        ymax = n - cu[-1] + 0.5
      )
  }
  if (labels) {
    plt <- plt +
      geom_text(
        size = labels_size
      )
  }

  plt
}
