#' Compute and plot an item correlation matrix
#'
#' @description Computes and visualizes an item correlation matrix (also known
#'   as a heatmap), offering several correlation "types" and optional clustering
#'   (with possible cluster outlining). The function relies on
#'   \code{\link{ggplot2}} package, providing a high customisability using "the
#'   grammar of graphics" (see the examples below).
#'
#' @param .data matrix, data.frame or tibble: either a dataframe with scored
#'   items (as columns, one observation per row), or a correlation matrix.
#'
#' @param cor character: correlation "type" used to correlation matrix
#'   computation; available options are "poly", "tetra", "pearson", "spearman"
#'   or "none" (in case you provide the correlation matrix directly instead).
#'
#' @param clust_method character: optional clustering method, available options
#'   are: \code{"ward.D"}, \code{"ward.D2"}, \code{"single"}, \code{"complete"},
#'   \code{"average"} (= UPGMA), \code{"mcquitty"} (= WPGMA), \code{"median"} (=
#'   WPGMC), \code{"centroid"} (= UPGMC) or "none" (clustering disabled). See
#'   \code{\link{hclust}} for a detailed description of available options.
#'
#' @param n_clust integer: the number of clusters you want to be outlined. When
#'   set to zero, clustering is disabled, ignoring the \code{clust_method}
#'   argument.
#'
#' @param labels logical: when TRUE, the correlation coefficients are plotted
#'   onto tiles
#'
#' @param labels_size numeric: label size in points (pts)
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
#' @inheritDotParams psych::tetrachoric -x -y -na.rm
#'
#' @details Correlation heatmap displays selected type of correlations between
#'   items.The color of tiles indicates how much and in which way the items are
#'   correlated - red color means possitive correlation and blue color means
#'   negative correlation. Correlation heatmap can be reordered using
#'   hierarchical clustering method specified with \code{clust_method} argument.
#'   When the desired number of clusters (argument \code{n_clust}) is not zero
#'   and any clustering is demanded, the rectangles outlining the found clusters
#'   are drawn.
#'
#' @return A ggplot object.
#'
#' @author Jan Netik \cr Department of Psychology, Faculty of Arts, Charles
#'   University \cr \email{netikja@@gmail.com}
#'
#'   Patricia Martinkova \cr Institute of Computer Science of the Czech Academy
#'   of Sciences \cr \email{martinkova@@cs.cas.cz} \cr
#'
#' @importFrom ggplot2 ggplot aes geom_tile labs scale_x_discrete
#'   scale_y_discrete scale_fill_gradient2 coord_fixed theme_minimal theme
#'   element_text element_blank annotate
#' @importFrom stats hclust as.dist cutree
#' @importFrom scales number
#' @importFrom psych polychoric tetrachoric alpha
#' @importFrom tibble as_tibble
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr mutate
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @examples
#' \dontrun{
#' # use first 20 columns from HCI dataset (the remainder are not items)
#' # and pipe it into the function
#' HCI[, 1:20] %>% plot_corr()
#'
#' # use tetrachoric correlation and reorder the resulting heatmap
#' # using Ward's method
#' HCI[, 1:20] %>% plot_corr(cor = "tetra", clust_method = "ward")
#'
#' # outline 3 Ward's clusters with bold yellow line and add labels
#' HCI[, 1:20] %>%
#'   plot_corr(n_clust = 3, clust_method = "ward.D", line_col = "yellow",
#'   line_size = 1.5, label = TRUE)
#'
#' # add title and position the legend below the plot
#' library(ggplot2)
#' HCI[, 1:20] %>% plot_corr(n_clust = 3) +
#'   ggtitle("HCI heatmap") +
#'   theme(legend.position = "bottom")
#' }
#'
#' @export
plot_corr <- function(.data, cor = "poly", clust_method = "none", n_clust = 0,
                      labels = FALSE, labels_size = 3,
                      line_size = .5, line_col = "black", line_alpha = 1,
                      fill = NA, fill_alpha = NA, ...) {
  cormat <- switch(cor,
    "poly" = tryCatch(psych::polychoric(.data, na.rm = TRUE, ...)$rho,
      error = function(e) {
        message(
          "Your items have more than 8 response categories, polychoric corr. is discouraged.\n",
          "Choose different correlation or stick with polychoric by specifying `max.cat = n`,\n",
          "where `n` is greater than the number of response categories of your items."
        )
      }
    ),
    "tetra" = psych::tetrachoric(.data, na.rm = TRUE, ...)$rho,
    "pearson" = cor(.data, method = "pearson", use = "pairwise.complete.obs"),
    "spearman" = cor(.data, method = "spearman", use = "pairwise.complete.obs"),
    "none" = .data,
    stop(
      "`", cor, "` is not a recognized correlation method!\n",
      "Use either `poly`, `tetra`, `pearson`, `spearman` or `none`."
    )
  )

  n <- nrow(cormat)
  m <- ncol(cormat)

  if (cor == "none" & n != m) {
    stop("The provided correlation matrix is not square.",
         call. = FALSE)
  }

  if (n_clust > n) {
    stop("There are only ", n, " items available, cannot display ", n_clust, " clusters.",
         call. = FALSE)
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
      warning("Showing a plain heatmap, because you have not stated any clustering method.")
    }
    new_ord <- colnames(cormat)
  }

  plt <- cormat %>%
    tibble::as_tibble(rownames = "x") %>%
    tidyr::pivot_longer(cols = -.data$x, names_to = "y", values_to = "r") %>%
    dplyr::mutate(corr. = gsub("0\\.", "\\.", scales::number(.data$r, .01))) %>%
    ggplot2::ggplot(ggplot2::aes(.data$x, .data$y, label = .data$corr.)) +
    ggplot2::geom_point(aes(size = .data$r, col = .data$r)) +
    ggplot2::scale_x_discrete(limits = new_ord, position = "top") +
    ggplot2::scale_y_discrete(limits = rev(new_ord)) + # make diagonal as usual
    ggplot2::scale_color_gradient2(
      midpoint = 0,
      limit = c(-1, 1)
    ) +
    ggplot2::scale_size_area(guide = "none") +
    ggplot2::labs(col = "corr.") +
    ggplot2::coord_fixed() +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x.top = ggplot2::element_text(angle = 90, vjust = .5, hjust = 0),
      axis.title = ggplot2::element_blank()
    )

  if (clust_method != "none" & n_clust != 0) {
    plt <- plt +
      ggplot2::annotate("rect",
        fill = scales::alpha(fill, fill_alpha),
        col = scales::alpha(line_col, line_alpha),
        size = line_size,
        xmin = cu[-(n_clust + 1)] + 0.5,
        xmax = cu[-1] + 0.5,
        ymin = n - cu[-(n_clust + 1)] + 0.5,
        ymax = n - cu[-1] + 0.5
      )
  }
  if (labels) {
    plt <- plt +
      ggplot2::geom_text(
        size = labels_size
      )
  }
  return(plt)
}
