#' Compute and plot an item correlation matrix
#'
#' Computes and visualizes an item correlation matrix (also known as a heatmap),
#' offering several correlation "types" and optional clustering (with possible
#' cluster outlining). The function relies on [ggplot2::ggplot()], providing a
#' high customisability using "the grammar of graphics" (see the examples
#' below).
#'
#' Correlation heatmap displays selected type of correlations between items. The
#' color of tiles indicates how much and in which way the items are correlated
#' -- red color means positive correlation and blue color means negative
#' correlation. Correlation heatmap can be reordered using hierarchical
#' clustering method specified with `clust_method` argument. When the desired
#' number of clusters (argument `n_clust`) is not zero and some clustering is
#' demanded, the rectangles outlining the found clusters are drawn.
#'
#' @param Data `matrix`, `data.frame` or `tibble`: either a
#'   `data.frame` with scored items (as columns, one observation per row),
#'   or a correlation matrix.
#'
#' @param cor character: correlation "type" used to correlation matrix
#'   computation; available options are `polychoric`, `tetrachoric`,
#'   `pearson`, `spearman`, or `none` (in case you provide
#'   the correlation matrix as `Data`).
#'
#' @param clust_method character: optional clustering method, available options
#'   are: `ward.D`, `ward.D2`, `single`, `complete`,
#'   `average` (= UPGMA), `mcquitty` (= WPGMA), `median` (=
#'   WPGMC), `centroid` (= UPGMC) or `none` (clustering disabled).
#'   See [hclust()] for a detailed description of available options.
#'
#' @param n_clust integer: the number of clusters you want to be outlined. When
#'   set to zero (the default), no cluster are outlined, but items still do get
#'   sorted according to `clust_method` (if not set to `none`).
#'
#' @param shape character: tile appearance; either `circle` (default) to
#'   map the correlation coefficient to circle size and color, or `square`
#'   to draw square-shaped tiles with only shade denoting the coefficient
#'   magnitude. You can use an unambiguous abbreviation of the two.
#'
#' @param labels logical: when `TRUE`, the correlation coefficients are
#'   plotted onto tiles.
#'
#' @param labels_size numeric: label size in points (pts).
#'
#' @param line_size numeric: cluster outline width.
#'
#' @param line_col character: color of the outline, either a HEX code (e.g.
#'   "#123456"), or one of `R`'s standard colors (see the
#'   [colors()]).
#'
#' @param line_size numeric: cluster outline width.
#'
#' @param line_alpha numeric 0-1: the opacity of the outline.
#'
#' @param fill character: the color used to fill the outlined clusters.
#'
#' @param fill_alpha numeric 0--1: the opacity of the fill color.
#'
#' @inheritDotParams psych::polychoric -x -y -na.rm -polycor
#'
#' @return An object of class `ggplot` and/or `gg`.
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
#' @importFrom purrr keep
#' @importFrom rlang .data arg_match abort inform try_fetch
#'
#' @examples
#' # use first 20 columns from HCI dataset (the remainder are not items)
#' HCI <- HCI[, 1:20]
#'
#' # use Pearson product-moment correlation coefficient for matrix computation
#' plot_corr(HCI, cor = "pearson")
#'
#' \dontrun{
#' # use tetrachoric correlation and reorder the resulting heatmap
#' # using Ward's method
#' HCI %>% plot_corr(cor = "tetrachoric", clust_method = "ward.D")
#'
#' # outline 3 Ward's clusters with bold yellow line and add labels
#' HCI %>%
#'   plot_corr(
#'     n_clust = 3, clust_method = "ward.D2", line_col = "yellow",
#'     line_size = 1.5, labels = TRUE
#'   )
#'
#' # add title and position the legend below the plot
#' library(ggplot2)
#' HCI %>%
#'   plot_corr(n_clust = 3) +
#'   ggtitle("HCI heatmap") +
#'   theme(legend.position = "bottom")
#'
#' # mimic the look of corrplot package
#' plot_corr(HCI, cor = "polychoric", clust_method = "complete", shape = "square") +
#'   scale_fill_gradient2(
#'     limits = c(-.1, 1),
#'     breaks = seq(-.1, 1, length.out = 12),
#'     guide = guide_colorbar(
#'       barheight = .8, barwidth = .0275,
#'       default.unit = "npc",
#'       title = NULL, frame.colour = "black", ticks.colour = "black"
#'     )
#'   ) +
#'   theme(axis.text = element_text(colour = "red", size = 12))
#' }
#'
#' @export
plot_corr <- function(Data,
                      cor = c("polychoric", "tetrachoric", "pearson", "spearman", "none"),
                      clust_method = "none", n_clust = 0L,
                      shape = c("circle", "square"),
                      labels = FALSE, labels_size = 3,
                      line_size = .5, line_col = "black", line_alpha = 1,
                      fill = NA, fill_alpha = NA, ...) {
  # first, detect if Data is corr. matrix, so we can set cor = none as early as possible
  if (is_corr(Data)) {
    inform(c("i" = "The input was recognized as a correlation matrix. Setting `cor = \"none\"`."))
    cor <- "none"
  }

  # check the args
  cor <- arg_match(cor)
  shape <- arg_match(shape)

  # compute corr. matrix
  cormat <- switch(cor,
    "polychoric" = try_fetch(
      polychoric(Data, na.rm = TRUE, ...)$rho,
      error = function(cnd) {
        affected_items <- names(keep(Data, function(x) max(x, na.rm = TRUE) > 8))
        abort(
          c(
            "Polychoric correlations were not estimated. Please inspect the error message below.",
            "i" = paste(
              "However, a common cause is that your items have more than 8 categories.",
              "Polychoric correlation is deemed superfluous in that case.",
              "You can still proceed by adding `max.cat = Inf` as an argument to `plot_corr()`."
            ),
            "i" = paste0(
              "Another common cause is that you have included wrong items, check the following ones: ",
              paste(affected_items, collapse = ", "), "."
            )
          ),
          parent = cnd
        )
      }
    ),
    "tetrachoric" = try_fetch(
      tetrachoric(Data, na.rm = TRUE, ...)$rho,
      error = function(cnd) {
        affected_items <- names(keep(Data, function(x) max(x, na.rm = TRUE) > 1))
        abort(
          c(
            "Tetrachoric correlations were not estimated. Please inspect the error message below.",
            "i" = paste0(
              "A common cause is that you have included wrong items, check the following ones: ",
              paste(affected_items, collapse = ", "), "."
            )
          ),
          parent = cnd
        )
      }
    ),
    "pearson" = cor(Data, method = "pearson", use = "pairwise.complete.obs"),
    "spearman" = cor(Data, method = "spearman", use = "pairwise.complete.obs"),
    "none" = Data
  )

  n <- nrow(cormat)

  if (is.null(dimnames(cormat))) {
    inform(c("i" = "Estimated correlation matrix has no names, using integers instead."))

    nms <- seq_len(n)
    # set both row- and col-names
    dimnames(cormat) <- list(nms, nms)
  }

  if (n_clust > n) {
    abort(
      paste0("There are only ", n, " items available, cannot display ", n_clust, " clusters.")
    )
  }

  # set n_clust to zero when clust_method is none, because any other value makes no sense
  if (clust_method == "none") {
    # inform the user if he/she set nonzero value, so it is is clear what has happened
    if (n_clust != 0L) {
      inform(c("i" = "Overwriting `n_clust` with `0`, because `clust_method = \"none\"`. Nothing will be outlined."))
      n_clust <- 0L
    }

    # we'll use that in ggplot to label the items
    new_ord <- colnames(cormat)
  } else {
    # if any clust_method is set
    tree <- hclust(as.dist(1 - cormat), method = clust_method)
    ord <- tree$order

    # new item names order according to item "distance"
    new_ord <- colnames(cormat)[ord]

    if (n_clust == 0L) {
      # just inform what is gonna happen
      inform(
        c(
          "i" = paste0("`n_clust = 0`, but the items will still be sorted according to `", clust_method, "` method."),
          "*" = "Set `clust_method = \"none\"` to keep the original order."
        )
      )
    }
  }

  # .data is a pronoun for cormat in non-standard evaluation
  plt <- cormat %>%
    as_tibble(rownames = "x") %>%
    pivot_longer(cols = -.data$x, names_to = "y", values_to = "r") %>%
    mutate(corr. = gsub("0\\.", "\\.", round(.data$r, digits = 2L))) %>%
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
    plt <- plt +
      geom_point(aes(size = .data$r, col = .data$r)) +
      scale_color_gradient2(
        midpoint = 0,
        limit = c(-1, 1)
      )
  } else {
    plt <- plt +
      geom_tile(aes(fill = .data$r)) +
      scale_fill_gradient2(
        midpoint = 0,
        limit = c(-1, 1)
      )
  }

  # outline the clusters if the number of clusters is not zero
  if (n_clust != 0L) {
    # cut the tree according to n_clust the cu object is for cluster outlining
    hc <- cutree(tree, k = n_clust)
    clustab <- table(hc)[unique(hc[ord])]
    cu <- c(0, cumsum(clustab))

    plt <- plt +
      annotate("rect",
        fill = ggplot2::alpha(fill, fill_alpha), # cannot use alpha in NAMESPACE due to psych conflict
        col = ggplot2::alpha(line_col, line_alpha),
        size = line_size,
        xmin = cu[-(n_clust + 1)] + .5,
        xmax = cu[-1] + .5,
        ymin = n - cu[-(n_clust + 1)] + .5,
        ymax = n - cu[-1] + .5
      )
  }

  if (labels) {
    plt <- plt +
      geom_text(size = labels_size)
  }

  plt
}
