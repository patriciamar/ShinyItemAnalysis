#' Complete theme for \code{ShinyItemAnalysis} graphics
#'
#' @aliases theme_app
#'
#' @description This complete theme is based on \code{theme_bw} and it was
#'   modified for purposes of \code{ShinyItemAnalysis}.
#'
#' @param base_size base font size
#' @param base_family base font family
#'
#' @seealso \code{\link[ggplot2]{ggtheme}}
#'
#' @examples
#' library(ggplot2)
#' data(GMAT, package = "difNLR")
#' data <- GMAT[, 1:20]
#' # total score calculation
#' df <- data.frame(score = apply(data, 1, sum))
#' # histogram
#' g <- ggplot(df, aes(score)) +
#'   geom_histogram(binwidth = 1) +
#'   xlab("Total score") +
#'   ylab("Number of respondents")
#'
#' g
#' g + theme_app()
#' @export
theme_app <- function(base_size = 15, base_family = "") {
  theme_bw(base_size = base_size, base_family = base_family) +
    theme(
      legend.key = element_rect(fill = "white", colour = NA),
      legend.title = element_blank(),
      legend.position = "none",
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      plot.title = element_text(face = "bold")
    )
}
