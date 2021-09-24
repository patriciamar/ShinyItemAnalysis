#' Plot category probabilities of adjacent category logit model
#'
#' @aliases plotAdjacent
#'
#' @description Function for plotting category probabilities function estimated
#'   by \code{vglm()} function from the \code{VGAM} package using the
#'   \pkg{ggplot2} package.
#'
#' @param x object of class \code{vglm}
#' @param matching.name character: name of matching criterion used for
#'   estimation in \code{x}.
#'
#' @return An object of class \code{ggplot} and/or \code{gg}.
#'
#' @author
#' Tomas Jurica \cr
#' Institute of Computer Science of the Czech Academy of Sciences \cr
#'
#' Adela Hladka \cr
#' Institute of Computer Science of the Czech Academy of Sciences \cr
#' \email{hladka@@cs.cas.cz}
#'
#' Patricia Martinkova \cr
#' Institute of Computer Science of the Czech Academy of Sciences \cr
#' \email{martinkova@@cs.cas.cz}
#'
#' @seealso
#' \code{\link[VGAM]{vglm}}
#'
#' @examples
#' # loading packages
#' library(VGAM)
#'
#' # loading data
#' data(Science, package = "mirt")
#'
#' # total score calculation
#' score <- rowSums(Science)
#' Science[, 1] <- factor(Science[, 1], levels = sort(unique(Science[, 1])), ordered = TRUE)
#'
#' # adjacent category logit model for item 1
#' fit <- vglm(Science[, 1] ~ score, family = acat(reverse = FALSE, parallel = TRUE))
#' # coefficients for item 1
#' coef(fit)
#'
#' plotAdjacent(fit, matching.name = "Total score")
#' @importFrom grDevices hcl
#' @importFrom ggplot2 scale_colour_manual ylim guides guide_legend
#' @export
plotAdjacent <- function(x, matching.name = "matching") {
  y <- x@y %*% as.numeric(colnames(x@y)) # responses
  cat <- as.numeric(paste(colnames(x@y))) # all categories
  num.cat <- length(cat) # number of all categories
  y <- factor(y, levels = cat) # releveling
  matching <- x@x[, 2] # matching
  match <- seq(min(matching, na.rm = TRUE), max(matching, na.rm = TRUE), length.out = 1000)

  coefs <- coef(x) # extracting coefficients
  cat.obs <- names(which(table(y) > 0)[-1]) # observed categories = categories with at least one observation
  num.cat.obs <- length(coefs) - 1 # number of categories with at least one observation

  # category probabilities
  df.probs.cat <- matrix(0, nrow = length(match), ncol = num.cat)
  colnames(df.probs.cat) <- paste(cat)

  # calculation probabilities on formula exp(\sum_{t = 0}^{k} b_{0t} + b1X)/(\sum_{r = 0}^{K}exp(\sum_{t=0}^{r}b_{0t} + b1X))
  df.probs.cat[, cat.obs] <- sapply(1:num.cat.obs, function(i) coefs[i] + coefs[num.cat.obs + 1] * match)
  # cumulative sum
  df.probs.cat <- t(apply(df.probs.cat, 1, cumsum))
  # exponential
  df.probs.cat <- exp(df.probs.cat)
  # norming
  df.probs.cat <- df.probs.cat / apply(df.probs.cat, 1, sum)

  # reshaping data
  df.probs.cat <- data.frame(match, df.probs.cat)
  colnames(df.probs.cat) <- c("matching", paste0("P(Y=", cat, ")"))
  df.probs.cat <- tidyr::pivot_longer(df.probs.cat, -matching, names_to = "Category", values_to = "Probability")
  df.probs.cat$Category <- as.factor(df.probs.cat$Category)
  colnames(df.probs.cat)[1] <- "Matching"

  # empirical category values
  df.emp.cat <- data.frame(table(y, matching),
    y = prop.table(table(y, matching), 2)
  )[, c(1, 2, 3, 6)]
  df.emp.cat$matching <- as.numeric(paste(df.emp.cat$matching))
  colnames(df.emp.cat) <- c("Category", "Matching", "Count", "Probability")
  df.emp.cat$Category <- as.factor(df.emp.cat$Category)
  levels(df.emp.cat$Category) <- paste0("P(Y=", levels(df.emp.cat$Category), ")")

  # colours
  gg_color_hue <- function(n) {
    hues <- seq(15, 375, length = n + 1)
    hcl(h = hues, l = 65, c = 100)[1:n]
  }
  cols <- c("black", gg_color_hue(num.cat - 1))

  df.emp.cat <- df.emp.cat[df.emp.cat$Category %in% paste0("P(Y=", cat, ")"), ]
  df.probs.cat <- df.probs.cat[df.probs.cat$Category %in% paste0("P(Y=", cat, ")"), ]

  rangex <- c(
    min(c(df.emp.cat$Matching, df.probs.cat$Matching)),
    max(c(df.emp.cat$Matching, df.probs.cat$Matching))
  )

  g <- ggplot() +
    geom_point(
      data = df.emp.cat,
      aes_string(
        x = "Matching", y = "Probability",
        size = "Count", col = "Category", fill = "Category"
      ),
      shape = 21, alpha = 0.5
    ) +
    geom_line(
      data = df.probs.cat,
      aes_string(
        x = "Matching", y = "Probability",
        col = "Category", linetype = "Category"
      ),
      size = 0.8
    ) +
    scale_fill_manual(values = cols) +
    scale_colour_manual(values = cols) +
    xlab(matching.name) +
    ylab("Category probability") +
    ylim(0, 1) +
    xlim(rangex[1], rangex[2]) +
    theme_app() +
    theme(
      legend.box = "horizontal",
      legend.position = c(0.03, 0.97),
      legend.justification = c(0.03, 0.97)
    ) +
    guides(
      size = guide_legend(order = 1),
      colour = guide_legend(order = 2),
      fill = guide_legend(order = 2),
      linetype = guide_legend(order = 2)
    )

  return(g)
}
