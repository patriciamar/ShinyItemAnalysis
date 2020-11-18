#' Plot category probabilities of adjacent logistic regression model
#'
#' @aliases plotAdjacent
#'
#' @description Function for plotting category probabilities function estimated
#'   by \code{vglm()} from \code{VGAM} package.
#'
#' @param x object of class \code{vglm}
#' @param matching.name character: name of matching criterion used for
#'   estimation in \code{x}.
#'
#' @author
#' Tomas Jurica \cr
#'
#' Adela Hladka \cr
#' Institute of Computer Science of the Czech Academy of Sciences \cr
#' Faculty of Mathematics and Physics, Charles University \cr
#' \email{hladka@@cs.cas.cz} \cr
#'
#' Patricia Martinkova \cr
#' Institute of Computer Science of the Czech Academy of Sciences \cr
#' \email{martinkova@@cs.cas.cz} \cr
#'
#' @seealso
#' \code{\link[VGAM]{vglm}}
#'
#' @examples
#' # loading packages
#' library(VGAM)
#'
#' # loading data
#' data <- dataMedicalgraded[, 1:100]
#'
#' # total score calculation
#' score <- apply(data, 1, sum)
#' data[, 1] <- ordered(factor(data[, 1], levels = 0:max(data[, 1])))
#'
#' # cummulative logistic model for item 1
#' fit <- vglm(data[, 1] ~ score, family = acat(reverse = FALSE, parallel = TRUE))
#' # coefficients for item 1
#' coefs <- coef(fit)
#'
#' plotAdjacent(fit, matching.name = "Total score")
#' @export
plotAdjacent <- function(x, matching.name = "matching") {
  y <- x@y %*% as.numeric(colnames(x@y)) # responses
  cat <- as.numeric(paste(colnames(x@y))) # all categories
  num.cat <- length(cat) # number of all categories
  y <- factor(y, levels = cat) # releveling
  matching <- x@x[, 2] # matching
  match <- seq(min(matching, na.rm = TRUE), max(matching, na.rm = TRUE), 0.01)

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
  # df.probs.cat <- melt(df.probs.cat, id.vars = "matching", variable.name = "category", value.name = "probability")
  df.probs.cat <- tidyr::pivot_longer(df.probs.cat, -matching, names_to = "category", values_to = "probability")
  df.probs.cat$category <- as.factor(df.probs.cat$category)

  # empirical category values
  df.emp.cat <- data.frame(table(y, matching),
    y = prop.table(table(y, matching), 2)
  )[, c(1, 2, 3, 6)]
  df.emp.cat$matching <- as.numeric(paste(df.emp.cat$matching))
  colnames(df.emp.cat) <- c("category", "matching", "size", "probability")
  df.emp.cat$category <- as.factor(df.emp.cat$category)
  levels(df.emp.cat$category) <- paste0("P(Y=", levels(df.emp.cat$category), ")")

  # colours
  gg_color_hue <- function(n) {
    hues <- seq(15, 375, length = n + 1)
    hcl(h = hues, l = 65, c = 100)[1:n]
  }
  cols <- c("black", gg_color_hue(num.cat - 1))

  df.emp.cat <- df.emp.cat[df.emp.cat$category %in% paste0("P(Y=", cat, ")"), ]
  df.probs.cat <- df.probs.cat[df.probs.cat$category %in% paste0("P(Y=", cat, ")"), ]

  rangex <- c(
    min(c(df.emp.cat$matching, df.probs.cat$matching)),
    max(c(df.emp.cat$matching, df.probs.cat$matching))
  )

  g <- ggplot() +
    geom_point(
      data = df.emp.cat,
      aes_string(
        x = "matching", y = "probability", group = "category",
        size = "size", col = "category", fill = "category"
      ),
      shape = 21, alpha = 0.5
    ) +
    geom_line(
      data = df.probs.cat,
      aes_string(
        x = "matching", y = "probability",
        col = "category", linetype = "category"
      ),
      size = 1
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
