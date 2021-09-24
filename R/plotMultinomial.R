#' Plot category probabilities of multinomial model
#'
#' @aliases plotMultinomial
#'
#' @description Plots category probabilities functions estimated by
#'   \code{multinom()} from the \code{nnet} package using the \pkg{ggplot2}
#'   package.
#'
#' @param x object of class \code{multinom}
#' @param matching numeric: vector of matching criterion used for estimation in
#'   \code{x}.
#' @param matching.name character: name of matching criterion used for
#'   estimation in \code{x}.
#'
#' @return An object of class \code{ggplot} and/or \code{gg}.
#'
#' @author
#' Adela Hladka \cr
#' Institute of Computer Science of the Czech Academy of Sciences \cr
#' \email{hladka@@cs.cas.cz}
#'
#' Tomas Jurica \cr
#' Institute of Computer Science of the Czech Academy of Sciences
#'
#' Patricia Martinkova \cr
#' Institute of Computer Science of the Czech Academy of Sciences \cr
#' \email{martinkova@@cs.cas.cz}
#'
#' @seealso
#' \code{\link[nnet]{multinom}}
#'
#' @examples
#' # loading data
#' data(GMAT, GMATtest, GMATkey, package = "difNLR")
#'
#' matching <- scale(rowSums(GMAT[, 1:20])) # Z-score
#'
#' # multinomial model for item 1
#' fit <- nnet::multinom(relevel(GMATtest[, 1], ref = paste(GMATkey[1])) ~ matching)
#'
#' # plotting category probabilities
#' plotMultinomial(fit, matching, matching.name = "Z-score")
#'
#' @importFrom nnet multinom
#' @importFrom ggplot2 ylim scale_linetype_manual guides guide_legend
#'
#' @export

plotMultinomial <- function(x, matching, matching.name = "matching") {
  # extracting data
  cat <- colnames(x$fitted.values)
  if (is.null(cat)) {
    y <- (x$fitted.values + x$residuals) %*% 1:ncol(x$fitted.values)
    y <- factor(y, levels = 0:1)
    cat <- x$lev
    levels(y) <- cat
  } else {
    y <- (x$fitted.values + x$residuals) %*% 1:ncol(x$fitted.values)
    y <- factor(y, levels = 1:ncol(x$fitted.values))
    levels(y) <- cat
  }

  # omit NA values
  if (!is.null(x$na.action)) {
    matching <- matching[-as.vector(x$na.action)]
  }

  match <- seq(min(matching, na.rm = TRUE), max(matching, na.rm = TRUE), length.out = 1000) # matching for curves
  coefs <- matrix(coef(x), ncol = 2)

  # calculation of fitted curves
  df.probs <- data.frame(1, apply(coefs, 1, function(x) exp(x[1] + x[2] * match)))
  df.probs <- df.probs / rowSums(df.probs)
  df.probs <- data.frame(match, df.probs)
  colnames(df.probs) <- c("matching", paste0("P(Y=", cat, ")"))
  df.probs <- tidyr::pivot_longer(df.probs, -matching, names_to = "Category", values_to = "Probability")
  df.probs$Category <- relevel(as.factor(df.probs$Category), paste0("P(Y=", cat[1], ")"))
  colnames(df.probs)[1] <- "Matching"

  # calculation of empirical values
  df.emp <- data.frame(table(y, matching),
    y = prop.table(table(y, matching), 2)
  )[, c(1, 2, 3, 6)]
  df.emp$matching <- as.numeric(paste(df.emp$matching))
  colnames(df.emp) <- c("Category", "Matching", "Count", "Probability")
  df.emp$Category <- paste0("P(Y=", df.emp$Category, ")")
  df.emp$Category <- relevel(as.factor(df.emp$Category), paste0("P(Y=", cat[1], ")"))

  num.cat <- length(levels(df.probs$Category))
  k1 <- num.cat %/% 12
  k2 <- ifelse(num.cat < 12, num.cat, num.cat - 12)
  linetypes <- c(rep(1:12, k1), c(1:12)[1:k2])

  # plotting category probabilities
  g <- ggplot() +
    geom_point(
      data = df.emp,
      aes_string(
        x = "Matching", y = "Probability",
        colour = "Category", fill = "Category", size = "Count"
      ),
      alpha = 0.5, shape = 21
    ) +
    geom_line(
      data = df.probs,
      aes_string(
        x = "Matching", y = "Probability",
        colour = "Category", linetype = "Category"
      ),
      size = 0.8
    ) +
    ylim(0, 1) +
    labs(
      x = matching.name,
      y = "Probability of answer"
    ) +
    scale_linetype_manual(values = linetypes) +
    theme_app() +
    theme(
      legend.box = "horizontal",
      legend.position = c(0.03, 0.97),
      legend.justification = c(0.03, 0.97)
    ) +
    guides(
      size = guide_legend(order = 2),
      colour = guide_legend(order = 1),
      fill = guide_legend(order = 1),
      linetype = guide_legend(order = 1)
    )

  return(g)
}
