#' Function for plotting category probabilities of multinomial log-linear regression model
#'
#' @aliases plotMultinomial
#'
#' @description Plots category probabilities functions estimated by \code{multinom()} from \code{nnet} package.
#'
#' @param x object of class \code{multinom}
#' @param matching numeric: vector of matching criterion used for estimation in \code{x}.
#' @param matching.name character: name of matching criterion used for estimation in \code{x}.
#'
#' @usage plotMultinomial(x, matching, matching.name = "matching")
#'
#' @author
#' Adela Hladka \cr
#' Institute of Computer Science, The Czech Academy of Sciences \cr
#' Faculty of Mathematics and Physics, Charles University \cr
#' hladka@cs.cas.cz \cr
#'
#' Tomas Jurica \cr
#' Faculty of Mathematics and Physics, Charles University \cr
#'
#' Patricia Martinkova \cr
#' Institute of Computer Science, The Czech Academy of Sciences \cr
#' martinkova@cs.cas.cz \cr
#'
#' @seealso
#' \code{\link[nnet]{multinom}}
#'
#' @examples
#' \dontrun{
#' # loading data
#' data(GMAT, GMATtest, GMATkey, package = "difNLR")
#'
#' matching <- scale(apply(GMAT[, 1:20] , 1, sum)) # Z-score
#' data <- GMATtest[, 1:20]
#' key <- GMATkey
#'
#' # multinomial model for item 1
#' fit <- nnet::multinom(relevel(data[, 1], ref = paste(key[1])) ~ matching)
#'
#' # plotting category probabilities
#' plotMultinomial(fit, matching, matching.name = "Z-score")
#' }
#' @export

plotMultinomial <- function(x, matching, matching.name = "matching"){
  # extracting data
  cat <- colnames(x$fitted.values)
  if (is.null(cat)){
    y <- (x$fitted.values + x$residuals) %*% 1:ncol(x$fitted.values)
    y <- factor(y, levels = 0:1)
    cat <- x$lev
    levels(y) <- cat
  } else {
    y <- (x$fitted.values + x$residuals) %*% 1:ncol(x$fitted.values)
    y <- factor(y, levels = 1:ncol(x$fitted.values))
    levels(y) <- cat
  }

  match <- seq(min(matching, na.rm = T), max(matching, na.rm = T), length.out = 300) # matching for curves

  coefs <- matrix(coef(x), ncol = 2)

  # calculation of fitted curves
  df.probs <- data.frame(1, apply(coefs, 1, function(x) exp(x[1] + x[2] * match)))
  df.probs <- df.probs/apply(df.probs, 1, sum)
  df.probs <- data.frame(match, df.probs)
  colnames(df.probs) <- c("matching", paste0("P=", cat))
  df.probs <- melt(df.probs, id.vars = "matching", variable.name = "category", value.name = "probability")

  # calculation of empirical values
  df.emp <- data.frame(table(y, matching),
                       y = prop.table(table(y, matching), 2))[, c(1, 2, 3, 6)]
  df.emp$matching <- as.numeric(paste(df.emp$matching))
  colnames(df.emp) <- c("category", "matching", "size", "probability")
  df.emp$category <- paste0("P=", df.emp$category)
  df.emp$category <- factor(df.emp$category, levels = levels(df.probs$category))

  # plotting category probabilities
  g <- ggplot() +
    geom_point(data = df.emp,
               aes_string(x = "matching", y = "probability",
                          colour = "category", fill = "category", size = "size"),
               alpha = 0.5, shape = 21) +
    geom_line(data = df.probs,
              aes_string(x = "matching", y = "probability",
                         colour = "category", linetype = "category"),
              size = 1) +

    ylim(0, 1) +
    labs(x = matching.name,
         y = "Probability of answer") +
    theme_app() +
    theme(legend.box = "horizontal",
          legend.position = c(0.03, 0.97),
          legend.justification = c(0.03, 0.97)) +
    guides(size = guide_legend(order = 1),
           colour = guide_legend(order = 2),
           fill = guide_legend(order = 2),
           linetype = guide_legend(order = 2))

  return(g)
}
