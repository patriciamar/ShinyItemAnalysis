#' Function for plotting cumulative and category probabilities of cumulative logistic regression model
#'
#' @aliases plotCumulative
#'
#' @description Function for plotting cumulative and category probabilities function estimated by \code{vglm()} from \code{VGAM} package
#'
#' @param x object of class \code{vglm}
#' @param type character: type of plot to be displayed. Options are \code{"cumulative"} (default) for cumulative probabilities
#' and \code{"category"} for category probabilities.
#' @param matching.name character: name of matching criterion used for estimation in \code{x}.
#'
#' @usage plotCumulative(x, type = "cumulative", matching.name = "matching")
#'
#' @author
#' #' Tomas Jurica \cr
#' Faculty of Mathematics and Physics, Charles University \cr
#' tomasjurica1997@gmail.com \cr
#'
#' Adela Hladka \cr
#' Institute of Computer Science, The Czech Academy of Sciences \cr
#' Faculty of Mathematics and Physics, Charles University \cr
#' drabinova@cs.cas.cz \cr
#'
#' Patricia Martinkova \cr
#' Institute of Computer Science, The Czech Academy of Sciences \cr
#' martinkova@cs.cas.cz \cr
#'
#' @seealso
#' \code{\link[VGAM]{vglm}}
#'
#' @examples
#' \dontrun{
#' # loading data
#' data <- dataMedicalgraded[, 1:100]
#'
#' # total score calculation
#' score <- apply(data, 1, sum)
#' data[, 1] <- ordered(factor(data[, 1], levels = 0:max(data[, 1])))
#'
#' # cummulative logistic model for item 1
#' fit <- vglm(data[, 1] ~ score, family = cumulative(reverse = TRUE, parallel = TRUE))
#' # coefficients for item 1
#' coefs <- coef(fit)
#'
#' plotCumulative(fit, type = "cumulative", matching.name = "Total score")
#' plotCumulative(fit, type = "category", matching.name = "Total score")
#' }
#'@export


plotCumulative <- function(x, type = "cumulative", matching.name = "matching"){

  # extracting data from vglm object
  y <- x@y %*% as.numeric(paste(colnames(x@y))) # responses
  cat <- as.numeric(paste(colnames(x@y)))       # all categories
  num.cat <- length(cat)                        # number of all categories
  y <- factor(y, levels = cat)                  # releveling
  matching <- x@x[, 2]                          # matching
  match <- seq(min(matching, na.rm = T), max(matching, na.rm = T), 0.01)

  coefs <- coef(x)                          # extracting coefficients
  cat.obs <- names(which(table(y) > 0)[-1]) # observed categories = categories with at least one observation
  num.cat.obs <- length(coefs) - 1          # number of categories with at least one observation

  # cumulative probabilities
  df.probs.cum <- matrix(1, nrow = length(match), ncol = num.cat) # initial values of cumprob for all categories
  colnames(df.probs.cum) <- paste(cat)

  # calculation of cumulative probabilities based on formula P(Y >= k) = exp(b0 + b1*x)/(1 + exp(b0 + b1*x))
  df.probs.cum[, cat.obs] <- sapply(1:num.cat.obs, function(i) exp(coefs[i] + coefs[num.cat.obs + 1] * match)/(1 + exp(coefs[i] + coefs[num.cat.obs + 1] * match)))
  # if column between non-ones valued columns consist of ones, it has to be changed to value on the left side
  need.correction <- which(sapply(2:num.cat, function(i) (all(df.probs.cum[, i] == 1) & all(df.probs.cum[, i - 1] != 1))))
  df.probs.cum[, need.correction + 1] <- df.probs.cum[, need.correction]

  # category probabilities
  df.probs.cat <- data.frame(sapply(1:(num.cat - 1), function(i) df.probs.cum[, i] - df.probs.cum[, i + 1]),
                             df.probs.cum[, num.cat])


  # melting data
  df.probs.cum <- data.frame(match, df.probs.cum)
  colnames(df.probs.cum) <- c("matching", paste0("P>=", cat))
  df.probs.cum <- melt(df.probs.cum, id.vars = "matching", variable.name = "category", value.name = "probability")

  df.probs.cat <- data.frame(match, df.probs.cat)
  colnames(df.probs.cat) <- c("matching", paste0("P=", cat))
  df.probs.cat <- melt(df.probs.cat, id.vars = "matching", variable.name = "category", value.name = "probability")


  # empirical category values
  df.emp.cat <- data.frame(table(y, matching),
                           y = prop.table(table(y, matching), 2))[, c(1, 2, 3, 6)]
  df.emp.cat$matching <- as.numeric(paste(df.emp.cat$matching))
  colnames(df.emp.cat) <- c("category", "matching", "size", "probability")
  df.emp.cat$category <- as.factor(df.emp.cat$category)
  levels(df.emp.cat$category) <- paste0("P=", levels(df.emp.cat$category))


  # empirical cumulative values
  df.emp.cum.count <- as.data.frame.matrix(table(matching, y))
  df.emp.cum.count <- t(apply(df.emp.cum.count, 1, function(x) sum(x) - cumsum(x) + x))
  df.emp.cum.count <- data.frame(as.numeric(paste(rownames(df.emp.cum.count))),
                                 df.emp.cum.count)
  colnames(df.emp.cum.count) <- c("matching", paste0("P>=", cat))
  df.emp.cum.count <- melt(df.emp.cum.count, id.vars = "matching", variable.name = "category", value.name = "size")

  df.emp.cum.prob <- as.data.frame.matrix(prop.table(table(matching, y), 1))
  df.emp.cum.prob <- t(apply(df.emp.cum.prob, 1, function(x) sum(x) - cumsum(x) + x))
  df.emp.cum.prob <- data.frame(as.numeric(paste(rownames(df.emp.cum.prob))),
                                df.emp.cum.prob)
  colnames(df.emp.cum.prob) <- c("matching", paste0("P>=", cat))
  df.emp.cum.prob <- melt(df.emp.cum.prob, id.vars = "matching", variable.name = "category", value.name = "probability")

  df.emp.cum <- merge(df.emp.cum.count, df.emp.cum.prob, by = c("matching", "category"))

  # colours
  gg_color_hue <- function(n) {
    hues = seq(15, 375, length = n + 1)
    hcl(h = hues, l = 65, c = 100)[1:n]
  }
  cols <- c("black", gg_color_hue(num.cat - 1))

  if (type == "cumulative") {

    ltys <- as.numeric(cat.obs)
    if (any(cat == 0)){
      ltys <- ltys + 1
      cols <- cols[as.numeric(cat.obs) + 1]
    } else {
      cols <- cols[as.numeric(cat.obs)]
    }

    df.emp.cum <- df.emp.cum[df.emp.cum$category %in% paste0("P>=", cat.obs), ]
    df.probs.cum <- df.probs.cum[df.probs.cum$category %in% paste0("P>=", cat.obs), ]

    g <- ggplot() +
      geom_point(data = df.emp.cum,
                 aes_string(x = "matching", y = "probability", group = "category",
                            size = "size", colour = "category", fill = "category"),
                 shape = 21, alpha = 0.5) +
      geom_line(data = df.probs.cum,
                aes_string(x = "matching", y = "probability", group = "category",
                           col = "category", linetype = "category"),
                size = 1) +
      scale_fill_manual(values = cols) +
      scale_colour_manual(values = cols) +
      scale_linetype_manual(values = ltys) +
      xlab(matching.name) +
      ylab("Cumulative probability") +
      ylim(0, 1) +
      xlim(min(matching), max(matching)) +
      theme_app() +
      theme(legend.position = c(0.97, 0.03),
            legend.justification = c(0.97, 0.03),
            legend.box = "horizontal") +
      guides(size = guide_legend(order = 1),
             colour = guide_legend(order = 2),
             fill = guide_legend(order = 2),
             linetype = guide_legend(order = 2))
  } else {
    ltys <- as.numeric(cat)
    if (any(cat == 0)){
      ltys <- ltys + 1
    }

    g <- ggplot() +
      geom_point(data = df.emp.cat,
                 aes_string(x = "matching", y = "probability", group = "category",
                            size = "size", col = "category", fill = "category"),
                 shape = 21, alpha = 0.5) +
      geom_line(data = df.probs.cat,
                aes_string(x = "matching", y = "probability", group = "category",
                           col = "category", linetype = "category"),
                size = 1) +
      scale_fill_manual(values = cols) +
      scale_colour_manual(values = cols) +
      scale_linetype_manual(values = ltys) +
      xlab(matching.name) +
      ylab("Category probability") +
      ylim(0, 1) +
      xlim(min(matching), max(matching)) +
      theme_app() +
      theme(legend.box = "horizontal",
            legend.position = c(0.03, 0.97),
            legend.justification = c(0.03, 0.97)) +
      guides(size = guide_legend(order = 1),
             colour = guide_legend(order = 2),
             fill = guide_legend(order = 2),
             linetype = guide_legend(order = 2))
  }
  return(g)
}
