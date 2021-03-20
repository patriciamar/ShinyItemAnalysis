library(ggplot2)
library(msm)

# loading data
data(GMAT, package = "difNLR")
data <- GMAT[, 1:20]
zscore <- scale(rowSums(data)) # standardized total score

# logistic model for item 1
fit <- glm(data[, 1] ~ zscore, family = binomial)

# coefficients
(coef <- c(a = coef(fit)[2], b = -coef(fit)[1] / coef(fit)[2])) # estimates

# SE using delta method
(se <- deltamethod(
  list(~x2, ~ -x1 / x2),
  mean = coef(fit),
  cov = vcov(fit),
  ses = TRUE
))
cbind(coef, se) # estimates and SE

# function for plot
fun <- function(x, a, b) {
  exp(a * (x - b)) / (1 + exp(a * (x - b)))
}

# empirical probabilities calculation
df <- data.frame(
  x = sort(unique(zscore)),
  y = tapply(data[, 1], zscore, mean),
  size = as.numeric(table(zscore))
)

# plot of estimated curve
ggplot(df, aes(x = x, y = y)) +
  geom_point(aes(size = size),
             color = "darkblue",
             fill = "darkblue",
             shape = 21, alpha = 0.5
  ) +
  stat_function(
    fun = fun, geom = "line",
    args = list(
      a = coef[1],
      b = coef[2]
    ),
    size = 1,
    color = "darkblue"
  ) +
  xlab("Standardized total score") +
  ylab("Probability of correct answer") +
  ylim(0, 1) +
  ggtitle("Item 1") +
  theme_app()
