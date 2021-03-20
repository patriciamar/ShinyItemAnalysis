library(difNLR)
library(ggplot2)

# loading data
data(GMAT, package = "difNLR")
data <- GMAT[, 1:20]
zscore <- scale(rowSums(data)) # standardized total score

# NLR 4P model for item 1
fun <- function(x, a, b, c, d) {
  c + (d - c) * exp(a * (x - b)) / (1 + exp(a * (x - b)))
}

fit <- nls(data[, 1] ~ fun(zscore, a, b, c, d),
           algorithm = "port",
           start = startNLR(
             data, GMAT[, "group"],
             model = "4PLcgdg",
             parameterization = "classic"
           )[[1]][1:4],
           lower = c(-Inf, -Inf, 0, 0),
           upper = c(Inf, Inf, 1, 1)
)

# coefficients
coef(fit) # estimates
sqrt(diag(vcov(fit))) # SE
summary(fit)$coefficients[, 1:2] # estimates and SE

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
      a = coef(fit)[1],
      b = coef(fit)[2],
      c = coef(fit)[3],
      d = coef(fit)[4]
    ),
    size = 1,
    color = "darkblue"
  ) +
  xlab("Standardized total score") +
  ylab("Probability of correct answer") +
  ylim(0, 1) +
  ggtitle("Item 1") +
  theme_app()
