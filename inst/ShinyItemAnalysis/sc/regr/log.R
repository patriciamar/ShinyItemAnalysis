library(ggplot2)

# loading data
data(GMAT, package = "difNLR")
data <- GMAT[, 1:20]
score <- rowSums(data) # total score

# logistic model for item 1
fit <- glm(data[, 1] ~ score, family = binomial)

# coefficients
coef(fit) # estimates
sqrt(diag(vcov(fit))) # SE
summary(fit)$coefficients[, 1:2] # estimates and SE

# function for plot
fun <- function(x, b0, b1) {
  exp(b0 + b1 * x) / (1 + exp(b0 + b1 * x))
}

# empirical probabilities calculation
df <- data.frame(
  x = sort(unique(score)),
  y = tapply(data[, 1], score, mean),
  size = as.numeric(table(score))
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
      b0 = coef(fit)[1],
      b1 = coef(fit)[2]
    ),
    size = 1,
    color = "darkblue"
  ) +
  xlab("Total score") +
  ylab("Probability of correct answer") +
  ylim(0, 1) +
  ggtitle("Item 1") +
  theme_app()
