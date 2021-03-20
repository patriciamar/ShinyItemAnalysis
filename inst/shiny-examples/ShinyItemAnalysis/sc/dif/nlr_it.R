library(difNLR)

# loading data
data(GMAT, package = "difNLR")
data <- GMAT[, 1:20]
group <- GMAT[, "group"]

# generalized logistic regression DIF method
# using 3PL model with the same guessing parameter for both groups
(fit <- difNLR(
  Data = data, group = group, focal.name = 1, model = "3PLcg",
  match = "zscore", type = "all", p.adjust.method = "none", purify = FALSE
))

# plot of characteristic curve of item 1
plot(fit, item = 1)

# estimated coefficients for item 1 with SE
coef(fit, SE = TRUE)[[1]]
