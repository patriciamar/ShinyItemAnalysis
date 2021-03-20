library(difNLR)

# loading data
data(dataMedicalgraded, package = "ShinyItemAnalysis")
data <- dataMedicalgraded[, 1:100]
group <- dataMedicalgraded[, 101]

# DIF with cumulative logit regression model
(fit <- difORD(
  Data = data, group = group, focal.name = 1, model = "cumulative",
  type = "both", match = "zscore", p.adjust.method = "none", purify = FALSE,
  parametrization = "classic"
))

# plot of cumulative probabilities for item X2003
plot(fit, item = "X2003", plot.type = "cumulative")

# plot of category probabilities for item X2003
plot(fit, item = "X2003", plot.type = "category")

# estimated coefficients for all items with SE
coef(fit, SE = TRUE)
