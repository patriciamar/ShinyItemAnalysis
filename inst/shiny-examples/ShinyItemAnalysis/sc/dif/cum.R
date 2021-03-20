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
