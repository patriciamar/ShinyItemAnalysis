library(difNLR)

# loading data
data(dataMedicalgraded, package = "ShinyItemAnalysis")
data <- dataMedicalgraded[, 1:100]
group <- dataMedicalgraded[, 101]

# DIF with adjacent category logit regression model
(fit <- difORD(
  Data = data, group = group, focal.name = 1, model = "cumulative",
  type = "both", match = "zscore", p.adjust.method = "none", purify = FALSE
))

# plot of characteristic curves for item X2003
plot(fit, item = "X2003")

# estimated coefficients with SE in IRT parametrization for item X2003
coef(fit, SE = TRUE, IRTpars = TRUE, CI = 0)[["X2003"]]
# estimated coefficients with SE in intercept/slope parametrization for item X2003
coef(fit, SE = TRUE, IRTpars = FALSE, CI = 0)[["X2003"]]
