library(difNLR)

# loading data
data(GMATtest, GMATkey, package = "difNLR")
data <- GMATtest[, 1:20]
group <- GMATtest[, "group"]
key <- GMATkey

# DDF with multinomial regression model
(fit <- ddfMLR(
  Data = data, group = group, focal.name = 1, key,
  type = "both", match = "zscore",
  p.adjust.method = "none", purify = FALSE
))

# plot of characteristic curves for item 1
plot(fit, item = 1)

# estimated coefficients with SE in IRT parametrization for item 1
coef(fit, SE = TRUE, IRTpars = TRUE, CI = 0)[[1]]
# estimated coefficients with SE in intercept/slope parametrization for item 1
coef(fit, SE = TRUE, IRTpars = FALSE, CI = 0)[[1]]
