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

# estimated parameters in IRT parametrization
coef(fit, SE = TRUE, simplify = TRUE, IRTpars = TRUE, CI = 0)
# estimated parameters in intercept/slope parametrization
coef(fit, SE = TRUE, simplify = TRUE, IRTpars = FALSE, CI = 0)
