library(difNLR)

# loading data
data(GMATtest, GMATkey, package = "difNLR")
data <- GMATtest[, 1:20]
group <- GMATtest[, "group"]
key <- GMATkey

# DDF with multinomial  regression model
(fit <- ddfMLR(
  Data = data, group = group, focal.name = 1, key,
  type = "both", match = "zscore",
  p.adjust.method = "none", purify = FALSE,
  parametrization = "classic"
))

# plot of characteristic curves for item 1
plot(fit, item = 1)

# estimated coefficients for all items with SE
coef(fit, SE = TRUE)
