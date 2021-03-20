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
  p.adjust.method = "none", purify = FALSE,
  parametrization = "classic"
))
