library(difR)

# loading data
data(GMAT, package = "difNLR")
data <- GMAT[, 1:20]
group <- GMAT[, "group"]

# Mantel-Haenszel test
(fit <- difMH(
  Data = data, group = group, focal.name = 1, match = "score",
  p.adjust.method = "none", purify = FALSE
))
