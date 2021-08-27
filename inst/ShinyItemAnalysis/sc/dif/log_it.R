library(difR)
library(ShinyItemAnalysis)

# loading data
data(GMAT, package = "difNLR")
data <- GMAT[, 1:20]
group <- GMAT[, "group"]

# logistic regression DIF detection method
(fit <- difLogistic(
  Data = data, group = group, focal.name = 1, match = "score",
  type = "both", p.adjust.method = "none", purify = FALSE
))

# plot of characteristic curve for item 1
plotDIFLogistic(fit, item = 1, Data = data, group = group)

# estimated coefficients for item 1
fit$logitPar[1, ]
