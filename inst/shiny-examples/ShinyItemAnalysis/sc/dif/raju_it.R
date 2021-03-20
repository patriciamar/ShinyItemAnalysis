library(difR)
library(ltm)
library(ShinyItemAnalysis)

# loading data
data(GMAT, package = "difNLR")
data <- GMAT[, 1:20]
group <- GMAT[, "group"]

# 1PL IRT model
(fit1PL <- difRaju(
  Data = data, group = group, focal.name = 1, model = "1PL",
  p.adjust.method = "none", purify = FALSE
))
# estimated coefficients for all items
(coef1PL <- fit1PL$itemParInit)
# plot of characteristic curve of item 1
plotDIFirt(parameters = coef1PL, item = 1, test = "Raju")

# 2PL IRT model
(fit2PL <- difRaju(
  Data = data, group = group, focal.name = 1, model = "2PL",
  p.adjust.method = "none", purify = FALSE
))
# estimated coefficients for all items
(coef2PL <- fit2PL$itemParInit)
# plot of characteristic curve of item 1
plotDIFirt(parameters = coef2PL, item = 1, test = "Raju")

# 3PL IRT model with the same guessing for groups
guess <- itemParEst(data, model = "3PL")[, 3]
(fit3PL <- difRaju(
  Data = data, group = group, focal.name = 1, model = "3PL",
  c = guess, p.adjust.method = "none", purify = FALSE
))
# estimated coefficients for all items
(coef3PL <- fit3PL$itemParInit)
# plot of characteristic curve of item 1
plotDIFirt(parameters = coef3PL, item = 1, test = "Raju")
