library(difR)
library(ltm)

# loading data
data(GMAT, package = "difNLR")
data <- GMAT[, 1:20]
group <- GMAT[, "group"]

# 1PL IRT model
(fit1PL <- difLord(
  Data = data, group = group, focal.name = 1, model = "1PL",
  p.adjust.method = "none", purify = FALSE
))

# 2PL IRT model
(fit2PL <- difLord(
  Data = data, group = group, focal.name = 1, model = "2PL",
  p.adjust.method = "none", purify = FALSE
))

# 3PL IRT model with the same guessing for groups
guess <- itemParEst(data, model = "3PL")[, 3]
(fit3PL <- difLord(
  Data = data, group = group, focal.name = 1, model = "3PL",
  c = guess, p.adjust.method = "none", purify = FALSE
))
