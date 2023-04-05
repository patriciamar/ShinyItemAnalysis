library(ltm)
library(mirt)
library(ShinyItemAnalysis)

# loading data
data(GMAT, package = "difNLR")

# obtaining details on values of model parameters
mirt(GMAT[, 1:20], model = 1, itemtype = "2PL", pars = "values")
# a1 parameter numbers (parnum) are 1, 5, 9,...

# fitting 1PL model as 2PL with slope a1 parameters constrained to be equal
fit <- mirt(GMAT[, 1:20],
  model = 1, itemtype = "2PL",
  constrain = list((1:20) * 4 - 3), SE = TRUE
)

# item response curves for item 1
itemplot(fit, 1)
itemplot(fit, 1, CE = TRUE)

# item information curves
itemplot(fit, 1, type = "info")
itemplot(fit, 1, type = "infoSE")
itemplot(fit, 1, type = "info", CE = TRUE)

# estimated parameters
coef(fit, simplify = TRUE)$items[1, ] # classical intercept-slope parametrization
coef(fit, printSE = TRUE)$Item1 # classical intercept-slope parametrization with SE
coef(fit)$Item1 # classical intercept-slope parametrization with CI

coef(fit, IRTpars = TRUE, simplify = TRUE)$items[1, ] # IRT parametrization
coef(fit, IRTpars = TRUE, printSE = TRUE)$Item1 # IRT parametrization with SE
coef(fit, IRTpars = TRUE)$Item1 # IRT parametrization with CI
