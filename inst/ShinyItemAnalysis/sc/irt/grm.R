library(mirt)

# loading data
data(Science, package = "mirt")

# GRM
fit_grm <- mirt(Science, model = 1, itemtype = "graded", SE = TRUE)

# IRT parametrization (a, b_k)
coef(fit_grm, IRTpars = TRUE, simplify = TRUE)$items
# intercept-slope parametrization (a1, d_k)
coef(fit_grm, IRTpars = FALSE, simplify = TRUE)$items

# plots
plot(fit_grm, type = "trace", which.items = 1)       # ICC
plot(fit_grm, type = "infotrace", which.items = 1)   # IIC
plot(fit_grm, type = "infoSE")                       # TIC + SE

fscores(fit_grm, full.scores.SE = TRUE)              # ability estimates
