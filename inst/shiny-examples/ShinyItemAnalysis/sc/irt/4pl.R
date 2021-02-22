library(mirt)
library(ShinyItemAnalysis)

# loading data
data(GMAT, package = "difNLR")

# fitting 4PL model
fit <- mirt(GMAT[, 1:20], model = 1, itemtype = "4PL", SE = TRUE)

# item characteristic curves
plot(fit, type = "trace", facet_items = FALSE)
# test score curve
plot(fit)
# item information curves
plot(fit, type = "infotrace", facet_items = FALSE)
# test information curve
plot(fit, type = "infoSE")

# estimated parameters
coef(fit, simplify = TRUE) # classical intercept-slope parametrization
coef(fit) # including confidence intervals
coef(fit, IRTpars = TRUE, simplify = TRUE) # IRT parametrization
coef(fit, IRTpars = TRUE) # including confidence intervals

# item fit statistics
itemfit(fit)

# factor scores vs standardized total scores
fs <- as.vector(fscores(fit))
sts <- as.vector(scale(rowSums(GMAT[, 1:20])))
plot(fs ~ sts, xlab = "Standardized total score", ylab = "Factor score")
cor(fs, sts)
