library(mirt)

# loading data
data(HCItest, HCI, package = "ShinyItemAnalysis")
HCInumeric <- HCItest[, 1:20]
HCInumeric[] <- sapply(HCInumeric, as.numeric)

# model
fit <- mirt(HCInumeric, model = 1, itemtype = "nominal")

# item response curves
plot(fit, type = "trace")
# item information curves
plot(fit, type = "infotrace", facet_items = FALSE)
# test information curve
plot(fit, type = "infoSE")

# estimated parameters
coef(fit, simplify = TRUE) # classical intercept-slope parametrization
coef(fit, IRTpars = TRUE, simplify = TRUE) # IRT parametrization

# factor scores vs standardized total scores
fs <- as.vector(fscores(fit))
sts <- as.vector(scale(rowSums(HCI[, 1:20])))
plot(fs ~ sts, xlab = "Standardized total score", ylab = "Factor score")
cor(fs, sts)
