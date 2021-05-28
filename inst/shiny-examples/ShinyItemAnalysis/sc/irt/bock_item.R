library(mirt)

# loading data
data(HCItest, HCI, package = "ShinyItemAnalysis")
HCInumeric <- HCItest[, 1:20]
HCInumeric[] <- sapply(HCInumeric, as.numeric)

# model
fit <- mirt(HCInumeric, model = 1, itemtype = "nominal", SE = TRUE)

# item response curves for item 1
itemplot(fit, 1)
itemplot(fit, 1, CE = TRUE)

# item information curves
itemplot(fit, 1, type = "info")
itemplot(fit, 1, type = "infoSE")
itemplot(fit, 1, type = "info", CE = TRUE)

# estimated parameters
coef(fit, simplify = TRUE)$items[1,] # classical intercept-slope parametrization
coef(fit, printSE = TRUE)$`Item 1` # classical intercept-slope parametrization with SE

coef(fit, IRTpars = TRUE, simplify = TRUE)$items[1,] # IRT parametrization
coef(fit, IRTpars = TRUE, printSE = TRUE)$`Item 1`  # IRT parametrization, no SE printed

