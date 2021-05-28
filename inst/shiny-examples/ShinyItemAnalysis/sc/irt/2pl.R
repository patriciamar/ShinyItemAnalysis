library(ltm)
library(mirt)
library(ShinyItemAnalysis)

# loading data
data(GMAT, package = "difNLR")

# fitting 2PL model
fit <- mirt(GMAT[, 1:20], model = 1, itemtype = "2PL", SE = TRUE)

# item characteristic curves
plot(fit, type = "trace", facet_items = FALSE)
# test score curve
plot(fit)
# item information curves
plot(fit, type = "infotrace", facet_items = FALSE)
# test information curve
plot(fit, type = "infoSE")
plot(fit, type = "info")

# estimated parameters
coef(fit, simplify = TRUE) # classical intercept-slope parametrization
coef(fit) # including confidence intervals
coef(fit, printSE = TRUE) # including SE

coef(fit, IRTpars = TRUE, simplify = TRUE) # IRT parametrization
coef(fit, IRTpars = TRUE) # including confidence intervals
coef(fit, IRTpars = TRUE, printSE = TRUE) # including SE

# item fit statistics
itemfit(fit)

# factor scores vs standardized total scores
fs <- as.vector(fscores(fit))
sts <- as.vector(scale(rowSums(GMAT[, 1:20])))
plot(fs ~ sts, xlab = "Standardized total score", ylab = "Factor score")
cor(fs, sts)

# you can also use the ltm package
library(ltm)

# fitting 2PL model
fit <- ltm(GMAT[, 1:20] ~ z1, IRT.param = TRUE)

# item characteristic curves
plot(fit)
# item information curves
plot(fit, type = "IIC")
# test information curve
plot(fit, items = 0, type = "IIC")

# estimated parameters
coef(fit)

# factor scores vs standardized total scores
df1 <- ltm::factor.scores(fit, return.MIvalues = TRUE)$score.dat
FS <- as.vector(df1[, "z1"])
df2 <- df1
df2$Obs <- df2$Exp <- df2$z1 <- df2$se.z1 <- NULL
STS <- as.vector(scale(rowSums(df2[, 1:20])))
df <- data.frame(FS, STS)
plot(FS ~ STS, data = df, xlab = "Standardized total score", ylab = "Factor score")
cor(FS, STS)
