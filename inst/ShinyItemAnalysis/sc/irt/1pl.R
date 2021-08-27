library(ltm)
library(mirt)
library(msm)
library(ShinyItemAnalysis)

# loading data
data(GMAT, package = "difNLR")

# fitting 1PL model
fit <- mirt(GMAT[, 1:20],
            model = 1, itemtype = "2PL",
            constrain = list((1:20) + seq(0, (20 - 1) * 3, 3)), SE = TRUE
)

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

# for item 1
coef(fit, IRTpars = TRUE, printSE = TRUE)$Item1 # including SE

# delta method by hand for item 1
coef_is <- coef(fit)[[1]][1, 1:2]
vcov_is <- matrix(vcov(fit)[1:2, 1:2], ncol = 2, nrow = 2,
                  dimnames = list(c("a1", "d"), c("a1", "d")))
# estimates
c(coef_is[1], -coef_is[2] / coef_is[1])
# standard errors
deltamethod(
  list( ~ x1, ~ -x2/x1),
  mean = coef_is,
  cov = vcov_is,
  ses = TRUE
)

# item fit statistics
itemfit(fit)

# factor scores vs standardized total scores
fs <- as.vector(fscores(fit))
sts <- as.vector(scale(rowSums(GMAT[, 1:20])))
plot(fs ~ sts, xlab = "Standardized total score", ylab = "Factor score")
cor(fs, sts)

# Wright map
b <- coef(fit, IRTpars = TRUE, simplify = TRUE)$items[, "b"]
ggWrightMap(fs, b)

# you can also use the ltm package
library(ltm)

# fitting 1PL model
fit <- rasch(GMAT[, 1:20])

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
