library(msm)
library(nnet)
library(ShinyItemAnalysis)

# loading data
data(GMAT, GMATtest, GMATkey, package = "difNLR")

# standardized total score calculation
zscore <- scale(rowSums(GMAT[, 1:20]))

# multinomial model for item 1
fit <- multinom(relevel(GMATtest[, 1], ref = paste(GMATkey[1])) ~ zscore)

# coefficients under intercept/slope parametrization
coef(fit) # estimates
sqrt(diag(vcov(fit))) # SE

# IRT parametrization
# delta method
subst_vcov <- function(vcov, cat) {
  ind <- grep(cat, colnames(vcov))
  vcov[ind, ind]
}
se <- t(sapply(
  rownames(coef(fit)),
  function(.x) {
    vcov_subset <- subst_vcov(vcov(fit), .x)
    msm::deltamethod(
      list(~ -x1 / x2, ~x2),
      mean = coef(fit)[.x, ],
      cov = vcov_subset,
      ses = TRUE
    )
  }
))

# estimates and SE in IRT parametrization
cbind(-coef(fit)[, 1] / coef(fit)[, 2], se[, 1], coef(fit)[, 2], se[, 2])

# plot of estimated category probabilities
plotMultinomial(fit, zscore, matching.name = "Standardized total score")
