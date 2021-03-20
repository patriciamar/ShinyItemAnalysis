library(msm)
library(ShinyItemAnalysis)
library(VGAM)

# loading data
data(Science, package = "mirt")

# standardized total score calculation
zscore <- scale(rowSums(Science))
Science[, 1] <- factor(
  Science[, 1], levels = sort(unique(Science[, 1])), ordered = TRUE
)

# cumulative logit model for item 1
fit <- vglm(Science[, 1] ~ zscore,
            family = cumulative(reverse = TRUE, parallel = TRUE))

# coefficients under intercept/slope parametrization
coef(fit) # estimates
sqrt(diag(vcov(fit))) # SE

# IRT parametrization
# delta method
num_par <- length(coef(fit))
formula <- append(
  paste0("~ x", num_par),
  as.list(paste0("~ -x", 1:(num_par - 1), "/", "x", num_par))
)
formula <- lapply(formula, as.formula)
se <- deltamethod(
  formula,
  mean = coef(fit),
  cov = vcov(fit),
  ses = TRUE
)
# estimates and SE in IRT parametrization
cbind(c(coef(fit)[num_par], -coef(fit)[-num_par] / coef(fit)[num_par]), se)

# plot of estimated cumulative probabilities
plotCumulative(fit, type = "cumulative", matching.name = "Standardized total  score")
# plot of estimated category probabilities
plotCumulative(fit, type = "category", matching.name = "Standardized total score")
