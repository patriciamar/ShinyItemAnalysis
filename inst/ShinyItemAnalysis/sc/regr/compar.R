library(difNLR)

# loading data
data(GMAT, package = "difNLR")
Data <- GMAT[, 1:20]
zscore <- scale(rowSums(Data)) # standardized total score

# function for fitting models
fun <- function(x, a, b, c, d) {
  c + (d - c) * exp(a * (x - b)) / (1 + exp(a * (x - b)))
}

# starting values for item 1
start <- startNLR(
  Data, GMAT[, "group"], model = "4PLcgdg",
  parameterization = "classic"
)[[1]][, 1:4]

# 2PL model for item 1
fit2PL <- nls(Data[, 1] ~ fun(zscore, a, b, c = 0, d = 1),
              algorithm = "port",
              start = start[1:2]
)
# NLR 3P model for item 1
fit3PL <- nls(Data[, 1] ~ fun(zscore, a, b, c, d = 1),
              algorithm = "port",
              start = start[1:3],
              lower = c(-Inf, -Inf, 0),
              upper = c(Inf, Inf, 1)
)
# NLR 4P model for item 1
fit4PL <- nls(Data[, 1] ~ fun(zscore, a, b, c, d),
              algorithm = "port",
              start = start,
              lower = c(-Inf, -Inf, 0, 0),
              upper = c(Inf, Inf, 1, 1)
)

# comparison
### AIC
AIC(fit2PL)
AIC(fit3PL)
AIC(fit4PL)
### BIC
BIC(fit2PL)
BIC(fit3PL)
BIC(fit4PL)
