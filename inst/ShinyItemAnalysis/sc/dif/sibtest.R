library(difR)

# loading data
data(GMAT, package = "difNLR")
data <- GMAT[, 1:20]
group <- GMAT[, "group"]

# SIBTEST (uniform DIF)
(fit_udif <- difSIBTEST(
  Data = data, group = group, focal.name = 1, type = "udif",
  p.adjust.method = "none", purify = FALSE
))

# Crossing-SIBTEST (non-uniform DIF)
(fit_nudif <- difSIBTEST(
  Data = data, group = group, focal.name = 1, type = "nudif",
  p.adjust.method = "none", purify = FALSE
))
