library(mirt)

# loading data
data(Science, package = "mirt")

# GPCM
fit_gpcm <- mirt(Science, model = 1, itemtype = "gpcm", SE = TRUE)

# PCM (GPCM with a = 1 fixed)
pars <- mirt(Science, 1, itemtype = "gpcm", pars = "values")
pars$value[pars$name == "a1"] <- 1
pars$est[pars$name   == "a1"] <- FALSE
fit_pcm <- mirt(Science, model = 1, itemtype = "gpcm", SE = TRUE, pars = pars)

# RSM (same K for all items required)
fit_rsm <- mirt(Science, model = 1, itemtype = "rsm", SE = TRUE)

# compare fit indices (AIC, BIC, logLik) and likelihood ratio tests
anova(fit_rsm, fit_pcm, fit_gpcm)
