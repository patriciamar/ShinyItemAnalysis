library(mirt)

# loading data
data(Science, package = "mirt")

# GPCM
fit_gpcm <- mirt(Science, model = 1, itemtype = "gpcm", SE = TRUE)
coef(fit_gpcm, IRTpars = TRUE,  simplify = TRUE)$items   # IRT pars
coef(fit_gpcm, IRTpars = FALSE, simplify = TRUE)$items   # intercept-slope

# PCM (GPCM with a = 1 fixed)
pars <- mirt(Science, 1, itemtype = "gpcm", pars = "values")
pars$value[pars$name == "a1"] <- 1
pars$est[pars$name   == "a1"] <- FALSE
fit_pcm <- mirt(Science, model = 1, itemtype = "gpcm", SE = TRUE, pars = pars)
coef(fit_pcm, IRTpars = TRUE, simplify = TRUE)$items

# RSM (same K for all items required)
fit_rsm <- mirt(Science, model = 1, itemtype = "rsm", SE = TRUE)
coef(fit_rsm, IRTpars = TRUE, simplify = TRUE)$items

# plots (same functions for all three models)
plot(fit_gpcm, type = "trace",     which.items = 1)   # ICC
plot(fit_gpcm, type = "infotrace", which.items = 1)   # IIC
plot(fit_gpcm, type = "infoSE")                       # TIC + SE

fscores(fit_gpcm, full.scores.SE = TRUE)              # ability estimates
