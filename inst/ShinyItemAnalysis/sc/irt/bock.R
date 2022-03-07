library(ShinyItemAnalysis)
library(mirt)

# get nominal, numeric-coded and scored HCI dataframes
# nominal
data_nominal <- HCItest[, 1:20]

# nominal coded as numeric
data_nominal_numeric <- HCItest[, 1:20]
data_nominal_numeric[] <- sapply(data_nominal_numeric, as.numeric)

# scored data
data_scored <- HCI[, 1:20]


# BLIS and BLIRT parametrizations -----------------------------------------

# fit the model on nominal HCI data (note the key of correct answers)
fitted_blis_blirt <- fit_blis(data_nominal, key = HCIkey, SE = TRUE)

# item response curves
plot(fitted_blis_blirt, type = "trace")

# item information curves
plot(fitted_blis_blirt, type = "infotrace", facet_items = FALSE)

# test information curve
plot(fitted_blis_blirt, type = "infoSE")

# show estimated parameters and their SEs
coef(fitted_blis_blirt, printSE = TRUE)

# show estimated parameters in IRT parametrization (i.e., BLIRT model)
coef(fitted_blis_blirt, printSE = TRUE, IRTpars = TRUE)


# obtain factor scores (from fitted model) and standardized total scores
factor_scores <- fscores(fitted_blis_blirt)
standard_scores <- scale(rowSums(data_scored))

# show both scores in a scatterplot
plot(factor_scores ~ standard_scores,
  xlab = "Standardized total score", ylab = "Factor score"
)

# Pearson correlation of both scores
cor(factor_scores, standard_scores)


# Bock and Thissen et al. parametrizations --------------------------------

fitted_bock_thissen <- mirt(data_nominal_numeric,
  model = 1, itemtype = "nominal", SE = TRUE
)

# item response curves
plot(fitted_bock_thissen, type = "trace")

# item information curves
plot(fitted_bock_thissen, type = "infotrace", facet_items = FALSE)

# test information curve
plot(fitted_bock_thissen, type = "infoSE")

# show estimated parameters and their SEs (this is Thissen et al. parametrization)
coef(fitted_bock_thissen, printSE = TRUE)

# show estimated parameters in IRT parametrization (i.e., Bock parametrization)
coef(fitted_bock_thissen, printSE = TRUE, IRTpars = TRUE)


# obtain factor scores (from fitted model)
factor_scores <- fscores(fitted_bock_thissen)

# show both scores in a scatterplot
plot(factor_scores ~ standard_scores,
  xlab = "Standardized total score", ylab = "Factor score"
)

# Pearson correlation of both scores
cor(factor_scores, standard_scores)
