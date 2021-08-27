library(psych)
library(ShinyItemAnalysis)

# loading data
data(GMAT, package = "difNLR")
Data <- GMAT[, 1:20]

# difficulty and discrimination plot
DDplot(Data, discrim = 'ULI', k = 3, l = 1, u = 3)

# Cronbach alpha
psych::alpha(Data)

# traditional item analysis table
ItemAnalysis(Data)
