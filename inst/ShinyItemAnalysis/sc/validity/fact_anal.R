library(psych)
library(ggplot2)

# loading data
data(HCI, package = "ShinyItemAnalysis")
data <- HCI[, 1:20]

# scree plot, parallel analysis
(fa_paral <- fa_parallel(data))
plot(fa_paral)
as.data.frame(fa_paral)

# EFA for 1, 2, and 3 factors
(FA1 <- psych::fa(data, nfactors = 1))
(FA2 <- psych::fa(data, nfactors = 2))
(FA3 <- psych::fa(data, nfactors = 3))

# Model fit for different number of factors
VSS(data)

# Path diagrams
fa.diagram(FA1)
fa.diagram(FA2)
fa.diagram(FA3)

# Higher order factor solution
(om.h <- omega(data, sl = FALSE))

