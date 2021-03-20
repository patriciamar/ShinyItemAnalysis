library(psychometric)

# loading data
data(HCI, package = "ShinyItemAnalysis")
data <- HCI[, 1:20]

# Cronbach's alpha with confidence interval
a <- psychometric::alpha(data)
psychometric::alpha.CI(a, N = nrow(data), k = ncol(data), level = 0.95)
