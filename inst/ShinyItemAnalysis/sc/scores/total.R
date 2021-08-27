library(ggplot2)
library(psych)
library(ShinyItemAnalysis)

# loading data
data(GMAT, package = "difNLR")
data <- GMAT[, 1:20]

# total score calculation
score <- rowSums(data)

# summary of total score
tab <- describe(score)[, c("n", "min", "max", "mean", "median", "sd", "skew", "kurtosis")]
tab$kurtosis <- tab$kurtosis + 3
tab

# histogram
ggplot(df, aes(score)) +
  geom_histogram(binwidth = 1, col = "black") +
  xlab("Total score") +
  ylab("Number of respondents") +
  theme_app()

# colors by cut-score
cut <- median(score) # cut-score
color <- c(rep("red", cut - min(score)), "gray", rep("blue", max(score) - cut))
df <- data.frame(score)

# histogram
ggplot(df, aes(score)) +
  geom_histogram(binwidth = 1, fill = color, col = "black") +
  xlab("Total score") +
  ylab("Number of respondents") +
  theme_app()

