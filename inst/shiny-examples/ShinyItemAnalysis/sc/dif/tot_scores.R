library(ggplot2)
library(moments)

# loading data
data(GMAT, package = "difNLR")
data <- GMAT[, 1:20]
group <- GMAT[, "group"]

# total score calculation wrt group
score <- rowSums(data)
score0 <- score[group == 0] # reference group
score1 <- score[group == 1] # focal group

# Summary of total score
rbind(
  c(
    length(score0), min(score0), max(score0), mean(score0), median(score0),
    sd(score0), skewness(score0), kurtosis(score0)
  ),
  c(
    length(score1), min(score1), max(score1), mean(score1), median(score1),
    sd(score1), skewness(score1), kurtosis(score1)
  )
)

df <- data.frame(score, group = as.factor(group))

# histogram of total scores wrt group
ggplot(data = df, aes(x = score, fill = group, col = group)) +
  geom_histogram(binwidth = 1, position = "dodge2", alpha = 0.75) +
  xlab("Total score") +
  ylab("Number of respondents") +
  scale_fill_manual(
    values = c("dodgerblue2", "goldenrod2"), labels = c("Reference", "Focal")
  ) +
  scale_colour_manual(
    values = c("dodgerblue2", "goldenrod2"), labels = c("Reference", "Focal")
  ) +
  theme_app() +
  theme(legend.position = "left")

# t-test to compare total scores
t.test(score0, score1)
