library(difR)

# loading data
data(GMAT, package = "difNLR")
data <- GMAT[, 1:20]
group <- GMAT[, "group"]

# contingency table for item 1 and score 12
item <- 1
cut <- 12

df <- data.frame(data[, item], group)
colnames(df) <- c("Answer", "Group")
df$Answer <- relevel(factor(df$Answer, labels = c("Incorrect", "Correct")), "Correct")
df$Group <- factor(df$Group, labels = c("Reference Group", "Focal Group"))
score <- rowSums(data) # total score calculation
df <- df[score == 12, ] # responses of those with total score of 12
xtabs(~ Group + Answer, data = df)

# Mantel-Haenszel estimate of OR
(fit <- difMH(
  Data = data, group = group, focal.name = 1, match = "score",
  p.adjust.method = "none", purify = FALSE
))
fit$alphaMH

# D-DIF index calculation
-2.35 * log(fit$alphaMH)
