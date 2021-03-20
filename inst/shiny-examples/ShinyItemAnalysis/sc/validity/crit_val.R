library(ggplot2)
library(ShinyItemAnalysis)

# loading data
data(GMAT, package = "difNLR")
data <- GMAT[, 1:20]
score <- rowSums(data) # total score calculation
criterion <- GMAT[, "criterion"] # criterion variable
hist(criterion)
criterionD <- round(criterion) # discrete criterion variable
hist(criterionD)

# number of respondents in each criterion level
size <- as.factor(criterionD)
levels(size) <- table(as.factor(criterionD))
size <- as.numeric(paste(sizeD))
df <- data.frame(score, criterionD, size)

# descriptive plots
### boxplot, for discrete criterion
ggplot(df, aes(y = score, x = as.factor(criterionD), fill = as.factor(criterionD))) +
  geom_boxplot() +
  geom_jitter(shape = 16, position = position_jitter(0.2)) +
  scale_fill_brewer(palette = "Blues") +
  xlab("Criterion group") +
  ylab("Total score") +
  coord_flip() +
  theme_app()

### scatterplot, for continuous criterion
ggplot(df, aes(x = score, y = criterion)) +
  geom_point() +
  ylab("Criterion variable") +
  xlab("Total score") +
  geom_smooth(
    method = lm,
    se = FALSE,
    color = "red"
  ) +
  theme_app()

# test for association between total score and criterion variable
cor.test(criterion, score, method = "pearson", exact = FALSE)
