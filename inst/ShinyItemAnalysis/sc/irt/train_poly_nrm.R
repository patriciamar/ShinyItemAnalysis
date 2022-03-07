library(ggplot2)
library(tidyr)

# setting parameters - the baseline-category parameter is constrained to 0
a <- c(0, -1.5, -1, -.5, -.5)
b <- c(0, -3, -2, -1.5, -.5)

# get `b`s except that of the baseline-category
# (we will use them to indicate the intercepts of distractors with the baseline)
vlines <- b[b != 0]

# create ability sequence
thetas <- seq(-4, 4, by = .01)

# get linear predictor
lin_pred <- sapply(seq_along(a), function(i) {
  a[i] * (thetas - b[i])
})

# exponentiate
exponentiated <- exp(lin_pred)

# get category probabilities
cat_probs <- exponentiated / (rowSums(exponentiated))

# set names
colnames(cat_probs) <- c("Correct", paste0("Distractor_", 1:4))

# make data.frame with thetas and categories probabilities
probs <- data.frame(thetas, cat_probs)

probs_long <- pivot_longer(probs, -thetas, names_to = "Response")

# plot category probabilities
ggplot(probs_long, aes(x = thetas, y = value, col = Response)) +
  geom_line(size = 1) +
  geom_vline(xintercept = vlines, col = "grey", linetype = "dashed") +
  labs(x = "Ability", y = "Category probability") +
  coord_cartesian(xlim = range(thetas), ylim = c(0, 1), expand = FALSE) +
  theme_minimal() +
  theme(legend.position = c(1, .5), legend.justification = c(1, .5))

# calculate expected item score
item_score <- data.frame(score = as.matrix(probs) %*% 0:5, thetas)

# plot expected item score
ggplot(item_score, aes(x = thetas, y = score)) +
  geom_line() +
  xlab("Ability") +
  ylab("Expected item score") +
  xlim(-4, 4) +
  ylim(1, 6) +
  theme_minimal() +
  ggtitle("Expected item score")
