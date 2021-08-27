library(ggplot2)
library(data.table)

# setting parameters
a <- 1
d <- c(-1.5, -1, -0.5, 0)
theta <- seq(-4, 4, 0.01)

# calculating category probabilities
ccgpcm <- function(theta, a, d) {
  a * (theta - d)
}
df <- sapply(1:length(d), function(i) ccgpcm(theta, a, d[i]))
pk <- sapply(1:ncol(df), function(k) apply(as.data.frame(df[, 1:k]), 1, sum))
pk <- cbind(0, pk)
pk <- exp(pk)
denom <- apply(pk, 1, sum)
df <- apply(pk, 2, function(x) x / denom)
df1 <- melt(data.frame(df, theta), id.vars = "theta")

# plotting category probabilities
ggplot(data = df1, aes(x = theta, y = value, col = variable)) +
  geom_line() +
  xlab("Ability") +
  ylab("Category probability") +
  xlim(-4, 4) +
  ylim(0, 1) +
  theme_bw() +
  theme(
    text = element_text(size = 14),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  ggtitle("Category probabilities") +
  scale_color_manual("",
                     values = c("black", "red", "yellow", "green", "blue"),
                     labels = paste0("P(Y = ", 0:4, ")")
  )

# calculating expected item score
df2 <- data.frame(exp = as.matrix(df) %*% 0:4, theta)
# plotting expected item score
ggplot(data = df2, aes(x = theta, y = exp)) +
  geom_line() +
  xlab("Ability") +
  ylab("Expected item score") +
  xlim(-4, 4) +
  ylim(0, 4) +
  theme_bw() +
  theme(
    text = element_text(size = 14),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  ggtitle("Expected item score")
