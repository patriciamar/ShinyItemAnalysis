library(ggplot2)
library(data.table)

# setting parameters
a <- 1
b <- c(-1.5, -1, -0.5, 0)
theta <- seq(-4, 4, 0.01)

# calculating cumulative probabilities
ccirt <- function(theta, a, b) {
  return(1 / (1 + exp(-a * (theta - b))))
}
df1 <- data.frame(sapply(1:length(b), function(i) ccirt(theta, a, b[i])), theta)
df1 <- melt(df1, id.vars = "theta")

# plotting cumulative probabilities
ggplot(data = df1, aes(x = theta, y = value, col = variable)) +
  geom_line() +
  xlab("Ability") +
  ylab("Cumulative probability") +
  xlim(-4, 4) +
  ylim(0, 1) +
  theme_bw() +
  theme(
    text = element_text(size = 14),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  ggtitle("Cumulative probabilities") +
  scale_color_manual("",
                     values = c("red", "yellow", "green", "blue"),
                     labels = paste0("P(Y >= ", 1:4, ")")
  )

# calculating category probabilities
df2 <- data.frame(1, sapply(
  1:length(b),
  function(i) ccirt(theta, a, b[i])
))
df2 <- data.frame(sapply(
  1:length(b),
  function(i) df2[, i] - df2[, i + 1]
), df2[, ncol(df2)], theta)
df2 <- melt(df2, id.vars = "theta")

# plotting category probabilities
ggplot(data = df2, aes(x = theta, y = value, col = variable)) +
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
                     labels = paste0("P(Y >= ", 0:4, ")")
  )

# calculating expected item score
df3 <- data.frame(1, sapply(
  1:length(b),
  function(i) ccirt(theta, a, b[i])
))
df3 <- data.frame(sapply(
  1:length(b),
  function(i) df3[, i] - df3[, i + 1]
), df3[, ncol(df3)])
df3 <- data.frame(exp = as.matrix(df3) %*% 0:4, theta)

# plotting category probabilities
ggplot(data = df3, aes(x = theta, y = exp)) +
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
