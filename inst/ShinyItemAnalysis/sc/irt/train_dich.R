library(ggplot2)
library(data.table)

# parameters
a1 <- 1
b1 <- 0
c1 <- 0
d1 <- 1
a2 <- 2
b2 <- 0.5
c2 <- 0
d2 <- 1

# latent ability
theta <- seq(-4, 4, 0.01)
# latent ability level
theta0 <- 0

# function for IRT characteristic curve
icc_irt <- function(theta, a, b, c, d) {
  return(c + (d - c) / (1 + exp(-a * (theta - b))))
}

# calculation of characteristic curves
df <- data.frame(theta,
                 "icc1" = icc_irt(theta, a1, b1, c1, d1),
                 "icc2" = icc_irt(theta, a2, b2, c2, d2)
)
df <- melt(df, id.vars = "theta")

# plot for characteristic curves
ggplot(df, aes(x = theta, y = value, color = variable)) +
  geom_line() +
  geom_segment(aes(
    y = icc_irt(theta0, a = a1, b = b1, c = c1, d = d1),
    yend = icc_irt(theta0, a = a1, b = b1, c = c1, d = d1),
    x = -4, xend = theta0
  ),
  color = "gray", linetype = "dashed"
  ) +
  geom_segment(aes(
    y = icc_irt(theta0, a = a2, b = b2, c = c2, d = d2),
    yend = icc_irt(theta0, a = a2, b = b2, c = c2, d = d2),
    x = -4, xend = theta0
  ),
  color = "gray", linetype = "dashed"
  ) +
  geom_segment(aes(
    y = 0,
    yend = max(
      icc_irt(theta0, a = a1, b = b1, c = c1, d = d1),
      icc_irt(theta0, a = a2, b = b2, c = c2, d = d2)
    ),
    x = theta0, xend = theta0
  ),
  color = "gray", linetype = "dashed"
  ) +
  xlim(-4, 4) +
  xlab("Ability") +
  ylab("Probability of correct answer") +
  theme_bw() +
  ylim(0, 1) +
  theme(
    axis.line = element_line(colour = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  ggtitle("Item characteristic curve")

# function for IRT information function
iic_irt <- function(theta, a, b, c, d) {
  pi <- c + (d - c) * exp(a * (theta - b)) / (1 + exp(a * (theta - b)))
  return(a^2 * (pi - c)^2 * (d - pi)^2 / (pi * (1 - pi) * (d - c)^2))
}

# calculation of information curves
df <- data.frame(theta,
                 "iic1" = iic_irt(theta, a1, b1, c1, d1),
                 "iic2" = iic_irt(theta, a2, b2, c2, d2)
)
df <- melt(df, id.vars = "theta")

# plot for information curves
ggplot(df, aes(x = theta, y = value, color = variable)) +
  geom_line() +
  xlim(-4, 4) +
  xlab("Ability") +
  ylab("Information") +
  theme_bw() +
  ylim(0, 4) +
  theme(
    axis.line = element_line(colour = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  ggtitle("Item information curve")
