library(psych)

# loading data
data(HCI, package = "ShinyItemAnalysis")

# first-second half split
df1 <- HCI[, 1:10]
df2 <- HCI[, 11:20]
# total score calculation
ts1 <- rowSums(df1)
ts2 <- rowSums(df2)
# correlation
cor.x <- cor(ts1, ts2)
# apply Spearman-Brown formula to estimate reliability
(rel.x <- 2 * cor.x / (1 + cor.x))

# even-odd half split
df1 <- HCI[, seq(1, 20, 2)]
df2 <- HCI[, seq(2, 20, 2)]
# total score calculation
ts1 <- rowSums(df1)
ts2 <- rowSums(df2)
# correlation
cor.x <- cor(ts1, ts2)
# apply Spearman-Brown formula to estimate reliability
(rel.x <- 2 * cor.x / (1 + cor.x))

# random halves split
samp <- sample(1:20, 10)
df1 <- HCI[, samp]
df2 <- HCI[, setdiff(1:20, samp)]
# total score calculation
ts1 <- rowSums(df1)
ts2 <- rowSums(df2)
# correlation
cor.x <- cor(ts1, ts2)
# apply Spearman-Brown formula to estimate reliability
(rel.x <- 2 * cor.x / (1 + cor.x))

# minimum of 10,000 split-halves
split <- psych::splitHalf(HCI[, 1:20], raw = TRUE)
items1 <- which(split$minAB[, "A"] == 1)
items2 <- which(split$minAB[, "B"] == 1)
df1 <- HCI[, items1]
df2 <- HCI[, items2]
# total score calculation
ts1 <- rowSums(df1)
ts2 <- rowSums(df2)
# correlation
cor.x <- cor(ts1, ts2)
# apply Spearman-Brown formula to estimate reliability
(rel.x <- 2 * cor.x / (1 + cor.x))

# calculation of CI
z.r <- 0.5 * log((1 + cor.x) / (1 - cor.x))
n <- length(ts1)
z.low <- z.r - 1.96 * sqrt(1 / (n - 3))
z.upp <- z.r + 1.96 * sqrt(1 / (n - 3))

cor.low <- (exp(2 * z.low) - 1) / (exp(2 * z.low) + 1)
cor.upp <- (exp(2 * z.upp) - 1) / (exp(2 * z.upp) + 1)

rel.x <- 2 * cor.x / (1 + cor.x)
rel.low <- 2 * cor.low / (1 + cor.low)
rel.upp <- 2 * cor.upp / (1 + cor.upp)

# average 10,000 split-halves
split <- psych::splitHalf(HCI[, 1:20], raw = TRUE)
(rel.x <- mean(split$raw))

# average all split-halves
split <- psych::splitHalf(HCI[, 1:20], raw = TRUE, brute = TRUE)
(rel.x <- mean(split$raw))

# calculation of CI
n <- length(split$raw)
rel.low <- rel.x - 1.96 * sd(split$raw) / sqrt(n)
rel.upp <- rel.x + 1.96 * sd(split$raw) / sqrt(n)
