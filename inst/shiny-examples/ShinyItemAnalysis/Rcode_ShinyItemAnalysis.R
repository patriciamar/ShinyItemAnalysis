###############################################################
# packages
###############################################################

library(difNLR)
library(nnet)
library(ShinyItemAnalysis)

###############################################################
# data
###############################################################
data(GMAT)
data <- GMAT[, colnames(GMAT) != "group"]
# Total score
score <- apply(data, 1, sum)

###############################################################
# SUMMARY of total scores
###############################################################

# TOTAL SCORE
# Summary
summary(score)
# Histogram
hist(score, breaks = 0:ncol(data))

# STANDARD SCORES
tosc <- sort(unique(score)) # Levels of total score
perc <- cumsum(prop.table(table(score))) # Percentiles
sura <- 100 * (tosc / max(score)) # Success rate
zsco <- sort(unique(scale(score))) # Z-score
tsco <- 50 + 10 * zsco # T-score

###############################################################
# TRADITIONAL ANALYSIS
###############################################################

# ITEM ANALYSIS
# Difficulty and discrimination plot
DDplot(data)
# Table
tab <- round(data.frame(item.exam(data, discr = TRUE)[, c(4, 1, 5, 2, 3)],
                        psych::alpha(data)$alpha.drop[, 1]), 2)
tab

# DISTRACTORS
data(GMATtest)
data <- GMATtest[, colnames(GMATtest) != "group"]
data(GMATkey)
key <- GMATkey

# Combinations - plot for item 1 and 3 groups
plotDistractorAnalysis(data, key, num.group = 3, item = 1, multiple.answers = T)
# Distractors - plot for item 1 and 3 groups
plotDistractorAnalysis(data, key, num.group = 3, item = 1, multiple.answers = F)
# Table with counts and margins - item 1 and 3 groups
DA <- DistractorAnalysis(data, key, num.groups = 3)[[1]]
dcast(as.data.frame(DA), response ~ score.level, sum, margins = T, value.var = "Freq")
# Table with proportions - item 1 and 3 groups
DistractorAnalysis(data, key, num.groups = 3, p.table = T)[[1]]
tab

###############################################################
# REGRESSION
###############################################################

# LOGISTIC
data(GMAT)
data <- GMAT[, colnames(GMAT) != "group"]
score <- apply(data, 1, sum)

# Logistic model for item 1
fit <- glm(data[, 1] ~ score, family = binomial)
# Coefficients
coef(fit)
# Function for plot
fun <- function(x, b0, b1){exp(b0 + b1 * x) / (1 + exp(b0 + b1 * x))}
# Plot of estimated curve
curve(fun(x, b0 = coef(fit)[1], b1 = coef(fit)[2]), 0, 20,
      xlab = "Total score",
      ylab = "Probability of correct answer",
      ylim = c(0, 1))


# LOGISTIC Z
stand.score <- scale(apply(data, 1, sum))

# Logistic model for item 1
fit <- glm(data[, 1] ~ stand.score, family = binomial)
# Coefficients
coef(fit)
# Function for plot
fun <- function(x, b0, b1){exp(b0 + b1 * x) / (1 + exp(b0 + b1 * x))}
# Plot of estimated curve
curve(fun(x, b0 = coef(fit)[1], b1 = coef(fit)[2]), -3, 3,
      xlab = "Standardized total score",
      ylab = "Probability of correct answer",
      ylim = c(0, 1))


# LOGISTIC IRT Z
# Logistic model for item 1
fit <- glm(data[, 1] ~ stand.score, family = binomial)
# Coefficients - tranformation
coef <- c(a = coef(fit)[2], b = - coef(fit)[1] / coef(fit)[2])
coef

# NONLINEAR IRT Z
# NLR model for item 1
fun <- function(x, a, b, c){c + (1 - c) * exp(a * (x - b)) / (1 + exp(a * (x - b)))}
fit <- nls(data[, 1] ~ fun(stand.score, a, b, c), algorithm = "port",
           start = startNLR(data, GMAT[, "group"])[1, 1:3])
# Coefficients
coef(fit)
# Plot of estimated curve
curve(fun(x,
          a = coef(fit)[1],
          b = coef(fit)[2],
          c = coef(fit)[3]), -3, 3,
      xlab = "Standardized total score",
      ylab = "Probability of correct answer",
      ylim = c(0, 1))


# MULTINOMIAL
data(GMAT)
data.scored <- GMAT[, colnames(GMAT) != "group"]
stand.score <- scale(apply(data, 1, sum))
data(GMATtest)
data <- GMATtest[, colnames(GMATtest) != "group"]
data(GMATkey)
key <- GMATkey

# multinomial model for item 1
fit <- multinom(relevel(data[, 1], ref = paste(key[1])) ~ stand.score)
# Coefficients
coef(fit)

###############################################################
# IRT MODELS
###############################################################

# 1 PL
data(GMAT)
data <- GMAT[, colnames(GMAT) != "group"]
# Model
fit <- rasch(data)
# Item Characteristic Curves
plot(fit)
# Item Information Curves
plot(fit, type = "IIC")
# Test Information Function
plot(fit, items = 0, type = "IIC")
# Coefficients
coef(fit)
# Factor scores vs Standardized total scores
df1  <- ltm::factor.scores(fit, return.MIvalues = T)$score.dat
FS   <- as.vector(df1[, "z1"])
df2  <- df1
df2$Obs <- df2$Exp <- df2$z1 <- df2$se.z1 <- NULL
STS <- as.vector(scale(apply(df2, 1, sum)))
df  <- data.frame(FS, STS)

plot(FS ~ STS, data = df,
     xlab = "Standardized total score",
     ylab = "Factor score")


# 2 PL
# Model
fit <- ltm(data ~ z1)
# Item Characteristic Curves
plot(fit)
# Item Information Curves
plot(fit, type = "IIC")
# Test Information Function
plot(fit, items = 0, type = "IIC")
# Coefficients
coef(fit)
# Factor scores vs Standardized total scores
df1  <- ltm::factor.scores(fit, return.MIvalues = T)$score.dat
FS   <- as.vector(df1[, "z1"])
df2  <- df1
df2$Obs <- df2$Exp <- df2$z1 <- df2$se.z1 <- NULL
STS <- as.vector(scale(apply(df2, 1, sum)))
df  <- data.frame(FS, STS)

plot(FS ~ STS, data = df,
     xlab = "Standardized total score",
     ylab = "Factor score")


# 3 PL
# Model
fit <- tpm(data)
# Item Characteristic Curves
plot(fit)
# Item Information Curves
plot(fit, type = "IIC")
# Test Information Function
plot(fit, items = 0, type = "IIC")
# Coefficients
coef(fit)
# Factor scores vs Standardized total scores
df1  <- ltm::factor.scores(fit, return.MIvalues = T)$score.dat
FS   <- as.vector(df1[, "z1"])
df2  <- df1
df2$Obs <- df2$Exp <- df2$z1 <- df2$se.z1 <- NULL
STS <- as.vector(scale(apply(df2, 1, sum)))
df  <- data.frame(FS, STS)

plot(FS ~ STS, data = df,
     xlab = "Standardized total score",
     ylab = "Factor score")


###############################################################
# DIF/FAIRNESS
###############################################################
# TOTAL SCORES
# Summary
group <- GMAT[, "group"]
sc_zero <- apply(data[group == 0, ], 1, sum); summary(sc_zero) # total scores of reference group
sc_one  <- apply(data[group == 1, ], 1, sum); summary(sc_one)  # total scores of focal group
# Histograms
hist(sc_zero, breaks = 0:20)
hist(sc_one, breaks = 0:20)

# DELTA PLOTS
# Delta scores with fixed threshold 1.5
deltascores <- deltaPlot(data.frame(data, group), group = "group",
                         focal.name = 1, thr = 1.5)
deltascores
# Delta plot
diagPlot(deltascores, thr.draw = T)

# Delta scores with normal threshold
deltascores <- deltaPlot(data.frame(data, group), group = "group",
                         focal.name = 1, thr = "norm")
deltascores
# Delta plot
diagPlot(deltascores, thr.draw = T)



# MANTEL-HAENSZEL
# Mantel-Haenszel test
fit <- difMH(Data = data, group = group, focal.name = 1,
             p.adjust.method = "BH")
fit
# Contingency table for item 1 and score 12
df <- data.frame(data[, 1], group)
colnames(df) <- c("Answer", "Group")
df$Answer <- relevel(factor(df$Answer, labels = c("Incorrect", "Correct")), "Correct")
df$Group <- factor(df$Group, labels = c("Reference Group", "Focal Group"))
score <- apply(data, 1, sum)

df <- df[score == 12, ]

tab <- dcast(data.frame(xtabs(~ Group + Answer, data = df)),
             Group ~ Answer,
             value.var = "Freq",
             margins = T,
             fun = sum)
tab

# LOGISTIC
# Logistic regression DIF method
fit <- difLogistic(Data = data, group = group, focal.name = 1,
                   type = "both",
                   p.adjust.method = "BH")
fit

# Plot of characteristic curve for item 1
plotDIFLogistic(data, group,
                type = "both",
                item =  1,
                IRT = F,
                p.adjust.method = "BH")
# Coefficients
fit$logitPar


# LOGISTIC IRT Z
# Logistic regression DIF method in IRT parameterization
scaled.score <- scale(score)
fit <- difLogistic(Data = data, group = group, focal.name = 1,
                   type = "both",
                   match = scaled.score,
                   p.adjust.method = "BH")

# Plot of characteristic curve for item 1
plotDIFLogistic(data, group,
                type = "both",
                item =  1,
                IRT = T,
                p.adjust.method = "BH")
# Coefficients for item 1 - recalculation
coef_old <- fit$logitPar[1, ]
coef <- c()
# a = b1, b = -b0/b1, adif = b3, bdif = -(b1b2-b0b3)/(b1(b1+b3))
coef[1] <- coef_old[2]
coef[2] <- -(coef_old[1] / coef_old[2])
coef[3] <- coef_old[4]
coef[4] <- -(coef_old[2] * coef_old[3] + coef_old[1] * coef_old[4] ) /
  (coef_old[2] * (coef_old[2] + coef_old[4]))


# NONLINEAR Z
# Nonlinear regression DIF method
fit <- difNLR(data = data, group = group, type = "both",
              p.adjust.method = "BH")
# Plot of characteristic curve of item 1
plot(fit, item = 1)
# Coefficients
fit$coef

# IRT LORD
# 2PL IRT MODEL
fit <- difLord(Data = data, group = group, focal.name = 1,
               model = "2PL",
               p.adjust.method = "BH")
# Coefficients for item 1
tab_coef <- fit$itemParInit[c(1, ncol(data) + 1), 1:2]

# Plot of characteristic curve of item 1
plotDIFirt(parameters = tab_coef, item = 1)

# IRT RAJU
# 2PL IRT MODEL
fit <- difRaju(Data = data, group = group, focal.name = 1,
               model = "2PL",
               p.adjust.method = "BH")
# Coefficients for item 1
tab_coef <- fit$itemParInit[c(1, ncol(data) + 1), 1:2]

# Plot of characteristic curve of item 1
plotDIFirt(parameters = tab_coef, item = 1, test = "Raju")
