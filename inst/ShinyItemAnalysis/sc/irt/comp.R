library(mirt)

# loading data
data(GMAT, package = "difNLR")

# 1PL IRT model
fit1PL <- mirt(GMAT[, 1:20],
               model = 1,
               constrain = list((1:20) + seq(0, (20 - 1) * 3, 3)), itemtype = "2PL"
)
# 2PL IRT model
fit2PL <- mirt(GMAT[, 1:20], model = 1, itemtype = "2PL")
# 3PL IRT model
fit3PL <- mirt(GMAT[, 1:20], model = 1, itemtype = "3PL")
# 4PL IRT model
fit4PL <- mirt(GMAT[, 1:20], model = 1, itemtype = "4PL")

# comparison
anova(fit1PL, fit2PL)
anova(fit2PL, fit3PL)
anova(fit3PL, fit4PL)
