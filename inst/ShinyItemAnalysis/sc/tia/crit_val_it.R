library(ShinyItemAnalysis)

# loading data
data(GMAT, GMATtest, GMATkey, package = "difNLR")
data <- GMATtest[, 1:20]
data_binary <- GMAT[, 1:20]
key <- GMATkey
criterion <- GMAT[, "criterion"]

# item difficulty / criterion validity plot
DDplot(data_binary, criterion = criterion, val_type = "simple")

# distractor plot for item 1 and 3 groups
plotDistractorAnalysis(data, key, num.groups = 3, item = 1, criterion = criterion)

# test for association between total score and criterion variable for item 1
cor.test(criterion, data_binary[, 1], method = "pearson", exact = FALSE)
