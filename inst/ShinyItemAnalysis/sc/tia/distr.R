library(ShinyItemAnalysis)

# loading data
data(GMATtest, GMATkey, package = "difNLR")
Data <- GMATtest[, 1:20]
key <- GMATkey

# combinations - plot for item 1 and 3 groups
plotDistractorAnalysis(Data, key, num.group = 3, item = c(1, 3), multiple.answers = TRUE)

# distractors - plot for item 1 and 3 groups
plotDistractorAnalysis(Data, key, num.group = 3, item = c(1, 3), multiple.answers = FALSE)

# table with counts - item 1 and 3 groups
DistractorAnalysis(Data, key, item = c(1, 3), num.groups = 3)

# table with proportions - item 1 and 3 groups
DistractorAnalysis(Data, key, item = c(1, 3), num.groups = 3, p.table = TRUE)
