library(data.table)
library(ShinyItemAnalysis)

# loading data
data(GMATtest, GMATkey, package = "difNLR")
Data <- GMATtest[, 1:20]
key <- unlist(GMATkey)

# combinations - plot for item 1 and 3 groups
plotDistractorAnalysis(Data, key, num.group = 3, item = 1, multiple.answers = TRUE)

# distractors - plot for item 1 and 3 groups
plotDistractorAnalysis(Data, key, num.group = 3, item = 1, multiple.answers = FALSE)

# table with counts and margins - item 1 and 3 groups
DA <- DistractorAnalysis(Data, key, num.groups = 3)[[1]]
dcast(as.data.frame(DA), response ~ score.level, sum, margins = TRUE, value.var = "Freq")

# table with proportions - item 1 and 3 groups
DistractorAnalysis(Data, key, num.groups = 3, p.table = TRUE)[[1]]
