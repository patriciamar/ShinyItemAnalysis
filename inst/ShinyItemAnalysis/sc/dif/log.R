library(difR)

# loading data
data(GMAT, package = "difNLR")
data <- GMAT[, 1:20]
group <- GMAT[, "group"]

# logistic regression DIF detection method
(fit <- difLogistic(
  Data = data, group = group, focal.name = 1, match = "score",
  type = "both", p.adjust.method = "none", purify = FALSE
))

# loading data
data(LearningToLearn, package = "ShinyItemAnalysis")
data <- LearningToLearn[, 87:94] # item responses from Grade 9 from subscale 6
group <- LearningToLearn$track # school track - group membership variable
match <- scale(LearningToLearn$score_6) # standardized test score from Grade 6

# detecting differential item functioning in change (DIF-C) using
# the logistic regression DIF detection method
# and standardized total score from Grade 6 as the matching criterion
(fit <- difLogistic(
  Data = data, group = group, focal.name = "AS", match = match,
  type = "both", p.adjust.method = "none", purify = FALSE
))
