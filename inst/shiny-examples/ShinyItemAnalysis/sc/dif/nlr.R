library(difNLR)

# loading data
data(GMAT, package = "difNLR")
data <- GMAT[, 1:20]
group <- GMAT[, "group"]

# generalized logistic regression DIF method
# using 3PL model with the same guessing parameter for both groups
(fit <- difNLR(
  Data = data, group = group, focal.name = 1, model = "3PLcg",
  match = "zscore", type = "all", p.adjust.method = "none", purify = FALSE
))

# loading data
data(LearningToLearn, package = "ShinyItemAnalysis")
data <- LearningToLearn[, 87:94] # item responses from Grade 9 from subscale 6
group <- LearningToLearn$track # school track - group membership variable
match <- scale(LearningToLearn$score_6) # standardized test score from Grade 6

# detecting differential item functioning in change (DIF-C) using
# the generalized logistic regression DIF method with 3PL model
# with the same guessing parameter for both groups
# and standardized total score from Grade 6 as the matching criterion
(fit <- difNLR(
  Data = data, group = group, focal.name = "AS", model = "3PLcg",
  match = match, type = "all", p.adjust.method = "none", purify = FALSE
))
