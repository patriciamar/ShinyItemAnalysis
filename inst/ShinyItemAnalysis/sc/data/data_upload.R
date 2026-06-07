# loading data from R package
data("LearningToLearn", package = "ShinyItemAnalysis")

# items
Data <- LearningToLearn[, grepl("Item", colnames(LearningToLearn)) & grepl("_9", colnames(LearningToLearn))]
# grouping variable
group <- LearningToLearn[, "track_01"]
# matching variable
match <- LearningToLearn[, "score_6"]
# criterion
criterion <- LearningToLearn[, "score_9"]

# saving datasets as csv files, ready to upload to SIA application
write.csv(Data, file = "LtL_items_grade9.csv", sep = ",", quote = FALSE, row.names = FALSE)
write.csv(group, file = "LtL_track.csv", sep = ",", quote = FALSE, row.names = FALSE)
write.csv(match, file = "LtL_score6.csv", sep = ",", quote = FALSE, row.names = FALSE)
write.csv(criterion, file = "LtL_score9.csv", sep = ",", quote = FALSE, row.names = FALSE)
