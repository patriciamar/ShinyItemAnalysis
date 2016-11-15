# Function for item distractor analysis
# extended version of distractor.analysis from CTT library
# data The unscored item response from a multiple-choice test
# key The answer key for the items
# p.table If p.table=FALSE (the default) the function returns the counts of examinees 
# who provide each answer. If p.table=TRUE the function returns the proportion of examinees 
# who provide each answer.
# num.groups The number of groups for distractor analysis

DistractorAnalysis <-  function (data, key, p.table = FALSE, num.groups = 3)
{
  data <- as.data.frame(data)
  if (length(key) == 1)
    key <- c(rep(key, ncol(data)))
  scores <- as.data.frame(score(data, key)$score)
  if (missing(key))
    warning("Answer key is not provided")
  else {
    if (!length(key) == ncol(data)) {
      warning("Answer key is not provided or some item keys are missing.")
    }
    key <- c(key)
  }
  score.level <- quantile(scores[, 1], seq(0, 1, by = 1/num.groups))
  score.level <- cut(scores[, 1], score.level, include.lowest = TRUE,
                     labels = paste("Group", 1:num.groups, sep = " "))
  itemtab <- function(response) {
    xtabs( ~ response + score.level)
  }
  itemtabp <- function(response) {
    round(prop.table(xtabs( ~ response + score.level), 2), 3)
  }

  out <- list()
  if (p.table == FALSE)
    for (i in 1:ncol(data)) {
      out[[i]] <- itemtab(data[, i])
    }
  else for (i in 1:ncol(data)) {
    out[[i]] <- itemtabp(data[, i])
  }
  names(out) <- colnames(data)
  out
}
