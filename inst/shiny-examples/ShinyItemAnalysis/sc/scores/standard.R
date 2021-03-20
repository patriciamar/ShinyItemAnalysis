# loading data
data(GMAT, package = "difNLR")
data <- GMAT[, 1:20]

# scores calculations (unique values)
score <- rowSums(data)               # Total score
tosc <- sort(unique(score))          # Levels of total score
perc <- ecdf(score)(tosc)            # Percentiles
sura <- 100 * (tosc / max(score))    # Success rate
zsco <- sort(unique(scale(score)))   # Z-score
tsco <- 50 + 10 * zsco               # T-score

cbind(tosc, perc, sura, zsco, tsco)
