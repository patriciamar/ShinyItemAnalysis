library(psychometric)

# loading data
data(HCI, package = "ShinyItemAnalysis")
data <- HCI[, 1:20]

# reliability of original data
rel.original <- psychometric::alpha(data)
# number of items in original data
items.original <- ncol(data)

# number of items in new data
items.new <- 30
# ratio of tests lengths
m <- items.new / items.original
# determining reliability
SBrel(Nlength = m, rxx = rel.original)

# desired reliability
rel.new <- 0.8
# determining test length
(m.new <- SBlength(rxxp = rel.new, rxx = rel.original))
# number of required items
m.new * items.original
