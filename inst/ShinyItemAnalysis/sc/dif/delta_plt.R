library(deltaPlotR)

# loading data
data(GMAT, package = "difNLR")
data <- GMAT[, -22]

# delta scores with fixed threshold
(DS_fixed <- deltaPlot(
  data = data, group = "group", focal.name = 1,
  thr = 1.5, purify = FALSE
))
# delta plot
diagPlot(DS_fixed, thr.draw = TRUE)

# delta scores with normal threshold
(DS_normal <- deltaPlot(
  data = data, group = "group", focal.name = 1,
  thr = "norm", purify = FALSE
))
# delta plot
diagPlot(DS_normal, thr.draw = TRUE)
