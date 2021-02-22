library(ggplot2)
library(purrr)
library(ShinyItemAnalysis)

# loading data
data(AIBS, package = "ShinyItemAnalysis")

# estimate reliability with ICC for complete AIBS dataset
ICCrestricted(Data = AIBS, case = "ID", var = "Score", rank = "ScoreRankAdj")

# estimate range-restricted ICC
ICCrestricted(Data = AIBS, case = "ID", var = "Score", rank = "ScoreRankAdj",
              sel = 0.90, dir = "top")

# caterpillar plot
AIBS %>%
  ggplot(aes(x = ScoreRankAdj, y = Score, group = ID)) +
  geom_line(col = "gray") +
  geom_point(shape = 1, size = 1.5) +
  stat_summary(fun = mean, fun.args = list(na.rm = TRUE), geom = "point",
               col = "red", shape = 5, size = 2.5, stroke = .35) +
  labs(x = "Ratee rank", y = "Rating (score)") +
  coord_cartesian(ylim = c(1, 5)) +
  theme_app()

# estimate all possible top-restricted subsets
all_top_restricted <- map_dfr(2:72,
                              ~ ICCrestricted(Data = AIBS, case = "ID", var = "Score",
                                              rank = "ScoreRankAdj", sel = .x, nsim = 10))
all_top_restricted

# or alternatively, in base R:
base_way <- lapply(2:72, function(x) {
  ICCrestricted(Data = AIBS, case = "ID", var = "Score", rank = "ScoreRankAdj",
                sel = x, nsim = 10)})
do.call(rbind.data.frame, base_way)

# plot
all_top_restricted %>%
  ggplot(aes(prop_sel, ICC1, ymin = ICC1_LCI, ymax = ICC1_UCI)) +
  geom_pointrange() + scale_x_continuous(labels = scales::percent) +
  labs(x = ("Proportion of top ratees"), y = "Reliability") +
  coord_cartesian(ylim = c(0, 1), xlim = c(0, 1)) +
  theme_app()
