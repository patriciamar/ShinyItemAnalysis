library(psych)
library(tidyverse)

# loading and formatting data
data(AIBS, package = "ShinyItemAnalysis")
AIBSwide <- AIBS %>%
  pivot_wider(ID, values_from = Score, names_from = RevCode) %>%
  select(-ID)
head(AIBSwide)

ICC(AIBSwide)
