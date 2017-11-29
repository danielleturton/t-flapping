## LOAD PACAKGES ####
library(lme4)

data_stats_glottal = data_figs %>%
  filter(tvar == "glottal")

mod = glmer(tvar ~ context + (1|Speaker) + (1|Word), family=poisson, data=data_stats)
