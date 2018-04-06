## LOAD PACAKGES ####
library(lme4)
library(dplyr)
library(nnet)

data_multinom = data_figs %>%
  select(tvar, Age, Gender, context) %>%
  mutate(tvar = factor(tvar))

#NEXT: understanding what this means and converting to sum coding

mod.mu = multinom(tvar ~ Age + Gender + context, data = data_multinom)
z <- summary(mod.mu)$coefficients/summary(mod.mu)$standard.errors
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p
