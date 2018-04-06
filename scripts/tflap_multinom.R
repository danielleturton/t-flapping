## LOAD PACAKGES ####
library(lme4)
library(dplyr)
library(nnet)

data_multinom = data_figs %>%
  select(tvar, Age, Gender, context) 

mod.mu = multinom(tvar ~ Age + Gender + context, data = data_multinom)

#test case ####
library(nnet)
options(contrasts = c("contr.treatment", "contr.poly"))
library(MASS)
example(birthwt)
(bwt.mu <- multinom(low ~ ., bwt))
z <- summary(bwt.mu)$coefficients/summary(bwt.mu)$standard.errors
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p