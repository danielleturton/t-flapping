## LOAD PACKAGES ####
library(dplyr)

## LOAD DATA ####
setwd("data/")
  file_names <- dir() #where you have your files
  data = do.call(rbind,lapply(file_names,read.delim))
setwd("~/Dropbox/Projects/Blackburn/tflap/tflap_R")

## UNWANTED WORDS
unwantedwords <- c("HOTEL", "HOTELS", "SETTEE", "SETTEES", "EIGHTEENTH")

## CLEAN DATA ####
data_clean = data %>%
  mutate(tvar = recode(Code1, "d" = "f", "f?" = "f", "?f" = "f")) %>%
  mutate(tvar = factor(tvar, labels=c("flap", "glottal", "t-to-r", "t", NA, "deleted", "?"))) %>%
  filter(tvar == "flap" | tvar == "glottal" | tvar == "t") %>%
  filter(!Word %in% unwantedwords)
