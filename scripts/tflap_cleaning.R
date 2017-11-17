## LOAD PACKAGES ####
library(dplyr)

## LOAD DATA ####
setwd("data/")
  file_names <- dir() #where you have your files
  data = do.call(rbind,lapply(file_names,read.delim))
setwd("~/Dropbox/Projects/Blackburn/tflap/tflap_R")

## UNWANTED WORDS
unwantedwords = c("HOTEL", "HOTELS", "SETTEE", "SETTEES", "EIGHTEENTH")

## WORDS TO CHANGE CATEGORY
#checking which words have Preceding AA
filter(data_clean, Pre_Seg == "AA") %>%
  distinct(Word)
aawords = c("TOMATO", "TOMATOES", "*TOMATOEY")

## CLEAN DATA ####
data_clean = data %>%
  mutate(tvar = recode(Code1, "d" = "f", "f?" = "f", "?f" = "f")) %>%
  mutate(tvar = factor(tvar, labels=c("flap", "glottal", "t-to-r", "t", NA, "deleted", "?"))) %>%
  filter(tvar == "flap" | tvar == "glottal" | tvar == "t") %>%
  filter(!Word %in% unwantedwords)%>%
  mutate(vowel_length = recode(Pre_Seg, "AA" = "short", "AE" = "short", "AH" = "short", "AO" = "long", "AW" = "long", "AY" = "long",  "EH" = "short", "EY" = "long", "IH" = "short", "IY" = "long",  "OW" = "long",  "UH" = "short", "UW" = "long")) %>%
  mutate(vowel_length = ifelse(Word %in% aawords, "long", as.character(vowel_length)))
  

