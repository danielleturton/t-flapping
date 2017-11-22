## LOAD PACKAGES ####
library(dplyr)

## LOAD DATA ####
setwd("data/")
  file_names <- dir() #where you have your files
  data = do.call(rbind,lapply(file_names,read.delim))
setwd("~/Dropbox/Projects/Blackburn/tflap/tflap_R")

## WORDS TO CHANGE ####
#unwanted words (stress etc)
unwantedwords = c("AUTISM", "DETAILS", "EIGHTEENTH", "EIGHTEEN", "HOTEL", "HOTELS", "POLITICS", "SETTEE", "SETTEES", "LUNATIC")

#syllabic words - different condioning, maybe delete
#more likely flaps before /l/, glottals before /n/?
syllabicwords = c("COTTON", "BUTTON", "LITTLE", "BOTTLE", "RAWTENSTALL", "BRIGHTON")

## WORDS TO CHANGE CATEGORY
#checking which words have Preceding AA
filter(data_clean, Pre_Seg == "AA") %>%
  distinct(Word)
aawords = c("TOMATO", "TOMATOES", "*TOMATOEY")

#data = arrange(data, Code1)

## Social data ####
social = read.delim("/Users/danielle/Dropbox/Projects/Blackburn/Data/social_Nov17.csv")

## CLEAN DATA ####
data_clean = data %>%
  mutate(tvar = recode(Code1, "d" = "f", "f?" = "f", "?f" = "f", "tf" = "f")) %>%
  mutate(tvar = factor(tvar, labels=c(NA, "deleted", "flap", "glottal", "t", "?", "tlat", "t-to-r"))) %>%
  filter(tvar == "flap" | tvar == "glottal" | tvar == "t") %>%
  filter(!Word %in% unwantedwords)%>%
  mutate(vowel_length = recode(Pre_Seg, "AA" = "short", "AE" = "short", "AH" = "short", "AO" = "long", "AW" = "long", "AY" = "long",  "EH" = "short", "EY" = "long", "IH" = "short", "IY" = "long",  "OW" = "long",  "UH" = "short", "UW" = "long")) %>%
  mutate(vowel_length = ifelse(Word %in% aawords, "long", as.character(vowel_length))) %>%
  left_join(social) %>%  
  mutate(duration = Seg_End - Seg_Start) %>% 
  select(24, 21, 25:30, 3, 5, 11, 22, 14, 8, 39, 18)
  

