## LOAD PACKAGES ####
library(ggplot2)
library(dplyr)

theme_set(theme_bw(base_size = 14))

data_figs = data_clean %>%
  mutate(vowel_length = factor(vowel_length, levels=c("short", "long"))) %>%
  mutate(AgeGroup = factor(AgeGroup, levels=c("younger", "older"))) %>%
  #mutate(tvar = factor(tvar, levels=c("t", "glottal", "flap"))) %>%
  # if you want flap as bottom colour
  mutate(morphClass = factor(Code2, labels = c(NA, "bimorphemic", "monomorphemic")))


## CURRENT GRAPHS TO RUN ####
#preceding vowel length
vowel_length.plot = 
  data_figs %>%
  ggplot(aes(x = vowel_length, fill = tvar)) +
  geom_bar(position = "fill") +
  ylab("") +
  xlab("vowel length") +
  scale_fill_brewer(palette = "Set1") +
  scale_y_continuous(labels = scales::percent) +
  theme(legend.title=element_blank(), legend.position="bottom")

pdf("figures/vowel_length_flap.pdf")
vowel_length.plot
dev.off()

# preceding vowel, greyed out

position.plot = 
data_figs %>%
  mutate(Position = factor(Position, levels=c("Internal", "End"))) %>%
  ggplot(aes(x = Position, fill = tvar)) +
  geom_bar(position = "fill") +
  ylab("") +
  xlab("word position") +
  scale_fill_brewer(palette = "Set1") +
  scale_y_continuous(labels = scales::percent) +
  theme(legend.title=element_blank(), legend.position="bottom")

pdf("figures/position_flap.pdf")
position.plot
dev.off()

# morphological class zooming in on tflap
data_figs_morph_tflap = 
  data_figs %>%
  filter(Position == "Internal") %>%
  group_by(morphClass, Speaker, tvar) %>%
  summarise (n = n()) %>%
  mutate(freq = n / sum(n)) %>%
  group_by(tvar, morphClass) %>%
  summarise(freq = mean(freq)) %>%
  filter(tvar == "flap")

morph_tflap.plot = 
  data_figs_morph_tflap %>%
  mutate(morphClass = factor(morphClass, labels=c("getting", "better"))) %>%
  ggplot(aes(x = morphClass, y = freq*100)) +
  geom_bar(stat = "identity", fill = "#E41A1C")  + 
  ylab("% flap") +
  xlab("morphological class") +
  ylim(0, 100)

pdf("figures/morph_flap.pdf")
morph_tflap.plot
dev.off()

## SOCIAL PLOTS ####
age.plot =
  ggplot(data_figs, aes(x = AgeGroup, fill = tvar)) +
  geom_bar(position = "fill") +
  ylab("") +
  xlab("age group") +
  scale_fill_brewer(palette = "Set1") +
  scale_y_continuous(labels = scales::percent) +
  theme(legend.title=element_blank(), legend.position="bottom")

pdf("figures/age_flap.pdf")
age.plot
dev.off()

#messing
data_figs_age = data_figs %>%
  group_by(Age, Speaker, tvar) %>%
  summarise (n = n()) %>%
  mutate(freq = n / sum(n)) %>%
  group_by(tvar, Age) %>%
  summarise(freq = mean(freq)) %>%
  filter(tvar == "flap")
  #na.omit() %>%
  #ungroup()

##previous graphs ####
#morphClass plot over every token, not averaging by speaker
# potentially problematic as some people don't flap, so skews distribution
data_figs %>%
  filter(Position == "Internal") %>%
  filter(Code2 != "") %>%
  mutate(Code2 = factor(Code2, labels=c("getting", "better"))) %>%
  ggplot(aes(x = Code2, fill = tvar)) +
  geom_bar(position = "fill") +
  ylab("") +
  xlab("morphological class") +
  scale_fill_brewer(palette = "Set1") +
  scale_y_continuous(labels = scales::percent) +
  theme(legend.title=element_blank(), legend.position="bottom")

intervocalic.position = data_clean %>%
  filter(Position == "Internal") %>%
  filter(Code2 != "") %>%
  #mutate(Group = factor(File, labels=c("Older", "Younger"))) %>%
  mutate(Code2 = factor(Code2, labels=c("getting", "better"))) %>%
  ggplot(aes(x = Code2, fill = tvar)) +
  geom_bar(position = "fill") + facet_wrap(~File) +
  ylab("") +
  xlab("intervocalic context") +
  scale_fill_brewer(palette = "Set1") +
  theme(legend.title=element_blank(), legend.position="top")



##  TRYING TO AVERAGE OVER SPEAKER AND WORD
#this was Joe's solution
# doesn't add quite up to 100 though, so just gonna do normal way I think
data_figs_vowellength = 
  data_figs %>%
  group_by(vowel_length, Speaker, tvar) %>%
  summarise (n = n()) %>%
  mutate(freq = n / sum(n)) %>%
  group_by(tvar, vowel_length) %>%
  summarise(freq = mean(freq)) 

ggplot(data_figs_vowellength, aes(x = vowel_length, y = freq*100, fill = tvar)) +
  geom_bar(stat = "identity") 

#general rates
data_clean %>%
  group_by(Speaker, tvar) %>%
  summarise (n = n()) %>%
  mutate(freq = n / sum(n)) %>%
  group_by(tvar) %>%
  summarise(freq = mean(freq)) 
#can't do by word cos some words don't have all three vars



