## LOAD PACKAGES ####
library(ggplot2)
library(dplyr)

theme_set(theme_bw())

data_figs = data_clean

ggplot(data_clean, aes(x = Position, fill = tvar)) +
  geom_bar(position = "fill") + facet_wrap(~File)

ggplot(data_clean, aes(x = vowel_length, fill = tvar)) +
  geom_bar(position = "fill") 


flap_fig_intervocalic = data_clean %>%
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
