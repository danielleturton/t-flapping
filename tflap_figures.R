## LOAD PACKAGES ####
library(ggplot2)
library(dplyr)

theme_set(theme_bw(base_size = 14))

data_figs = data_clean

## CURRENT GRAPHS ####
vowel_length.plot = 
  data_figs %>%
  mutate(vowel_length = factor(vowel_length, levels=c("short", "long"))) %>%
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

#this won't work because only Trudy and WIllow are coded for morphclass
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

pdf("figures/position_flap.pdf")
position.plot
dev.off()


ggplot(data_figs, aes(x = Position, fill = tvar)) +
  geom_bar(position = "fill") + facet_wrap(~File)

##previous graphs ####
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
data_figs_vowellength = data_figs %>%
  group_by(vowel_length, File, tvar) %>%
  summarise (n = n()) %>%
  mutate(freq = n / sum(n)) %>%
  group_by(tvar, vowel_length) %>%
  summarise(freq = mean(freq)) 

#general rates
data_clean %>%
  group_by(File, tvar) %>%
  summarise (n = n()) %>%
  mutate(freq = n / sum(n)) %>%
  group_by(tvar) %>%
  summarise(freq = mean(freq)) 
#can't do by word cos some words don't have all three vars


