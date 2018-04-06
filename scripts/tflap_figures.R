## LOAD PACKAGES ####
library(ggplot2)
library(dplyr)

theme_set(theme_bw(base_size = 20))

data_figs = data_clean %>%
  mutate(vowel_length = factor(vowel_length, levels=c("short", "long"))) %>%
  mutate(AgeGroup = factor(AgeGroup, levels=c("younger", "older"))) %>%
  mutate(context = factor(context, levels=c("word-final prevoc", "bimorphemic", "monomorphemic")))  %>%
  mutate(context = factor(context, labels=c("get in", "getting", "better")))
  #mutate(tvar = factor(tvar, levels=c("t", "glottal", "flap"))) %>%
  # if you want flap as bottom colour


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
#preceding vowel length
vowel_length_grey.plot = 
  ggplot(data_figs) +
  geom_bar(aes(x = vowel_length, fill = tvar),  position = "fill")+
  geom_bar(aes(x = vowel_length, group = tvar), 
           fill = c(paste0("grey", 7:9*10),
           paste0("grey", 8:9*10), "#E41A1C"), position = "fill") +
  ylab("") +
  xlab("vowel length") +
  scale_fill_brewer(palette = "Set1") +
  scale_y_continuous(labels = scales::percent) +
  theme(legend.title=element_blank(), legend.position="bottom")

pdf("figures/vowel_length_flap_grey.pdf")
vowel_length_grey.plot
dev.off()

#age & preceding vowel length
vowel_length_age.plot = 
  data_figs %>%
  ggplot(aes(x = vowel_length, fill = tvar)) +
  geom_bar(position = "fill") +
  ylab("") +
  xlab("vowel length") +
  facet_wrap(~AgeGroup) +
  scale_fill_brewer(palette = "Set1") +
  scale_y_continuous(labels = scales::percent) +
  theme(legend.title=element_blank(), legend.position="bottom")

pdf("figures/vowel_length_age.pdf")
vowel_length_age.plot
dev.off()

# position
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

position_age.plot = 
  data_figs %>%
  mutate(Position = factor(Position, levels=c("Internal", "End"))) %>%
  ggplot(aes(x = Position, fill = tvar)) +
  geom_bar(position = "fill") +
  ylab("") +
  xlab("word position") +
  facet_wrap(~AgeGroup) +
  scale_fill_brewer(palette = "Set1") +
  scale_y_continuous(labels = scales::percent) +
  theme(legend.title=element_blank(), legend.position="bottom")

pdf("figures/position_age.pdf", width = 8)
position_age.plot
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

# stem/word/phrase zooming in on tflap
data_figs_swp_tflap = 
  data_figs %>%
  group_by(context, Speaker, tvar) %>%
  summarise (n = n()) %>%
  mutate(freq = n / sum(n)) %>%
  group_by(tvar, context) %>%
  summarise(freq = mean(freq)) %>%
  filter(tvar == "flap")

swp_tflap.plot = 
  data_figs_swp_tflap %>%
  #mutate(context = factor(context, labels=c("get in", "getting", "better"))) %>%
  ggplot(aes(x = context, y = freq*100)) +
  geom_bar(stat = "identity", fill = "#E41A1C")  + 
  ylab("% flap") +
  xlab("") +
  ylim(0, 100)

pdf("figures/swp_flap.pdf")
swp_tflap.plot
dev.off()


# stem/word/phrase zooming in on glottal
data_figs_swp_glottal = 
  data_figs %>%
  group_by(context, Speaker, tvar) %>%
  summarise (n = n()) %>%
  mutate(freq = n / sum(n)) %>%
  group_by(tvar, context) %>%
  summarise(freq = mean(freq)) %>%
  filter(tvar == "glottal")

data_figs_swp_glottal_shortvowel = 
  data_figs %>%
  filter(vowel_length == "short") %>%
  group_by(context, Speaker, tvar) %>%
  summarise (n = n()) %>%
  mutate(freq = n / sum(n)) %>%
  group_by(tvar, context) %>%
  summarise(freq = mean(freq)) %>%
  filter(tvar == "glottal")

data_figs_swp_glottal_spk = 
  data_figs %>%
  group_by(context, Speaker, tvar) %>%
  summarise (n = n()) %>%
  mutate(freq = n / sum(n)) %>%
  #group_by(tvar, context) %>%
  #summarise(freq = mean(freq)) %>%
  filter(tvar == "glottal")

swp_glottal.plot_spk = 
  data_figs_swp_glottal_spk %>%
  ggplot(aes(x = context, y = freq*100)) +
  facet_wrap(~Speaker) +
  geom_bar(stat = "identity", fill = "#377EB8")  + 
  ylab("% glottal") +
  xlab("") +
  ylim(0, 100)

pdf("figures/swp_flap_spk.pdf")
swp_glottal.plot_spk
dev.off()

swp_glottal.plot = 
  data_figs_swp_glottal %>%
  #mutate(context = factor(context, labels=c("get out", "getting", "better"))) %>%
  ggplot(aes(x = context, y = freq*100)) +
  geom_bar(stat = "identity", fill = "#377EB8")  + 
  ylab("% glottal") +
  xlab("") +
  ylim(0, 100)

pdf("figures/swp_glottal.pdf")
swp_glottal.plot
dev.off()

context_swp.plot = 
  data_figs %>%
  filter(!is.na(context)) %>%
  ggplot(aes(x = context, fill = tvar)) +
  geom_bar(position = "fill") +
  ylab("") +
  xlab("context") +
  scale_fill_brewer(palette = "Set1") +
  scale_y_continuous(labels = scales::percent) +
  theme(legend.title=element_blank(), legend.position="bottom")
# this looks different in flaps to when we summarise by speaker, as there must be an influential speaker in the 'better' flaps. So this one isn't exactly the bewst 

pdf("figures/context_swp.pdf")
context_swp.plot
dev.off()

context_swp_shortlong.plot = 
data_figs %>%
  filter(!is.na(context)) %>%
  ggplot(aes(x = context, fill = tvar)) +
  geom_bar(position = "fill") +
  ylab("") +
  xlab("context") +
  scale_fill_brewer(palette = "Set1") +
  scale_y_continuous(labels = scales::percent) +
  facet_wrap(~vowel_length) +
  theme(legend.title=element_blank(), legend.position="bottom")

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

pdf("figures/age_flap_wide.pdf", width = 6, height = 4)
age.plot
dev.off()

#age.sex.plot =
  ggplot(data_figs, aes(x = AgeGroup, fill = tvar)) +
  geom_bar(position = "fill") +
  facet_wrap(~Gender) +
  ylab("") +
  xlab("age group") +
  scale_fill_brewer(palette = "Set1") +
  scale_y_continuous(labels = scales::percent) +
  theme(legend.title=element_blank(), legend.position="bottom")
  
#frequency
data_figs_freq_tflap = 
    data_figs %>%
    group_by(frequency, tvar) %>%
    summarise (n = n()) %>%
    mutate(freq = n / sum(n)) %>%
    group_by(tvar, frequency) %>%
    summarise(freq = mean(freq)) %>%
    filter(tvar == "flap")

data_figs_freq_glottal = 
  data_figs %>%
  group_by(frequency, tvar) %>%
  summarise (n = n()) %>%
  mutate(freq = n / sum(n)) %>%
  group_by(tvar, frequency) %>%
  summarise(freq = mean(freq)) %>%
  filter(tvar == "glottal")

data_figs_freq = 
  data_figs %>%
  group_by(tvar, frequency, Speaker, Word) %>%
  summarise (n = n()) %>%
  mutate(freq = n / sum(n)) 
#this won't work, need to get values as above
ggplot(data_figs_freq_tflap, aes(x = frequency, y=freq)) +
  geom_point() +
  stat_smooth(colour="black")

ggplot(data_figs_freq_glottal, aes(x = frequency, y=freq)) +
  geom_point() +
  stat_smooth(colour="black")


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


ggplot(data_figs, aes(x = context, fill = tvar)) +
  geom_bar(position = "fill") +
  scale_fill_brewer(palette = "Set1") +facet_wrap(~AgeGroup)

ggplot(data_figs, aes(x = Occupation, fill = tvar)) +
  geom_bar(position = "fill") +
  scale_fill_brewer(palette = "Set1")


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



