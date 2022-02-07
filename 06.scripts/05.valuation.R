#################################################
# 
# Experiment:     Qualiamotion_binocular_rivalry
# Programmer:     Thomas Quettier
# Date:           05/05/2020
# Description:    Stimuli Valuation analysis
#
#################################################

############### Parameters ----
## library ----
library(dplyr)
library(tidyverse)
library(ggplot2)
library(ggeffects)
library(lsmeans)
library(afex)
library(ggpirate)

## loading data ----
rm(list=ls())
load("04.data_preprocessing/qualia_soma.RData")

## Data
############### Valuation ----
# dataset ----
VAVplot<-valuation_dataset %>%
  filter(subject >= 6,subject !=14)%>%
  mutate(valence = case_when(valence == 1 ~ -3,
                             valence == 2 ~ -2,
                             valence == 3 ~ -1,
                             valence == 4 ~  0,
                             valence == 5 ~  1,
                             valence == 6 ~  2,
                             valence == 7 ~  3,))%>%
  select( subject,stimulation, emotion,  valence, arousal)%>%
  group_by(subject,stimulation,emotion )%>%
  summarise_at(vars(valence,arousal), list(mean))%>%
  as.data.frame()

DSV<-VAVplot%>%
  group_by(stimulation,emotion )%>%
  summarise_at(vars(valence,arousal), list(mean,sd))%>%
  mutate_if(is.numeric, round,digits=3)%>%
  select( stimulation, emotion, valence_fn1,valence_fn2,arousal_fn1, arousal_fn2)%>%
  'colnames<-'(c("stimulation","face_emotion","valence_mean","valence_sd","arousal_mean","arousal_sd"))%>%
  as.data.frame()




# plot valuatioon  ----
#plot
ggplot(VAVplot, aes(x=valence, y=arousal, color=emotion, shape=stimulation)) +
  geom_point(size=6, alpha=0.6)+
  scale_colour_manual(values=c( "#56B4E9", "#E69F00"))+
  coord_cartesian(ylim = c(1,7),xlim = c(-3,3))+
  labs(x="Valence valuation",y="Arousal Valuation",fill="Categories")+
  theme_classic()
#ggsave("figure/VAV.pdf", height = 7 , width = 7)



############### ANOVA VALENCE ----
# dataset ----
temp<-valuation_dataset%>%
  filter(subject >= 6,subject !=14)%>%
  mutate(valence = case_when(valence == 1 ~ -3,
                                                  valence == 2 ~ -2,
                                                  valence == 3 ~ -1,
                                                  valence == 4 ~  0,
                                                  valence == 5 ~  1,
                                                  valence == 6 ~  2,
                                                  valence == 7 ~  3,))
  
x<-temp%>%
  select( subject,stimulation,emotion, valence)
colnames(x)<-c("Subject","Condition","Emotion","score")
x$Subject<-as.factor(x$Subject)

x%>%
  group_by( Emotion) %>%
  summarise_at(vars(score), list(mean,sd))%>%
  'colnames<-'(c("Mimicry","Valence","Sd"))%>%
  as.data.frame()

a1 <- aov_ez("Subject", "score", x,  within = c("Condition", "Emotion"))
afex_plot(a1, x = "Emotion", trace = "Condition", error = "within",mapping = c("color", "fill"),
          data_geom = geom_boxplot, data_arg = list(width = 0.4),
          point_arg = list(size = 1.5), line_arg = list(size = 1))+theme_classic()
m1<-emmeans(a1,pairwise~ Emotion,adjust="bonf")

############### ANOVA AROUSAL ----
# dataset ----
x<-valuation_dataset%>%
  filter(subject >= 6,subject !=14)%>%
  select( subject,stimulation,emotion, arousal)
colnames(x)<-c("Subject","Condition","Emotion","score")
x$Subject<-as.factor(x$Subject)

x%>%
  group_by( Emotion) %>%
  summarise_at(vars(score), list(mean,sd))%>%
  'colnames<-'(c("Mimicry","arousal","Sd"))%>%
  as.data.frame()

a2 <- aov_ez("Subject", "score", x,  within = c("Condition", "Emotion"))
afex_plot(a1, x = "Emotion", trace = "Condition", error = "within",mapping = c("color", "fill"),
          data_geom = geom_boxplot, data_arg = list(width = 0.4),
          point_arg = list(size = 1.5), line_arg = list(size = 1))+theme_classic()
m1<-emmeans(a2,pairwise~ Emotion,adjust="bonf")
m1<-emmeans(a2,pairwise~ Emotion|Condition,adjust="bonf")
m2<-emmeans(a2,pairwise~ Condition|Emotion,adjust="bonf")


#################################################
# 
# END
#
#################################################