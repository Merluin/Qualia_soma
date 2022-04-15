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
library(wesanderson)

## loading data ----
rm(list=ls())
load("04.data/qualia_soma.RData")

## Data
############### Valuation ----
# dataset ----


DSV<-valuation_dataset%>%
  select(subject>1)%>%
  group_by(frequency,emotion )%>%
  summarise_at(vars(valence,arousal), list(mean,sd))%>%
  mutate_if(is.numeric, round,digits=3)%>%
  select( frequency, emotion, valence_fn1,valence_fn2,arousal_fn1, arousal_fn2)%>%
  'colnames<-'(c("frequency","face_emotion","valence_mean","valence_sd","arousal_mean","arousal_sd"))%>%
  as.data.frame()


# plot valuatioon  ----
#plot
DSV%>%ggplot(aes(x=valence_mean, y=arousal_mean,color=as.factor(frequency),  shape=as.factor(face_emotion)))+
  geom_point(size=6, alpha=0.6)+
  coord_cartesian(ylim = c(1,7),xlim = c(-3,3))+
  labs(x="Valence valuation",y="Arousal Valuation",fill="Categories")+
  theme_classic()
ggsave("07.figures/Valuation.tiff", units="in", width=5, height=4, dpi=200, compression = 'lzw')



############### ANOVA VALENCE ----
# dataset ----
 
x<-valuation_dataset%>%
  select( subject,emotion, frequency, valence)%>%
  'colnames<-'(c("subject","emotion","frequency","score"))%>%
  mutate(subject =as.factor(subject))


a1 <- aov_ez("subject", "score", x,  within = c("frequency", "emotion"))

a1m1<-emmeans(a1,pairwise~ emotion,adjust="bonf")

############### ANOVA AROUSAL ----
# dataset ----
y<-valuation_dataset%>%
  select( subject,emotion, frequency, arousal)%>%
  'colnames<-'(c("subject","emotion","frequency","score"))%>%
  mutate(subject =as.factor(subject))


a2 <- aov_ez("subject", "score", y,  within = c("frequency", "emotion"))

a2m1<-emmeans(a2,pairwise~ emotion,adjust="bonf")

# results dataset
anova.valence<-a1
onova.arousal <- a2
posthoc.valence <- a1m1
posthoc.arousal <- a2m1
save(anova.valence,
     onova.arousal,
     posthoc.valence,
     posthoc.arousal,
     file = "04.data/valuation_results.RData")


#################################################
# 
# END
#
#################################################