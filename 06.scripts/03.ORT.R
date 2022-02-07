#################################################
# 
# Experiment:     Qualiasoma_binocular_rivalry
# Programmer:     Thomas Quettier
# Date:           12/01/2021
# Description:    Onset Resolution ORT analysis
#
#################################################
rm(list=ls())
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

load("04.data_preprocessing/qualia_soma.RData")

############### ORT ----
# dataset ----
ORT<-onset_dataset%>%
  filter(subject == 1)%>%
  drop_na()%>%
  select(subject, stimulation, onset, initial_percept)%>%
  'colnames<-'(c("subject", "stimulation", "onset", "initial_percept"))%>%
  mutate(onset = log(onset))


# summary ORT  ----
full<-ORT%>%
  group_by(stimulation,initial_percept) %>%
  summarise_at(vars(onset), list(mean,sd))%>%
  mutate_if(is.numeric, round,digits=3)%>%
  'colnames<-'(c("stimulation","initial_percept","onset","sd"))%>%
  as.data.frame()
ip<-ORT%>%
  group_by(initial_percept) %>%
  summarise_at(vars(onset), list(mean,sd))%>%
  mutate_if(is.numeric, round,digits=3)%>%
  'colnames<-'(c("initial_percept","onset","sd"))%>%
  as.data.frame()
mi<-ORT%>%
  group_by(stimulation) %>%
  summarise_at(vars(onset), list(mean,sd))%>%
  mutate_if(is.numeric, round,digits=3)%>%
  'colnames<-'(c("stimulation","onset","sd"))%>%
  as.data.frame()



# data ANOVA
ORTANOVA<-ORT%>%
  group_by(subject,stimulation,initial_percept) %>%
  summarise_at(vars(onset), list(mean))%>%
  data.frame()



# plot ORT  ----
ORTANOVA%>%
  spread(stimulation,onset)%>%
  data.frame()%>%
  'colnames<-'(c("subject",  "IP",  "no" ,   "yes"))%>%
  ggplot(aes(y=yes,x=no) )+
  geom_point(aes(  color=IP, shape=IP),size=3)+ 
  geom_abline(intercept = 0, slope = 1)+
  labs(y="ORT Tactile Stimulation (log)",x="ORT No Stimulation (log)")+
  coord_fixed()+
  expand_limits( y=c(0,10),x=c(0,10))+
  theme_classic()

ggsave("07.figures/ORTsoma.tiff", units="in", width=5, height=4, dpi=200, compression = 'lzw')


ORTANOVA<-ORTANOVA
# Anova ORT ----
a1<-aov_ez("subject", "onset", ORTANOVA,  within = c("stimulation", "initial_percept"))



#################################################
# 
# END
#
#################################################