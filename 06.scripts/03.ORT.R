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

load("04.data/qualia_soma.RData")

############### ORT ----
# dataset ----
ORT<-onset_dataset%>%
  drop_na()%>%
  filter(subject>1)%>%
  select(subject, condition,stimulation, onset, initial_percept)%>%
  'colnames<-'(c("subject","condition", "stimulation", "onset", "initial_percept"))%>%
  group_by(subject, condition,stimulation, initial_percept)%>%
  summarise_at(vars(onset), list(mean))%>%
  mutate(percept = ifelse(initial_percept == "baffi" | initial_percept == "happy", "target",initial_percept))%>%
  select(subject, condition,stimulation, onset, percept)%>%
  spread(percept,onset,fill=0)%>%
  select(subject ,condition ,stimulation, mixed, neutral, target)%>%
  gather(initial_percept,onset,4:6)%>%
  mutate(onset = log(onset))


# summary ORT  ----
full<-ORT%>%
  group_by(stimulation,condition,initial_percept) %>%
  summarise_at(vars(onset), list(mean,sd))%>%
  mutate_if(is.numeric, round,digits=3)%>%
  'colnames<-'(c("stimulation","initial_percept","onset","sd"))%>%
  as.data.frame()

# data ANOVA
ORTANOVA<-ORT%>%
  group_by(subject,stimulation,condition,initial_percept) %>%
  summarise_at(vars(onset), list(mean))%>%
  data.frame()%>%
  mutate(onset = log(onset))

plot <- ORTANOVA%>%
  filter(onset >= 0, initial_percept != "mixed")%>%
  select(subject,condition,stimulation,initial_percept,onset)%>%
  spread(stimulation,onset)%>%
  'colnames<-'(c("subject","condition","percept","freq.0", "freq.5","freq.31"))%>%
  na.omit()




# plot 5Hz ----
plot%>%
  ggplot(aes(y=freq.0,x=freq.5) )+
  geom_point(aes(  color=percept, shape=condition),size=3)+ 
  #  geom_text(aes(  color=percept, shape=condition,label=subject),size=3)+ 
  geom_abline(intercept = 0, slope = 1)+
  labs(y="No-stimulation",x="5 Hz stimulation")+
  coord_fixed()+
  expand_limits( y=c(1.5,2.5),x=c(1.5,2.5))+
  theme_classic()+
  theme(text=element_text(size=16,  family="Helvetica"),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        strip.text.y = element_text(size = 20))
ggsave("07.figures/ORT_5Hz.tiff", units="in", width=5, height=4, dpi=200, compression = 'lzw')

# plot 31Hz ----
plot%>%
  ggplot(aes(y=freq.0,x=freq.31) )+
  geom_point(aes(  color=percept, shape=condition),size=3)+ 
  #  geom_text(aes(  color=percept, shape=condition,label=subject),size=3)+ 
  geom_abline(intercept = 0, slope = 1)+
  labs(y="No-stimulation",x="31 Hz stimulation")+
  coord_fixed()+
  expand_limits( y=c(1.5,2.5),x=c(1.5,2.5))+
  theme_classic()+
  theme(text=element_text(size=16,  family="Helvetica"),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        strip.text.y = element_text(size = 20))
ggsave("07.figures/ORT_31Hz.tiff", units="in", width=5, height=4, dpi=200, compression = 'lzw')


ORTANOVA<-ORTANOVA
# Anova ORT ----
ORTcontrol<-ORTANOVA%>%
  filter(condition == "control",stimulation != 31)
ORTemotion<-ORTANOVA%>%
  filter(condition == "emotion",stimulation != 31)

a1<-aov_ez("subject", "onset", ORTANOVA,  within = c("condition","stimulation", "initial_percept"))
a2<-aov_ez("subject", "onset", ORTemotion,  within = c("stimulation", "initial_percept"))
a3<-aov_ez("subject", "onset", ORTcontrol,  within = c("stimulation", "initial_percept"))

# results dataset
anova.ORT<-a1

save(anova.ORT,
     file = "04.data/ORT_results.RData")


#################################################
# 
# END
#
#################################################