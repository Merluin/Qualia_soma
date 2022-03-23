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
  drop_na()%>%
  select(subject, condition,stimulation, onset, initial_percept)%>%
  'colnames<-'(c("subject","condition", "stimulation", "onset", "initial_percept"))%>%
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
  mutate(percept = ifelse(initial_percept == "baffi" | initial_percept == "happy", "target",initial_percept))%>%
  data.frame()

plot <- ORTANOVA%>%
  select(subject,condition,stimulation,percept,onset)%>%
  spread(stimulation,onset,fill=0)%>%
  'colnames<-'(c("subject","condition","percept","freq.0", "freq.5","freq.31"))



# plot 5Hz ----
plot%>%
  ggplot(aes(y=freq.0,x=freq.5) )+
  geom_point(aes(  color=percept, shape=condition),size=3)+ 
  #  geom_text(aes(  color=percept, shape=condition,label=subject),size=3)+ 
  geom_abline(intercept = 0, slope = 1)+
  labs(y="no-stimulation",x="5 hz stimulation")+
  coord_fixed()+
  expand_limits( y=c(0,12),x=c(0,12))+
  theme_classic()+
  theme(text=element_text(size=16,  family="Helvetica"),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        strip.text.y = element_text(size = 20))
ggsave("07.figures/ORT5Hz.tiff", units="in", width=5, height=4, dpi=200, compression = 'lzw')

# plot 31Hz ----
plot%>%
  ggplot(aes(y=freq.0,x=freq.31) )+
  geom_point(aes(  color=percept, shape=condition),size=3)+ 
  #  geom_text(aes(  color=percept, shape=condition,label=subject),size=3)+ 
  geom_abline(intercept = 0, slope = 1)+
  labs(y="no-stimulation",x="31 hz stimulation")+
  coord_fixed()+
  expand_limits( y=c(0,12),x=c(0,12))+
  theme_classic()+
  theme(text=element_text(size=16,  family="Helvetica"),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        strip.text.y = element_text(size = 20))
ggsave("07.figures/ORT31Hz.tiff", units="in", width=5, height=4, dpi=200, compression = 'lzw')


ORTANOVA<-ORTANOVA
# Anova ORT ----
a1<-aov_ez("subject", "onset", ORTANOVA,  within = c("condition","stimulation", "percept"))



#################################################
# 
# END
#
#################################################