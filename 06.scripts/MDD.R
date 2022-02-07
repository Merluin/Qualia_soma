#################################################
# 
# Experiment:     Qualiasoma_binocular_rivalry
# Programmer:     Thomas Quettier
# Date:           05/05/2020
# Description:    Mean Dominance Duration MDD analysis
#
#################################################

rm(list=ls())
############### Parameters ----
## library ----
library(dplyr)
library(tidyverse)
library(ggplot2)
library(ggeffects)
library(afex)
library(hrbrthemes)
library(emmeans)

## loading data ----

load("04.data_preprocessing/qualia_soma.RData")
## Data
############### MDD ----
# dataset ----
removed<-rivalry_dataset%>%
  filter(last.key!="yes",subject >=6,subject !=14)%>%
  select(subject, stimulation, key,emotion, trial,  duration)%>%
  group_by(subject, stimulation, emotion, trial)%>%
  summarise_at(vars(duration), list(mean))%>%
  spread(emotion,duration,fill=0)%>%
  select(subject ,stimulation ,trial, happy, mixed, neutral)%>%
  gather(emotion,duration,4:6)%>%
  as.data.frame()%>%
  na.omit()

# summary MDD  ----
 removed%>%
  group_by(stimulation, emotion) %>%
  summarise_at(vars(duration), list(mean,sd))%>%
  'colnames<-'(c("stimulation","Emotion","duration","Sd"))%>%
  as.data.frame%>% 
  mutate(duration = duration/1000,Sd = Sd/1000) 

# data ANOVA
MDDANOVA<-removed%>%
  group_by(subject,stimulation,emotion) %>%
  summarise_at(vars(duration), list(mean))%>%
  as.data.frame()%>%
  mutate(Pt= as.character(subject),
         #group = ifelse(subject<6,"cheeks","mouth")
         )

# plot MDD  ----
MDDANOVA%>%
 filter(duration!=0)%>%
  spread(stimulation,duration)%>%
  data.frame()%>%
  ggplot(aes(y=yes,x=no) )+
  #geom_text(aes(color=emotion, shape=emotion,label= subject))+
  geom_point(aes(  color=emotion, shape=emotion),size=3)+ 
  geom_abline(intercept = 0, slope = 1)+
  labs(y="Bolcked Mean Dominance Duration (ms)",x="Free Mean Dominance Duration (ms)")+
  coord_fixed()+
  expand_limits( y=c(5000,12000),x=c(5000,12000))+
  theme_classic()+
  theme(text=element_text(size=16,  family="Helvetica"),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        strip.text.y = element_text(size = 20))
ggsave("07.figures/MDDsoma.tiff", units="in", width=5, height=4, dpi=200, compression = 'lzw')


# anova MDD  ----

a1<-aov_ez("subject", "duration", MDDANOVA,  within = c("stimulation", "emotion"))
a1
m1<-emmeans(a1,pairwise~ emotion,adjust="bonf")


#################################################
# 
# END
#
#################################################