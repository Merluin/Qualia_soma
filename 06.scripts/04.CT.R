#################################################
# 
# Experiment:     Qualiasoma_binocular_rivalry
# Programmer:     Thomas Quettier
# Date:           02/02/2022
# Description:    Cumulative duration CT analysis
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
############### Cumulative duration CT
############### CT ----
# dataset  ----
CT<-rivalry_dataset%>%
  filter( !dur == 0)%>%
  select(subject, trial,Hz, key,emotion,condition,  dur)%>%
  group_by(subject,trial,condition,Hz,emotion)%>%
  summarise_at(vars(dur), list(sum))%>%
  filter(!dur == 0)%>%
  as.data.frame()%>%
  na.omit()%>%
  'colnames<-'(c("subject","trial","condition","frequency","emotion","duration"))
  

# summary CT  ----
summary<-CT%>%
   group_by(emotion,condition,frequency) %>%
  summarise_at(vars(duration), list(mean))%>%
  as.data.frame%>% 
  mutate(duration = duration/1000) 

# data ANOVA
CTANOVA<-CT%>%
  group_by(subject,emotion,condition,frequency) %>% #add condition
  summarise_at(vars(duration), list(mean))%>%
  mutate(percept = ifelse(emotion == "baffi" | emotion == "happy", "target",emotion))%>%
  data.frame()
  
# data predominance
Delta <- CTANOVA%>%
  select(subject,condition,frequency,percept,duration)%>%
  spread(frequency,duration)%>%
  'colnames<-'(c("subject","condition","percept","freq.0", "freq.5","freq.31"))%>%
  mutate(PR.5 = (freq.5-freq.0),
         PR.31 = (freq.31-freq.0))

# plot CT  ----
CTANOVA%>%
  group_by(percept,condition,frequency) %>%
  summarise_at(vars(duration), list(mean))%>%
  ggplot(aes(y=duration,x=percept, fill = frequency) )+
  geom_bar(stat="identity", position = "dodge2")+
  facet_grid(. ~ condition)+
  theme_classic()+
  theme(text=element_text(size=16,  family="Helvetica"),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        strip.text.y = element_text(size = 20))
ggsave("07.figures/bar_summary.tiff", units="in", width=5, height=4, dpi=200, compression = 'lzw')


# plot delta  ----
Delta%>%
  select(subject,condition,percept,PR.5,PR.31)%>%
  gather(frequency,duration,4:5)%>%
  group_by(percept,condition,frequency) %>%
  summarise_at(vars(duration), list(mean))%>%
  ggplot(aes(y=duration,x=percept, fill = frequency) )+
  geom_bar(stat="identity", position = "dodge2")+
  facet_grid(. ~ condition)+
  theme_classic()+
  theme(text=element_text(size=16,  family="Helvetica"),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        strip.text.y = element_text(size = 20))
ggsave("07.figures/bar_delta.tiff", units="in", width=5, height=4, dpi=200, compression = 'lzw')

# plot 5Hz ----
Delta%>%
ggplot(aes(y=freq.0,x=freq.5) )+
  geom_point(aes(  color=percept, shape=condition),size=3)+ 
#  geom_text(aes(  color=percept, shape=condition,label=subject),size=3)+ 
  geom_abline(intercept = 0, slope = 1)+
  labs(y="no-stimulation",x="5 hz stimulation")+
  coord_fixed()+
  expand_limits( y=c(2000,30000),x=c(2000,30000))+
  theme_classic()+
  theme(text=element_text(size=16,  family="Helvetica"),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        strip.text.y = element_text(size = 20))
ggsave("07.figures/CT5Hz.tiff", units="in", width=5, height=4, dpi=200, compression = 'lzw')

# plot 31Hz ----
Delta%>%
  ggplot(aes(y=freq.0,x=freq.31) )+
  geom_point(aes(  color=percept, shape=condition),size=3)+ 
#  geom_text(aes(  color=percept, shape=condition,label=subject),size=3)+ 
  geom_abline(intercept = 0, slope = 1)+
  labs(y="no-stimulation",x="31 hz stimulation")+
  coord_fixed()+
  expand_limits( y=c(2000,30000),x=c(2000,30000))+
  theme_classic()+
  theme(text=element_text(size=16,  family="Helvetica"),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        strip.text.y = element_text(size = 20))
ggsave("07.figures/CT31Hz.tiff", units="in", width=5, height=4, dpi=200, compression = 'lzw')


# Anova CT  ----

a1 <- aov_ez("subject", "duration",CTANOVA,   within = c( "percept", "condition","frequency"))
a1

#################################################
# 
# END
#
#################################################