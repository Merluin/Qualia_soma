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
  select(subject, trial, key,emotion,condition,  dur)%>%
  mutate(emotion = paste0(condition,".",emotion))%>%
  group_by(subject, trial,emotion)%>%
  summarise_at(vars(dur), list(sum))%>%
  spread(emotion,dur,fill=0)%>%
  select(subject ,trial,control.baffi,control.mixed,control.neutral, emotion.happy, emotion.mixed, emotion.neutral)%>%
  gather(emotion,duration,3:8)%>%
  filter(!duration == 0)%>%
  as.data.frame()%>%
  na.omit()

# summary CT  ----
CT%>%
  # group_by(trial, emotion) %>%
  summarise_at(vars(duration), list(mean)%>%
  'colnames<-'(c("stimulation","emotion","duration","Sd"))%>%
  as.data.frame%>% 
  mutate(duration = duration/1000,Sd = Sd/1000) 

# data ANOVA
CTANOVA<-CT%>%
  group_by(subject,emotion) %>% #add condition
  summarise_at(vars(duration), list(mean))%>%
  as.data.frame()%>%
  mutate(Pt= as.character(subject)
         )%>%
  filter(duration!=0)%>%
  spread(emotion,duration)%>%
  data.frame()

control = CTANOVA[1,c(1,3:5)]%>%
  mutate(condition = "control")%>%
'colnames<-'(c("subject","emotion","mixed","neutral", "condition"))
  

emotion = CTANOVA[1,c(1,6:8)]%>%
  mutate(condition = "emo")%>%
  'colnames<-'(c("subject","emotion","mixed","neutral", "condition"))

data <- rbind(control, emotion)%>%
  select(subject,condition,emotion,neutral,mixed)%>%
  gather(emotion,duration,3:5)%>%
  select(subject,emotion,condition,duration)%>%
  spread(condition,duration)
#simulation expected risults
# nb <- 30
# simulation<-matrix(1:nb,nb,3)%>%
#   as.data.frame()%>%
# mutate(no_happy = round(runif(nb,18000,25000),0),
# yes_happy = no_happy+round(runif(nb,-2000,2000),0),
# no_mixed = round(runif(nb,9000,21000),0),
# yes_mixed = round(runif(nb,9000,21000),0),
# no_neutral = round(runif(nb,8000,15000),0),
# yes_neutral = no_neutral+round(runif(nb,-2000,2000),0))%>%
#   gather(condition,duration,4:9)%>%
#   'colnames<-'(c("subject","stimulation","Emotion","cond","duration"))%>%
#   mutate( stimulation = ifelse(cond=="no_happy" | cond=="no_mixed"| cond=="no_neutral" , "no" , "yes"),
#           Emotion = case_when(cond=="no_happy"| cond=="yes_happy" ~ "happy",
#                               cond=="no_mixed"| cond=="yes_mixed" ~ "mixed",
#                               cond=="no_neutral"| cond=="yes_neutral" ~ "neutral"))%>%
# select( subject, stimulation, Emotion ,duration)
# 
# 
# CTANOVA<-simulation
CTANOVA%>%
  gather(emotion,duration,3:8)%>%
  group_by(emotion) %>%
  summarise_at(vars(duration), list(mean))


# plot CT  ----
CTANOVA%>%
  gather(emotion,duration,3:8)%>%
  ggplot(aes(y=duration,x=emotion, fill = emotion) )+
  geom_bar(stat="identity")+
  theme_classic()+
  theme(text=element_text(size=16,  family="Helvetica"),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        strip.text.y = element_text(size = 20))



data%>%
ggplot(aes(y=control,x=emo) )+
  geom_point(aes(  color=emotion, shape=emotion),size=3)+ 
  #geom_text(aes(  color=Emotion, shape=Emotion,label=subject),size=3)+ 
  geom_abline(intercept = 0, slope = 1)+
  labs(y="control",x="happy")+
  coord_fixed()+
  expand_limits( y=c(2000,30000),x=c(2000,30000))+
  theme_classic()+
  theme(text=element_text(size=16,  family="Helvetica"),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        strip.text.y = element_text(size = 20))

ggsave("07.figures/CTsoma.tiff", units="in", width=5, height=4, dpi=200, compression = 'lzw')

# Anova CT  ----

a1 <- aov_ez("subject", "duration",CTANOVA,   within = c("stimulation", "emotion"))
a1
m1<-emmeans(a1,pairwise~ emotion,adjust="bonf")
m2<-emmeans(a1,pairwise~ stimulation|emotion,adjust="bonf")
emmeans(a1,pairwise~ stimulation,adjust="bonf")


# CHI squared
x<-CTANOVA%>%
  select(subject, stimulation, emotion,  duration)%>%
  filter(duration!=0)%>%
  data.frame()%>%
  spread(stimulation,duration)%>%
  mutate(no = ifelse(no>yes,1,0),
         yes = ifelse(no==1,0,1))%>%
  gather(condition,score,3:4)%>%
  mutate(score = ifelse(score==1,1,NA))

x<-table(x$emotion,x$condition,x$score)%>%data.frame()

x<- x%>%
  spread(Var1,Freq)
chisq.test(x[,-c(1:2)])

# delta test
happy<-CTANOVA%>%
  filter(emotion=="happy" )%>%
  select(subject ,stimulation, duration)%>%
  spread(stimulation,duration)%>%
  mutate(delta= yes-no)%>%
  select(delta)%>%
  t.test()

neutral<-CTANOVA%>%
  filter(emotion=="neutral" )%>%
  select(subject ,stimulation, duration)%>%
  spread(stimulation,duration)%>%
  mutate(delta= yes-no)%>%
  select(delta)%>%
  t.test()

mixed<-CTANOVA%>%
  filter(emotion=="mixed" )%>%
  select(subject ,stimulation, duration)%>%
  spread(stimulation,duration)%>%
  mutate(delta= yes-no)%>%
  select(delta)%>%
  t.test()

p.adjust(happy$p.value,method ="bonferroni",3) < 0.05
p.adjust(neutral$p.value,method ="bonferroni",3) < 0.05
p.adjust(mixed$p.value,method ="bonferroni",3) < 0.05

#################################################
# 
# END
#
#################################################