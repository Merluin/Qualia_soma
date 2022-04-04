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
library(gridExtra)

## loading data ----

load("04.data/qualia_soma.RData")

############### MDD ----

# dataset ----
MDD<-rivalry_dataset%>%
  filter(last.key!="yes",trial < 13, subject > 1)%>%
  select(subject,block,condition, trial,Hz, key,emotion,  dur)%>%
  mutate(percept = ifelse(emotion == "baffi" | emotion == "happy", "target",emotion))%>%
  group_by(subject,block,condition, trial,Hz,percept)%>%
  summarise_at(vars(dur), list(mean))%>%
  spread(percept,dur,fill=0)%>%
  gather(percept,duration,6:8)%>%
  group_by(subject,block,condition,Hz,percept)%>%
  summarise_at(vars(duration), list(mean))%>%
  group_by(subject,condition,Hz,percept,)%>%
  summarise_at(vars(duration), list(mean))%>%
  data.frame()%>%
  'colnames<-'(c("subject","condition","frequency","percept","duration"))

## summary CT  ----
summary<-MDD%>%
  group_by(percept,condition,frequency) %>%
  summarise_at(vars(duration), list(mean))%>%
  as.data.frame%>% 
  mutate(duration = duration/1000) 

# data ANOVA
MDDANOVA<-MDD

# data predominance
Delta <- MDDANOVA%>%
  select(subject,condition,frequency,percept,duration)%>%
  spread(frequency,duration)%>%
  'colnames<-'(c("subject","condition","percept","freq.0", "freq.5","freq.31"))%>%
  mutate(Dlt.5.Hz = (freq.5-freq.0),
         Dlt.31.Hz = (freq.31-freq.0))

# plot MDD  ----
MDDANOVA%>%
  group_by(percept,condition,frequency) %>%
  summarise_at(vars(duration), list(mean))%>%
  mutate(frequency = as.factor(frequency))%>%
  ggplot(aes(y=duration,x=percept, fill = frequency) )+
  geom_bar(stat="identity", position = "dodge2")+
  facet_grid(. ~ condition)+
  theme_classic()+
  theme(text=element_text(size=16,  family="Helvetica"),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        strip.text.y = element_text(size = 20))
ggsave("07.figures/MDD_bar_summary.tiff", units="in", width=5, height=4, dpi=200, compression = 'lzw')

plot_list <- list()
for(i in 2:max(MDDANOVA$subject)){
  g<-MDDANOVA%>%
    filter(subject == i)%>%
    group_by(percept,condition,frequency) %>%
    summarise_at(vars(duration), list(mean))%>%
    mutate(frequency = as.factor(frequency))%>%
    ggplot(aes(y=duration,x=percept, fill = frequency) )+
    geom_bar(stat="identity", position = "dodge2")+
    facet_grid(. ~ condition)+
    theme_classic()+
    theme(text=element_text(size=16,  family="Helvetica"),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black"),
          strip.text.y = element_text(size = 20))
  plot_list[[i-1]] <- g
}
grid.arrange(grobs=plot_list,ncol=2)
ggsave("07.figures/MDD_bar_summary_sub.tiff", units="in", width=5, height=4, dpi=200, compression = 'lzw')


# plot delta  ----
Delta%>%
  select(subject,condition,percept,Dlt.5.Hz,Dlt.31.Hz)%>%
  gather(frequency,duration,4:5)%>%
  group_by(percept,condition,frequency) %>%
  summarise_at(vars(duration), list(mean))%>%
  ggplot(aes(y=duration,x=percept, fill = condition) )+
  geom_bar(stat="identity", position = "dodge2")+
  facet_grid(. ~ frequency)+
  theme_classic()+
  theme(text=element_text(size=16,  family="Helvetica"),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        strip.text.y = element_text(size = 20))
ggsave("07.figures/MDD_bar_delta.tiff", units="in", width=5, height=4, dpi=200, compression = 'lzw')

plot_list <- list()
for(i in 1:max(MDDANOVA$subject)){
  g<-Delta%>%
    filter(subject == i)%>%
    select(subject,condition,percept,Dlt.5,Dlt.31)%>%
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
  plot_list[[i]] <- g
}
grid.arrange(grobs=plot_list,ncol=2)
ggsave("07.figures/MDD_bar_delta_sub.tiff", units="in", width=5, height=4, dpi=200, compression = 'lzw')

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
ggsave("07.figures/MDD_5Hz.tiff", units="in", width=5, height=4, dpi=200, compression = 'lzw')

# plot 31Hz ----
Delta%>%
  ggplot(aes(y=freq.0,x=freq.31) )+
  geom_point(aes(  color=percept, shape=condition),size=3)+ 
  #geom_text(aes(  color=percept, shape=condition,label=subject),size=3)+ 
  geom_abline(intercept = 0, slope = 1)+
  labs(y="no-stimulation",x="31 hz stimulation")+
  coord_fixed()+
  expand_limits( y=c(2000,30000),x=c(2000,30000))+
  theme_classic()+
  theme(text=element_text(size=16,  family="Helvetica"),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        strip.text.y = element_text(size = 20))
ggsave("07.figures/MDD_31Hz.tiff", units="in", width=5, height=4, dpi=200, compression = 'lzw')

# anova MDD  ----
a1 <- aov_ez("subject", "duration",MDDANOVA,   within = c( "percept", "condition","frequency"))
a1
a1m1<-emmeans(a1,pairwise~ percept|condition,adjust="bonf")

# Anova Delta MDD  ----
x<-Delta%>%
  filter(percept!="mixed")%>%
  select(subject,condition,percept,PR.5.Hz,PR.31.Hz)%>%
  gather(frequency,duration,4:5)
a2 <- aov_ez("subject", "duration",x,   within = c( "percept", "condition","frequency"))
a2
a2m1<-emmeans(a1,pairwise~ percept|condition|frequency,adjust="bonf")

# results dataset
anova.mdd<-a1
onova.delta <- a2
posthoc.mdd <- a1m1
posthoc.delta <- a2m1
save(anova.mdd,
     onova.delta,
     posthoc.mdd,
     posthoc.delta,
     file = "04.data/MDD_results.RData")

#################################################
# 
# END
#
#################################################