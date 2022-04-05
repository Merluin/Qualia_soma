#################################################
# 
# Experiment:     Qualiasoma_binocular_rivalry
# Programmer:     Thomas Quettier
# Date:           04/04/2022
# Description:    Predominance ratio PR analysis
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

# Cumulative duration CT

# dataset  ----
PR<-rivalry_dataset%>%
  select(subject,block,condition, trial,Hz, key,emotion,  dur)%>%
  filter(trial < 13, subject > 1)%>%
  mutate(percept = ifelse(emotion == "baffi" | emotion == "happy", "target",emotion))%>%
  group_by(subject,block,condition, trial,Hz,percept)%>%
  summarise_at(vars(dur), list(sum))%>%
  spread(percept,dur,fill=0)%>%
  select(subject ,block,condition ,trial,Hz, mixed, neutral, target)%>%
  gather(percept,duration,6:8)%>%
  group_by(subject,block,condition,Hz,percept)%>%
  summarise_at(vars(duration), list(mean))%>%
  group_by(subject,condition,Hz,percept,)%>%
  summarise_at(vars(duration), list(mean))%>%
  data.frame()%>%
  'colnames<-'(c("subject","condition","frequency","percept","duration"))



# summary PR  ----
summary<-PR%>%
  group_by(percept,condition,frequency) %>%
  summarise_at(vars(duration), list(mean))%>%
  as.data.frame%>% 
  spread(percept,duration,fill=0)%>%
  mutate(PR = (target-neutral)/(neutral+target))
  

# data ANOVA
PRANOVA<-PR%>%
  group_by(subject,percept,condition,frequency) %>%
  summarise_at(vars(duration), list(mean))%>%
  as.data.frame%>% 
  spread(percept,duration,fill=0)%>%
  mutate(PR = (target-neutral)/(neutral+target))


# data predominance
Delta <- PRANOVA%>%
  select(subject,condition,frequency,PR)%>%
  spread(frequency,PR)%>%
  'colnames<-'(c("subject","condition","pr.0", "pr.5","pr.31"))%>%
  mutate(Dlt.5 = (pr.5-pr.0),
         Dlt.31 = (pr.31-pr.0))

# plot CT  ----
PRANOVA%>%
  select(subject,condition,frequency,PR)%>%
  group_by(condition,frequency) %>%
  summarise_at(vars(PR), list(mean))%>%
  mutate(frequency = as.factor(frequency))%>%
  ggplot(aes(y=PR,x=frequency, fill = frequency) )+
  geom_bar(stat="identity", position = "dodge2")+
  facet_grid(. ~ condition)+
  theme_classic()+
  theme(text=element_text(size=16,  family="Helvetica"),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        strip.text.y = element_text(size = 20))
ggsave("07.figures/PR_bar_summary.tiff", units="in", width=5, height=4, dpi=200, compression = 'lzw')

plot_list <- list()
for(i in 2:max(PRANOVA$subject)){
  g<-PRANOVA%>%
    filter(subject == i)%>%
    select(subject,condition,frequency,PR)%>%
    group_by(condition,frequency) %>%
    summarise_at(vars(PR), list(mean))%>%
    mutate(frequency = as.factor(frequency))%>%
    ggplot(aes(y=PR,x=frequency, fill = frequency) )+
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
ggsave("07.figures/PR_bar_summary_sub.tiff", units="in", width=5, height=4, dpi=200, compression = 'lzw')

# plot PR  ----
Delta%>%
  select(subject,condition,Dlt.5,Dlt.31)%>%
  gather(frequency,duration,3:4)%>%
  group_by(condition,frequency) %>%
  summarise_at(vars(duration), list(mean))%>%
  ggplot(aes(y=duration,x=frequency, fill = frequency) )+
  geom_bar(stat="identity", position = "dodge2")+
  facet_grid(. ~ condition)+
  theme_classic()+
  theme(text=element_text(size=16,  family="Helvetica"),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        strip.text.y = element_text(size = 20))
ggsave("07.figures/PR_bar_delta.tiff", units="in", width=5, height=4, dpi=200, compression = 'lzw')

plot_list <- list()
for(i in 2:max(PRANOVA$subject)){
  g<-Delta%>%
    filter(subject == i)%>%
    select(subject,condition,Dlt.5,Dlt.31)%>%
    gather(frequency,duration,3:4)%>%
    group_by(condition,frequency) %>%
    summarise_at(vars(duration), list(mean))%>%
    ggplot(aes(y=duration,x=frequency, fill = frequency) )+
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
ggsave("07.figures/PR_bar_delta_sub.tiff", units="in", width=5, height=4, dpi=200, compression = 'lzw')


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
ggsave("07.figures/PR_5Hz.tiff", units="in", width=5, height=4, dpi=200, compression = 'lzw')

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
ggsave("07.figures/PR_31Hz.tiff", units="in", width=5, height=4, dpi=200, compression = 'lzw')

# Anova CT  ----
a1 <- aov_ez("subject", "PR",PRANOVA,   within = c(  "condition","frequency"))
a1
a1m1<-emmeans(a1,pairwise~ condition,adjust="bonf")

# Anova Delta CT  ----
x<-Delta%>%
  select(subject,condition,Dlt.5,Dlt.31)%>%
  gather(frequency,duration,3:4)
a2 <- aov_ez("subject", "duration",x,   within = c(  "condition","frequency"))
a2
a2m1<-emmeans(a2,pairwise~ condition,adjust="bonf")


# results dataset
anova.pr<-a1
onova.delta <- a2
posthoc.pr1 <- a1m1
posthoc.delta <- a2m1
save(anova.pr,
     onova.delta,
     posthoc.pr1,
     posthoc.delta,
     file = "04.data/PR_results.RData")

#################################################
# 
# END
#
#################################################