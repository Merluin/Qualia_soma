#################################################
# 
# Experiment:     Qualiasoma_binocular_rivalry
# Programmer:     Thomas Quettier
# Date:           02/02/2022
# Description:    Data simulation from pilot for effect size estimation
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
library(faux)

## loading data ----

load("04.data/qualia_soma.RData")

# pilot dataset 
pilot<-rivalry_dataset%>%
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

  
## Simulation data ----
# Produces a data table with the same distributions and correlations as an existing data table Only returns numeric columns and simulates all numeric variables from a continuous normal distribution 
nrep <- 10000

##  cohen's dz from t.test: (happy/stimulation - happy/no_stimulation)-(neutral/stimulation - neutral/no_stimulation) vs. 0 

#data selection to simulate matrix of interest ( happy neutral stimuli)
flitered_pilot<- pilot%>%
  filter(condition == "emotion", percept != "mixed")%>%
  mutate(percept = ifelse(percept == "target","happy","neutral"))
original_data<-list(happy5 = flitered_pilot%>%filter(percept == "happy", frequency != 31),
                    happy31 = flitered_pilot%>%filter(percept == "happy", frequency != 5),
                    neutral5 = flitered_pilot%>%filter(percept == "neutral", frequency != 31),
                    neutral31 = flitered_pilot%>%filter(percept == "neutral", frequency != 5))

#data simulation
simulation_list <- lapply(original_data, function(sim){
  perceptname <- sim$percept[1]
  sim%>%
  mutate(frequency = ifelse(frequency > 0, 1,0))%>%
  select(subject,frequency,percept,duration)%>%
  sim_df(
    n = nrep,
    within = "frequency",
    id = "subject",
    dv = "duration",
    empirical = FALSE,
    long = TRUE
  )%>%
  'colnames<-'(c("subject","no.stim", "yes.stim"))%>%
  mutate(differences = no.stim-yes.stim,
         percept = perceptname)})

# simulated dataset
simulation_data<- list(full = rbind(simulation_list$happy5,simulation_list$happy31,simulation_list$neutral5,simulation_list$neutral31),
                       hz5 =  rbind(simulation_list$happy5,simulation_list$neutral5),
                       hz31 = rbind(simulation_list$happy31,simulation_list$neutral31))

# matrix reduction for t.test
diff_list<-lapply(simulation_data, function(sim){
  sim%>%
  select(subject,percept,differences)%>%
  group_by(subject,percept)%>%
  summarise_at(vars(differences),list(mean))%>%
  data.frame()%>%
  spread(percept,differences)%>%
  mutate(differences = happy-neutral)
  })

# t.test 
t_list<-lapply(diff_list, function(diff){
  t.test(diff$differences~1)
  })

# cohen's dz estimation
dz_list<-lapply(t_list, function(t){
  t$statistic/sqrt(nrep)
  })

##save data ---
save(simulation_list,simulation_data,diff_list,t_list,dz_list,file="04.data/simulated_data.RData")

#################################################
# 
# END
#
#################################################