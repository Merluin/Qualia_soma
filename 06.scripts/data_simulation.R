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
library(Matrix)
library(faux)

## loading data ----

load("04.data/qualia_soma.RData")

# pilot dataset 
pilot<-rivalry_dataset%>%
  select(subject,block,condition, trial,Hz, key,emotion,  dur)%>%
  filter(trial < 13, subject > 1)%>% # subject 1 is male participant and excluded.
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

# simulation parameters
n = 10000
within = c("percept","frequency")
id = "subject"
dv = "duration"
data <- long2wide(data = flitered_pilot, within = within, dv = dv, id = id)
mat<-nearPD(cor(data, use = "complete.obs"))
mat<-mat$mat

#data simulation from faux package script.
sim<-rnorm_multi(
  n = n,
  vars = ncol(data),
  mu = sapply(data, mean, na.rm = TRUE),
  sd = sapply(data, sd, na.rm = TRUE),
  r = mat,
  varnames = names(data)
)

# matrix reduction for t.test
sim.diff<-sim%>%
  mutate(diff.neu5 = neutral_5 - neutral_0,
         diff.neu31 = neutral_31 - neutral_0,
         diff.hap5 = happy_5 - happy_0,
         diff.hap31 = happy_31 - happy_0)%>%
  mutate(diff.5 = diff.hap5 - diff.neu5,
         diff.31 = diff.hap31 - diff.neu31)%>%
  select(subject,diff.5,diff.31)

# t.test 
t.diff.5 <- t.test(sim.diff$diff.5 ~1)
t.diff.31 <- t.test(sim.diff$diff.31~1)

# cohen's dz estimation
t.diff.5$statistic/sqrt(n)
t.diff.31$statistic/sqrt(n)

# save data ---
save(flitered_pilot,sim,sim.diff,t.diff.5,t.diff.31,file="04.data/simulated_data.RData")

#################################################
# 
# END
#
#################################################