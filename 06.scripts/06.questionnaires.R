#################################################
# 
# Experiment:     Qualiamotion_binocular_rivalry
# Programmer:     Thomas Quettier
# Date:           05/05/2020
# Description:    Questionaires Correlations analysis
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
library(PerformanceAnalytics)

############### loading data ----

load("04.data_preprocessing/qualia_soma.RData")

############### Data
# Questionnaire ----
Questionnaire<-Questionnaires%>%
  mutate(subject = as.numeric(as.character(id)))%>%
  select(id, age,fantasy,personal_distress,perspective_taking,empathic_concern,iri_tot,tas_tot)

#age
age_pilot<-c(23,23,40,23)
mean(age_pilot)
sd(age_pilot)


# IRI TAS summary
IRI<-Questionnaire%>%
  select(id, iri_tot)%>%
  summarise_at(vars(iri_tot), list(mean,sd))%>%
  'colnames<-'(c( "mean", "Sd"))

TAS<-Questionnaire%>%
  select(id, tas_tot)%>%
  summarise_at(vars( tas_tot), list(mean,sd))%>%
  'colnames<-'(c( "mean", "Sd"))
age
IRI
TAS

questionnaire<-Questionnaire%>%
  select(fantasy,perspective_taking,empathic_concern,personal_distress,iri_tot,tas_tot)
  
questionnaire<-questionnaire[-9,]
  

# Valuation ----
val<-valuation_dataset %>%
  filter(subject >= 6)%>%
  mutate(valence = case_when(valence == 1 ~ -3,
                             valence == 2 ~ -2,
                             valence == 3 ~ -1,
                             valence == 4 ~  0,
                             valence == 5 ~  1,
                             valence == 6 ~  2,
                             valence == 7 ~  3,))%>%
  select( subject,stimulation, emotion,  valence, arousal)%>%
  group_by(subject,stimulation,emotion )%>%
  summarise_at(vars(valence,arousal), list(mean))%>%

  gather(cat,val,-c(subject,stimulation,emotion))%>%
  mutate(cond=paste0(cat,".",stimulation,".",emotion))%>%
  group_by(subject,cond,val) %>%
  select(subject,cond,val)%>%
  spread(cond,val)
val<-cbind(questionnaire,val[-9,-1])

# OR ----
OR<-onset_dataset%>%
  filter(subject >= 6)%>%
  drop_na()%>%
  select(subject, stimulation, initial_percept, onset)%>%
  'colnames<-'(c("subject", "stimulation", "initial_pt", "onset"))%>%
  group_by(subject,stimulation,initial_pt) %>%
  summarise_at(vars(onset), list(mean))%>%
  as.data.frame()%>%
  mutate(cond=paste0("OR.",stimulation,".",initial_pt))%>%
  group_by(subject,cond,onset) %>%
  select(subject,cond,onset)%>%
  spread(cond,onset)
OR<-cbind(questionnaire,OR[-9,-1])

# IP ----
IP<-onset_dataset %>%
  filter(subject >= 6)%>%
  select(subject, stimulation, initial_percept, onset)%>%
  mutate(freq = case_when(onset>=1 ~ 1))%>%
  na.omit()%>%
  group_by(subject,stimulation, initial_percept) %>%
  summarise_at(vars(freq), list(sum))%>%
  as.data.frame()%>%
  mutate(cond=paste0("IP.",stimulation,".",initial_percept))%>%
  group_by(subject,cond,freq) %>%
  select(subject,cond,freq)%>%
  spread(cond,freq)
IP<-cbind(questionnaire,IP[-9,-1])

# Cumulative Time ----

  CT<-rivalry_dataset%>%
  filter(subject >= 6)%>%
  select(subject, stimulation, key,emotion, trial,  duration)%>%
  group_by(subject, stimulation, emotion, trial)%>%
  summarise_at(vars(duration), list(sum))%>%
  spread(emotion,duration,fill=0)%>%
  select(subject ,stimulation ,trial, happy, mixed, neutral)%>%
  gather(emotion,duration,4:6)%>%
  as.data.frame()%>%
  group_by(subject,stimulation,emotion) %>%
  summarise_at(vars(duration), list(mean))%>%
  mutate(cond= paste0("CT.",stimulation,".",emotion))%>%
  group_by(subject) %>%
  select( subject,cond,duration)%>%
  spread(cond,duration)

CT<-cbind(questionnaire,CT[-9,-c(1:4)])




############### Plot cor ----
jpeg("07.figures/cor_val_exp3.jpg", units="in", width=10, height=8, res=200)
val<-val%>%
  'colnames<-'(c("IRI.F", "IRI.PT", "IRI.EC", "IRI.PD" ,"IRI.TOT","TAS.TOT" ,"AR.BLO.HPY", "AR.BLO.NEU", "AR.FRE.HPY", "AR.FRE.NEU","VAL.BLO.HPY", "VAL.BLO.NEU", "VAL.FRE.HPY" ,"VAL.FRE.NEU"))
chart.Correlation(val, histogram=FALSE, pch=19,method ="pearson")
dev.off()

jpeg("07.figures/cor_ort_exp3.jpg", units="in", width=10, height=8, res=200)
OR<-OR%>%
  'colnames<-'(c("IRI.F", "IRI.PT", "IRI.EC", "IRI.PD" ,"IRI.TOT","TAS.TOT" ,"ORT.BLO.HPY", "ORT.BLO.NEU", "ORT.FRE.HPY", "ORT.FRE.NEU"))
chart.Correlation(OR, histogram=FALSE, pch=19,method ="pearson")
dev.off()

jpeg("07.figures/cor_ip_exp3.jpg", units="in", width=10, height=8, res=200)
IP<-IP%>%
  'colnames<-'(c("IRI.F", "IRI.PT", "IRI.EC", "IRI.PD" ,"IRI.TOT","TAS.TOT" ,"IP.BLO.HPY", "IP.BLO.NEU", "IP.FRE.HPY", "IP.FRE.NEU"))
chart.Correlation(IP, histogram=FALSE, pch=19,method ="pearson")
dev.off()
p<-cor.test(IP[,2],IP[,3])
p.adjust(p$p.value, method = "fdr", n = 4)
p<-cor.test(IP[,2],IP[,5])
p.adjust(p$p.value, method = "fdr", n = 4)


jpeg("07.figures/cor_CT_exp3.jpg", units="in", width=10, height=8, res=200)
CT<-CT%>%
  'colnames<-'(c("IRI.F", "IRI.PT", "IRI.EC", "IRI.PD" ,"IRI.TOT","TAS.TOT" ,"CT.BLO.HPY", "CT.BLO.MIX", "CT.BLO.NEU", "CT.FRE.HPY","CT.FRE.MIX", "CT.FRE.NEU"))
chart.Correlation(CT, histogram=FALSE, pch=19,method ="pearson")
dev.off()

#################################################
# 
# END
#
#################################################