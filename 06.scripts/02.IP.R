#################################################
# 
# Experiment:     tidbitting chicks
# Programmer:     Thomas Quettier
# Date:           22/09/2020
# Description:    spontaneous choice analysis
#
#################################################

rm(list=ls())
# Packages ----------------------------------------------------------------

library(tidyverse)
library(lme4)
library(broom.mixed)

load("04.data_preprocessing/qualia_soma.RData")


# Setting the factors reference level

onset_dataset <- onset_dataset %>%
  filter(subject >= 6,subject !=14)%>%
  drop_na(initial_percept) %>% 
  mutate(initial_percept = factor(initial_percept),
         stimulation = factor(stimulation)) %>% 
  tibble()

# Relevel the mimicry factor

onset_dataset$stimulation = relevel(onset_dataset$stimulation, ref = "no") # reference level mimicry-free
onset_dataset$initial_percept_01 = ifelse(onset_dataset$initial_percept == "happy", 1,0) # 1 is happy and 0 is neutral

# Models Fitting -----------------------------------------------------------

fit <- glmer(initial_percept_01 ~ stimulation + (1|subject),
             data = onset_dataset,
             family = binomial(link = "logit"))

tidy(fit)

confint(fit)


table(onset_dataset$stimulation,onset_dataset$initial_percept)
#################################################
# 
# END
#
#################################################