#################################################
# 
# Experiment:     Qualiasoma_binocular_rivalry
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

load("04.data/qualia_soma.RData")


# Setting the factors reference level

onset_dataset <- onset_dataset %>%
  filter(subject>1)%>%
  drop_na(initial_percept) %>% 
  mutate(initial_percept = factor(initial_percept),
         stimulation = factor(stimulation)) %>% 
  tibble()

onset_emotion <- onset_dataset%>%filter(condition == "emotion")
onset_control <- onset_dataset%>%filter(condition == "control")

# Relevel the mimicry factor

onset_emotion$stimulation = relevel(onset_emotion$stimulation, ref = "0") # reference level mimicry-free
onset_emotion$initial_percept_01 = ifelse(onset_emotion$initial_percept == "happy", 1,0) # 1 is happy and 0 is neutral
onset_control$stimulation = relevel(onset_control$stimulation, ref = "0") # reference level mimicry-free
onset_control$initial_percept_01 = ifelse(onset_control$initial_percept == "baffi", 1,0) # 1 is happy and 0 is neutral

# Models Fitting -----------------------------------------------------------

fit_emotion <- glmer(initial_percept_01 ~ stimulation + (1|subject),
             data = onset_emotion,
             family = binomial(link = "logit"))
fit_control <- glmer(initial_percept_01 ~ stimulation + (1|subject),
                     data = onset_control,
                     family = binomial(link = "logit"))

tidy(fit_emotion)

confint(fit_emotion)

tidy(fit_control)

confint(fit_control)


table(onset_emotion$stimulation,onset_emotion$initial_percept)
table(onset_control$stimulation,onset_control$initial_percept)

#################################################
# 
# END
#
#################################################