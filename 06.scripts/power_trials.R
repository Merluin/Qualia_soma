###########################################################################
#
#  Experiment:  Qualia_soma
#  Programmer:  QUETTIER THOMAS / FILIPPO
#  Date:        02/2022
#  Description: Power analysis + trials
#
#  https://github.com/Merluin/Qualia_soma
###########################################################################

rm(list=ls()) # remove all objects

# Packages ----------------------------------------------------------------

library(tidyverse)
library(dplyr)
library(ggplot2)
library(stringr)
library(EMT)
library(nnet)

# parameters ----------------------------------------------------------------

es <- c(0.2, 0.478)
sample_size <- c(10, 30, 50)
trials <- 30 * 24
nsim <- 10
Prob <- c(0.5,0.3,0.2) 	

# simulation ----------------------------------------------------------------

sim <- expand_grid(es, sample_size, trials, nsim = 1:nsim)
t_list <- vector(mode = "list", length = nrow(sim))


for(i in 1:nrow(sim)){
  
  g1 <- sample(1:3, size = sim$sample_size[i]*trials, replace = TRUE, prob = Prob)
  multinom(test ~ 1)
  table(test)
  
  #t_list[[i]] <- multinomial.test(g1$Freq, g1$Prob)
}

sim$t_list <- t_list

# plot ----------------------------------------------------------------

sim %>% 
  mutate(p_value = map_dbl(t_list, function(x) x$p.value),
         is_sign = ifelse(p_value <= 0.05, 1, 0)) %>% 
  pivot_longer(9:10, names_to = "is", values_to = "value") %>% 
  mutate(type = ifelse(str_detect(is, "is_sign"), "freq", "bayes")) %>% 
  group_by(es, sample_size) %>% 
  summarise(power = mean(value)) %>% 
  ggplot(aes(x = sample_size, y = power, color = factor(es))) +
  geom_point(size = 3) +
  geom_line() +
  facet_wrap(~type)+
  theme_classic()+
  theme(text=element_text(size=16,  family="Helvetica"),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        strip.text.y = element_text(size = 20))


###########################################################################
#                                   END                                   #
###########################################################################