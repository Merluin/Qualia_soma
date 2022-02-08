###########################################################################
#
#  Experiment:  Qualia_soma
#  Programmer:  QUETTIER THOMAS / Filippo
#  Date:        02/2022
#  Description: sensitive analysis
#
#   
###########################################################################

rm(list=ls()) # remove all objects

# Packages ----------------------------------------------------------------

library(tidyverse)
library(dplyr)
library(BayesFactor)
library(ggplot2)
library(stringr)
source("05.functions/progress.R")

# parameters ----------------------------------------------------------------

es <- seq(0.3, 0.7, 0.05)
sample_size <- c(30, 40, 50, 60)
bf_threshold = 6
nsim <- 10000

# simulation ----------------------------------------------------------------

sim <- expand_grid(es, sample_size, bf_threshold, nsim = 1:nsim)
bf_list <- vector(mode = "list", length = nrow(sim))
t_list <- vector(mode = "list", length = nrow(sim))

for(i in 1:nrow(sim)){
  g1 <- rnorm(sim$sample_size[i], sim$es[i], 1)
  bf_list[[i]] <- ttestBF(g1, mu = 0)
  t_list[[i]] <- t.test(g1, mu = 0)
  progress(nrow(sim), i)
}

sim$bf_list <- bf_list
sim$t_list <- t_list

# plot ----------------------------------------------------------------

sim %>% 
  mutate(bf = map_dbl(bf_list, function(x) exp(x@bayesFactor[["bf"]])),
         p_value = map_dbl(t_list, function(x) x$p.value),
         is_above = ifelse(bf >= bf_threshold, 1, 0),
         is_sign = ifelse(p_value <= 0.05, 1, 0)) %>% 
  pivot_longer(9:10, names_to = "is", values_to = "value")%>% 
  mutate(type = ifelse(str_detect(is, "is_sign"), "frequentist", "bayesian")) %>% 
  group_by(es, sample_size, type) %>% 
  summarise(power = mean(value)) %>% 
  ggplot(aes(x = es, y = power,color = factor(sample_size))) +
  geom_point(size = 3) +
  geom_line() +
  geom_hline(yintercept = 0.80, linetype="dashed", color = "gray")+
  geom_vline(xintercept = 0.654, linetype="dashed", color = "gray")+
  geom_vline(xintercept = 0.478, linetype="dashed", color = "gray")+
  facet_wrap(~type)+
  ggtitle("sensitive analysis")+
  xlab("effect size") + 
  labs(color = "sample size")+
  theme_classic()+
  theme(text=element_text(size=16,  family="Helvetica"),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        strip.text.y = element_text(size = 20))

 ggsave("07.figures/sanalysis.tiff", units="in",  dpi=200, compression = 'lzw')



###########################################################################
#                                   END                                   #
###########################################################################