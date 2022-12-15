#################################################
# 
# Experiment:     Qualia_soma
# Programmer:     Thomas Quettier
# Date:           11/05/2022
# Description:    power analysis
#
#################################################
library(pwr)

eta <- 0.2  # insert eta square 
  
###Parameters ---- 
d <- 2*sqrt(eta / (1 - eta))

effect<- 0.2 # or replace with cohen d 0.5 is a medium effect

directedeffect = "greater"

###Test ----
pwr.t.test( d = effect, power = 0.90, sig.level = 0.02 , type = "paired", alternative = directedeffect)


nbgroup <- 2 # number of group in anova
pwr.anova.test(f = eta,k = ndgroup, power = 0.90, sig.level = 0.05)
