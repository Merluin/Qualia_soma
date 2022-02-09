#################################################
# 
# Experiment:     Qualia_soma
# Programmer:     Thomas Quettier
# Date:           30/03/2021
# Description:    power analysis
#
#################################################
library(pwr)

###Parameters ----
effect<- 0.4 # medium effect
directedeffect = "two.sided"

###Test ----
pwr.t.test( n = 50, power = 0.80, sig.level = 0.05 , type = "paired", alternative = directedeffect)
