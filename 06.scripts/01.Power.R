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
effect<- .478 # medium effect
directedeffect = "greater"

###Test ----
pwr.t.test( d = effect, n =7, sig.level = 0.05 , type = "paired", alternative = directedeffect)
