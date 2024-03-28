#################################################
# 
# Experiment:     Qualia_soma
# Programmer:     Thomas Quettier
# Date:           11/05/2022
# Description:    power analysis
#
#################################################
library(pwr)

###Parameters ---- 

effect<- 0.47 # or replace with cohen d 0.5 is a medium effect

directedeffect = "greater"

###Test ----
pwr.t.test( d = effect, power = 0.95, sig.level = 0.02 , type = "paired", alternative = directedeffect)

pwr.r.test(d = effect, r = 4, sig.level = 0.05, power = 0.95,
           alternative = "two.sided")
