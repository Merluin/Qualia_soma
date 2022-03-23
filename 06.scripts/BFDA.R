#################################################
# 
# Experiment:     Qualiasoma_binocular_rivalry
# Programmer:     Thomas Quettier
# Date:           16/06/2021
# Description:    open-ended BFDA
#
#################################################

rm(list=ls())
## library ----
library(dplyr)
library(tidyverse)
library(ggplot2)
library(ggeffects)
library(afex)
library(hrbrthemes)
library(emmeans)
library(BFDA)

## loading data ----

load("04.data_preprocessing/BFDA.RData")


## parametres ----

ESH1          <-    0.478 #we can also provide a vector; rnorm(100000, 0.5, 0.1)
ESH0          <-    0

Type          <-    "t.paired" #expected.ES has to be provided as Cohen’s d
Alternative   <-    "greater"

Prior         <-    list("Cauchy",list(prior.location = 0,prior.scale = sqrt(2)/2)) #prior distribution for t-tests
Design        <-    "sequential" # otherwise fixed-N 

nmax          <-    300 #maximum sample size
nmin          <-    20 #initial sample size

Boundary      <-    6 # 6 is required by cortex

rep           <-    10000
Verbose       <-    TRUE 
Cores         <-    11
Stepsize      <-    10 #The number of observations added to the sample in each step of a sequential process


## Simulation, both under H1 and under H0  ----
# SIMULATION https://rawgit.com/nicebread/BFDA/master/package/doc/BFDA_manual.html

sim.H1 <- BFDA.sim(expected.ES = ESH1, type = Type, prior = Prior, design = Design,
                   n.max = nmax, alternative = Alternative,boundary = Inf, 
                   B = rep,verbose = Verbose, cores = Cores,stepsize = Stepsize)

sim.H0 <- BFDA.sim(expected.ES = ESH0, type = Type, prior = Prior, design = Design,
                   n.max = nmax, alternative = Alternative,boundary = Inf, 
                   B = rep,verbose = Verbose, cores = Cores,stepsize = Stepsize) 


## Analyze the simulated data ----

nlz.H1 <- BFDA.analyze(sim.H1, design = Design, n.min = nmin,
                       n.max = nmax, boundary = Boundary) 

nlz.H0 <- BFDA.analyze(sim.H0,design = Design, n.min = nmin,
                       n.max = nmax, boundary = Boundary)



## Plots ----

plot(sim.H1, n.min=nmin, n.max=nmax, boundary=c(1/Boundary, Boundary))
plot(sim.H0, n.min=nmin, n.max=nmax, boundary=c(1/Boundary, Boundary), forH1 = FALSE)

SSD(sim.H1, power=.90, boundary=c(1/Boundary, Boundary))
SSD(sim.H0, alpha=.02, boundary=c(1/Boundary, Boundary))



##save data ---

save(sim.H1,sim.H0,nlz.H1,nlz.H0,file="04.data_preprocessing/BFDA.RData")

#################################################
# 
# END
#
#################################################