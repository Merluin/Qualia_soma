#################################################
# 
# Experiment:     Qualiasoma_binocular_rivalry
# Programmer:     Thomas Quettier
# Date:           16/06/2021
# Description:    BFDA
#
#################################################

############### Parameters ----
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

load("04.data_preprocessing/qualia_soma.RData")

# SIMULATION https://rawgit.com/nicebread/BFDA/master/package/doc/BFDA_manual.html


nmin<-10
nmax<-15

sim.H1 <- BFDA.sim(expected.ES=0.2, type="t.paired",
                   prior=list("Cauchy",list(prior.location=0, prior.scale=sqrt(2)/2)),
                   design="sequential", n.max=nmax, alternative="greater",boundary=Inf, B=10000,
                   verbose=TRUE, cores=11, stepsize = 10)

BFDA.analyze(sim.H1, design="sequential", n.min=nmin, n.max = nmax, boundary=6)

sim.H0 <- BFDA.sim(expected.ES=0,  type="t.paired",
                   prior=list("Cauchy",list(prior.location=0, prior.scale=sqrt(2)/2)),
                   design="sequential", n.max=nmax, alternative="greater", boundary=Inf, B=10000,
                   verbose=TRUE, cores=11, stepsize = 10)
BFDA.analyze(sim.H0, design="sequential", n.min=nmin, n.max = nmax, boundary=6)


plot(sim.H1, n.min=nmin, n.max=nmax, boundary=c(1/6, 6))
plot(sim.H0, n.min=nmin, n.max=nmax, boundary=c(1/6, 6), forH1 = FALSE)

SSD(sim.H1, power=.80, boundary=c(1/6, 6))
SSD(sim.H0, alpha=.02, boundary=c(1/6, 6))





