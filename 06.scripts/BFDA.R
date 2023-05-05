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

load("04.data/BFDA.RData")




# The general workflow

# 1) Simulate many hypothetical studies, both under H1 and under H0, using the function BFDA.sim
# 2) Analyze the simulated studies, using the function BFDA.analyze
# 3) Plot the simulated studies (plot, SSD, ...)
# ref: https://rawgit.com/nicebread/BFDA/master/package/doc/BFDA_manual.html

## 1) Simulation -------------------
# parametres

ESH1          <-   c(0,0.2,0.4,0.5)# possible effect size (cohen's dz)

Type          <-    "t.paired" #expected.ES has to be provided as Cohen d
Alternative   <-    "greater"

Prior         <-    list("Cauchy",list(prior.location = 0,prior.scale = sqrt(2)/2)) #prior distribution for t-tests
Design        <-    "sequential" 

nmax          <-    120 #maximum sample size
nmin          <-    40 #initial sample size

Boundary      <-    6 # 6 is required by journal (BF 1/6, 6)

rep           <-    10000
Verbose       <-    TRUE 
Cores         <-    8
Stepsize      <-    20 #The number of observations added to the sample in each step of a sequential process


sim_list<-lapply(ESH1, function(es){
  es<-BFDA.sim(expected.ES = es, # BFDA simulation
               type = Type, 
               prior = Prior, 
               design = Design,
               n.max = nmax, 
               n.min = nmin,
               alternative = Alternative,
               boundary = Inf, 
               B = rep,verbose = Verbose, cores = Cores,stepsize = Stepsize)})



## 2) Analyze the simulated data ----

nlz_list<-lapply(sim_list, function(sim){
  BFDA.analyze(sim, design = Design, n.min = nmin, # BFDA analysis
               n.max = nmax, boundary = Boundary) })

# BFDA analysis outputs 
H0<-nlz_list[[1]] # H0 output (Es = 0)
H1_2<-nlz_list[[2]] # H01output (Es = 0.2)
H1_4<-nlz_list[[3]] # H1 output (Es = 0.4)
H1_5<-nlz_list[[4]] # H1 output (Es = 0.5)


## 3) Plots ----

# BFDA simulation extracted from simulation list 
SH0<-sim_list[[1]]
SH1_2<-sim_list[[2]]
SH1_4<-sim_list[[3]]
SH1_5<-sim_list[[4]]

# BFDA plots for H1 when ES = 0.4 and for H0 
plot(SH1_$, n.min=nmin, n.max=nmax, boundary=c(1/Boundary, Boundary))
plot(SH0, n.min=nmin, n.max=nmax, boundary=c(1/Boundary, Boundary), forH1 = FALSE)

SSD(SH1_4, power=.90, boundary=c(1/Boundary, Boundary))
SSD(SH0, alpha=.02, boundary=c(1/Boundary, Boundary))


# Plot figure 1 of manuscript:
# method is used from BFDA.analyse package script  
data_plot <- data.frame(matrix(nrow = 16, ncol = 4))
colnames(data_plot) = c("n","es","H1","H0")
r<-1
for(i in 1:4){
  for(j in 1:5){
    nb <- c(40,60,80,100,120) 
    es <- c(0,0.2,0.4,0.5)
    
    sim<-sim_list[[i]]$sim%>%
      filter(n <= nb[j])
    # reduce to *first* break of a boundary
    boundary.hit <- sim%>% group_by(id) %>%
      filter(logBF>=log(6) | logBF<=-log(6)) %>%
      filter(row_number()==1) %>% 
      ungroup()	%>%
      mutate(hitCondition = "boundary",
             all.traj.n = length(unique(id)))
    
    boundary.upper.traj.n <- length(unique(boundary.hit$id[boundary.hit$logBF>0]))
    boundary.lower.traj.n <- length(unique(boundary.hit$id[boundary.hit$logBF<0]))
    
    all.traj.n <- length(unique(sim$id))
    
    upper.hit.frac <- boundary.upper.traj.n/all.traj.n
    lower.hit.frac <- boundary.lower.traj.n/all.traj.n
    
    data_plot[r,1]<- nb[j]
    data_plot[r,2]<- es[i]
    data_plot[r,3]<- upper.hit.frac
    data_plot[r,4]<- lower.hit.frac
    
    r <- r+1
  }
}


plot<-data_plot%>%
  mutate(es = case_when( es == 0 ~"Cohen's dz = 0",
                         es == 0.2 ~"Cohen's dz = 0.2",
                         es == 0.4 ~"Cohen's dz = 0.4",
                         es == 0.5 ~"Cohen's dz = 0.5"))%>%
  gather("Hypothesis","percent",c(H1,H0))


plot%>%
  ggplot(aes(x=n, y= percent, color = Hypothesis, shape = Hypothesis))+
  geom_point(size = 3)+
  geom_line()+
  facet_grid(.~es)+
  theme(text=element_text(size=16,  family="Arial"),
        axis.line = element_line(colour = "black"),
        legend.position="bottom")+
  xlab("Sample size")+
  ylab("Studies terminating at a boundary (%)")




##save data ---

save(sim_list,nlz_list,data_plot,plot,file="04.data/BFDA.RData")

#################################################
# 
# END
#
#################################################