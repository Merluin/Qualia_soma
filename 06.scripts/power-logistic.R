# Packages ----------------------------------------------------------------

library(tidyverse)
library(lme4)
library(ggplot2)

# Functions ---------------------------------------------------------------

get_p_value <- function(fit){
  fits <- summary(fit)
  fits$coefficients[2, 4] #pvalue of the odds ratio
}

# Parameters --------------------------------------------------------------

set.seed(2022)

nsample <- c(30, 35, 40, 45, 50)
ntrials <- c(12, 24, 36,  48, 60)
b0 <- qlogis(0.5) # probability of happy free. You set the probability but the model want the logit
b1 <- c(0.3, 0.7, 1.4) # log odds ratio for the conversion to cohen's d see https://www.escal.site/ ~ 0.2, 0.5, 0.8
cond <- c("free", "stimulated")
tau_id <- c(0.1, 0.5, 0.8) # random intercept i.e. how much the happy-free probability is different among subject (in log scale)
# in order to see the effect of tau on the intercept you can do hist(plogis(rnorm(1000, b0, tau_id))) and see the range of values
nsim <- 1000

# Generate Data -----------------------------------------------------------

sim <- expand_grid(
  nsample,
  ntrials,
  b0,
  b1,
  tau_id,
  nsim = 1:nsim
)

# Simulation --------------------------------------------------------------

p_value <- vector(mode = "numeric", length = nrow(sim))

for(i in 1:nrow(sim)){
  b0_i <- rnorm(sim$nsample[i], 0, sim$tau_id[i]) # intercepts
  dat_i <- tibble(id = 1:sim$nsample[i], b0_i) # make subj data
  sim_i <- expand_grid(dat_i, trial = 1:sim$ntrials[i], cond) # create the trial-by-trial dataset
  sim_i$cond <- factor(sim_i$cond, levels = c("free", "stimulated")) # set free as baseline
  sim_i$cond_e <- ifelse(sim_i$cond == "free", 0, 1) # 0-1 version for the model
  sim_i$linpred <- plogis((sim$b0[i] + sim_i$b0_i) + sim$b1[i]*sim_i$cond_e) # get the predicted probability
  sim_i$p <- rbinom(nrow(sim_i), 1, sim_i$linpred) # get the 0-1 trial (0 = neutral, 1 = happy)
  
  # fit the model
  fit <- glmer(p ~ cond + (1|id), data = sim_i, family = binomial(link = "logit"))
  
  # extract p-value
  p_value[i] <- get_p_value(fit)
  
  save(i,
     sim_i,
     p_value,
     file = "04.data/temp.RData")
}


# Post-Processing ---------------------------------------------------------

sim$p_value <- p_value

sim <- sim %>% 
  group_by(nsample, ntrials, b1, tau_id) %>% # all sim conditions
  summarise(power = mean(p_value < 0.05)) # calculate power


# Plotting ----------------------------------------------------------------
sim%>%
  filter(b1== 0.3)%>%
  group_by(b1, nsample, ntrials) %>% 
  summarise(power = mean(power)) %>% 
  ggplot(aes(x=nsample, y=power, color=as.factor(ntrials)))+
  geom_point(size = 3) +
  geom_line()+
  facet_wrap(~b1)



# ...