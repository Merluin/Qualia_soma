# Packages ----------------------------------------------------------------

library(foreach)
library(doSNOW)

# Functions ---------------------------------------------------------------

source("BFDA-simulation/funs.R")

# Setup Simulation --------------------------------------------------------

main_effect_face <- 0.5 # cohen's d
sd_effect_face <- 0

int_face_stimolation <- 0.2 # cohen's f
sd_effect_interaction <- 0.05

rho <- 0.5 # the correlation between repeated measures
bf_bound <- 6 # evidence for h1 or h0
start_n <- 10
max_n <- 80
nsim <- 10000

# simulate

cl <- makeCluster(parallel::detectCores())
registerDoSNOW(cl)
iterations <- nsim
pb <- txtProgressBar(max = iterations, style = 3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress = progress)

simh1 <- foreach(i = 1:nsim, .options.snow = opts) %dopar% {
    do_simulation(main_effect_face, sd_effect_face = sd_effect_face, int_face_stimolation, sd_effect_interaction,
                  rho = rho, bf_bound = bf_bound, start_n = start_n, max_n = max_n)
}

parallel::stopCluster(cl)

# saving

saveRDS(simh1, "sim_h1_results.rds")