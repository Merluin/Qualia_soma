# Packages ----------------------------------------------------------------



# Functions ---------------------------------------------------------------

# Generate a dataset given main effect and interaction or cell means

generate_data <- function(n, main_effect_face = NULL, int_face_stim = NULL, sigma_pop = 1, rho, mus = NULL){
    
    if(is.null(mus)){
        # compute all cell means
        mus <- get_cells(main_effect_face, int_face_stim)
    }
    
    # generate correlated data
    dat <- faux::rnorm_multi(n = n, 
                             mu = mus,
                             sd = 1,
                             r = rho, 
                             varnames = names(mus),
                             empirical = FALSE)
    
    dat$id <- 1:nrow(dat)
    
    # wide to long
    
    datl <- reshape2::melt(dat, id.vars = "id")
    datl$face <- factor(ifelse(grepl("h_", datl$variable), "h", "n"))
    datl$stimolation <- factor(ifelse(grepl("_ns", datl$variable), "ns", "s"))
    datl$id = factor(datl$id)
    
    # sum contrasts
    
    contrasts(datl$face) <- contr.sum(2)/2
    contrasts(datl$stimolation) <- contr.sum(2)/2
    
    return(datl)
}

# return the analytical cohen f for the interaction 
# see https://webpower.psychstat.org/wiki/_media/grant/webpower_manual_book.pdf page 140

get_f_interaction <- function(mus, rho){
    gm <- mean(mus)
    mat <- matrix(mus, nrow = 2, byrow = TRUE)
    marg_row <- rowMeans(mat)
    marg_col <- colMeans(mat)
    
    cells <- c(
        mus[1] - marg_row[1] - marg_col[1] + gm,
        mus[2] - marg_row[1] - marg_col[2] + gm,
        mus[3] - marg_row[2] - marg_col[1] + gm,
        mus[4] - marg_row[2] - marg_col[2] + gm
    )
    
    # sd(int)/sd(error) * C
    sqrt(mean(cells^2)) * sqrt(length(cells)/(1 - rho))
}

# return cell means given the main effect and the interaction

get_cells <- function(main_effect_face, int_face_stim){
    n_s <- 0 
    n_ns <- 0
    h_ns <- main_effect_face - int_face_stim/2
    h_s <- int_face_stim + h_ns
    cells <- c(h_s = h_s, h_ns = h_ns, n_s = n_s, n_ns = n_ns)
    return(cells)
}

# find the optimal interaction value (in the raw data scale) that generate a given cohen f

find_optimal_int <- function(main_effect_face, desired_int_f, rho){
    grid <- seq(0, 5, 0.0001)
    opt <- sapply(grid, function(i) abs(get_f_interaction(get_cells(main_effect_face, i), rho) - desired_int_f))
    grid[which.min(opt)]
}

# compute a single simulation. the while loop stops when bf is greater than 6 or less than 1/6

do_simulation <- function(main_effect_face, sd_effect_face = 0,
                          int_face_stimolation, sd_effect_interaction = 0,
                          rho = 0.5,
                          start_n, max_n, step = 1, bf_bound = 6){
    
    log_bf_bound <- log(bf_bound) # log it's easier
    bfi <- 0
    ni <- start_n
    
    bf_within_bound <- (-log_bf_bound < bfi) & (bfi < log_bf_bound)
    n_lower_nmax <- ni <= max_n
    
    while(bf_within_bound & n_lower_nmax){
        # generate 1 effect size from priors
        main_effect_face_i <- rnorm(1, main_effect_face, sd_effect_face)
        int_face_stimolation_i <- rnorm(1, int_face_stimolation, sd_effect_interaction)
        dat <- generate_data(ni, main_effect_face_i, int_face_stimolation_i, rho = rho)
        bfi <- fit_anova_bf(dat)
        ni <- ni + step
        bf_within_bound <- (-log_bf_bound < bfi) & (bfi < log_bf_bound)
        n_lower_nmax <- ni <= max_n
    }
    
    return(c(bf = bfi, n = ni))
    
}

# fitting anova and returning the interaction

fit_anova_bf <- function(data){
    fit <- BayesFactor::anovaBF(value ~ face * stimolation + id, 
                                whichRandom = "id", 
                                progress = FALSE,
                                data = data)
    bf10 <- fit[4]/fit[3] # model with interaction / model without interaction
    bf10@bayesFactor[["bf"]]
}


# Setup Simulation --------------------------------------------------------

main_effect_face <- 0.5 # cohen's d
sd_effect_face <- 0.05

int_face_stimolation <- 0.1 # cohen's f
sd_effect_interaction <- 0.05

rho <- 0.5 # the correlation between repeated measures
int_face_stimolation_raw <- find_optimal_int(main_effect_face, int_face_stimolation, rho = rho)
mus <- get_cells(main_effect_face, int_face_stimolation_raw) # cell means
bf_bound <- 6 # evidence for h1 or h0
start_n <- 10
max_n <- 80

# simulate

cl <- makeCluster(parallel::detectCores())
registerDoParallel(cl)

sim <- foreach(i = 1:nsim) %dopar% {
    do_simulation(m_main_face, sd_main_face = 0, m_int_face_stim, sd_int_face_stim = sd_effect_interaction,
                  start_n, max_n, step = 1, rho = 0.5, bf_bound = 6)
}

# saving

saveRDS(sim, "sim_results.rds")