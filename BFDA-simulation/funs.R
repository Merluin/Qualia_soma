silent <- function(fun, ...){
    suppressMessages({fun(...)})
}

between <- function(x, left, right){
    left <= x & x <= right
}

# Generate a dataset given main effect and interaction or cell means

generate_data <- function(n, main_effect_face = NULL, int_face_stim = NULL, sigma_pop = 1, rho, mus = NULL, .id = NULL){
    
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
    
    if(is.null(.id)){
        dat$id <- 1:nrow(dat)
    }else{
        dat$id <- .id
    }
    
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
    to_optimize <- function(x) abs(get_f_interaction(get_cells(main_effect_face, x), rho) - desired_int_f)
    optimize(to_optimize, interval = c(0, 5))$minimum
}

# compute a single simulation. the while loop stops when bf is greater than 6 or less than 1/6

do_simulation <- function(main_effect_face, sd_effect_face = 0,
                          int_face_stimolation, sd_effect_interaction = 0,
                          rho = 0.5,
                          start_n, max_n, step = 1, bf_bound = 6){
    
    # Setup Simulation
    log_bf_bound <- log(bf_bound) # log it's easier
    main_effect_face_i <- rnorm(1, main_effect_face, sd_effect_face) # main effect
    int_face_stimolation_i <- rnorm(1, int_face_stimolation, sd_effect_interaction) # interaction
    if(int_face_stimolation == 0){
        int_i = 0
    }else{
        int_i <- find_optimal_int(main_effect_face, int_face_stimolation_i, rho = rho) # find the raw int
    }
    mus <- get_cells(main_effect_face_i, int_i) # cell values
    dat <- generate_data(start_n, mus = mus, rho = rho) # initial data
    
    # Starting values
    bfi <- 0
    ni <- start_n
    i <- 0
    
    ni_res <- vector(mode = "integer", length = max_n)
    bf_res <- vector(mode = "numeric", length = max_n)
    p_res <- vector(mode = "numeric", length = max_n)
    f_res <- vector(mode = "numeric", length = max_n)
    
    repeat {
        i <- i + 1
        if(i != 1){ # increase if not the first iteration
            dat <- increase_n(dat, mus, ni, step = step)
        }
        bfi <- fit_anova_bf(dat)
        freq_res <- fit_anova_car(dat)
        ni_res[i] <- ni
        bf_res[i] <- bfi
        p_res[i] <- freq_res$p
        f_res[i] <- freq_res$es_f
        
        if(between(bfi, -log_bf_bound, log_bf_bound) & ni < max_n){
            ni <- ni + 1
        }else{
            break
        }
    }
    
    result <- list(
        n = ni,
        ni = ni_res,
        mus = mus,
        #dat = dat,
        main_effect_face = main_effect_face_i,
        int_face_stimolation = int_face_stimolation_i,
        bf = bf_res,
        pvalue = p_res,
        cohen_f = f_res
    )
    
    return(result)
    
}

# fitting anova and returning the interaction

fit_anova_bf <- function(data){
    fit <- silent(BayesFactor::anovaBF,
                  value ~ face * stimolation + id, 
                  whichRandom = "id", 
                  progress = FALSE,
                  multicore = TRUE,
                  data = data)
    bf10 <- fit[4]/fit[3] # model with interaction / model without interaction
    bf10@bayesFactor[["bf"]]
}

fit_anova_car <- function(data){
    fit <- silent(afex::aov_car,
                  value ~ face * stimolation + Error(id/face*stimolation),
                  data = data,
                  anova_table = list(es = "pes"))
    
    pvalue <- fit$anova_table$`Pr(>F)`[3]
    es_pes <- fit$anova_table$pes[3]
    es_f <- sqrt(es_pes / (1 - es_pes))
    return(list(p = pvalue, es_f = es_f))
}

increase_n <- function(data, mus, ni, step){
    new_n <- generate_data(step, mus = mus, rho = rho, .id = ni)
    rbind(data, new_n)
}
