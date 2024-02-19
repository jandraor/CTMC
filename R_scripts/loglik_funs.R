mif_ll <- function(mf_list, Np = 100000, seed, n_cores, filename) {
  
  if(!file.exists(filename)) {
    
    registerDoRNG(seed)
    registerDoParallel(cores = n_cores)
    
    message(paste0("Number of working cores: "), getDoParWorkers())
    
    message(paste0("Starting at ", date()))
    
    tic.clearlog()
    tic()
    
    foreach(mf = mf_list, .combine = c,
            .errorhandling = 'pass') %dopar% {
              
              library(pomp)
              
              out <- tryCatch({
                replicate(10, mf %>% pfilter(Np = Np) |>  logLik()) |> 
                  logmeanexp(se = TRUE) -> ll
                
                mf |>  coef() |>  bind_rows() |> 
                  bind_cols(loglik = ll[1],loglik.se = ll[2]) -> ll_df
                list(ll_df)
              },
              error = function(cond) {
                error_list <- list(list(error = cond))
                return(error_list)
              })
              
            } -> ll_results
    
    toc(quiet = FALSE, log = TRUE)
    log.lst <- tic.log(format = FALSE)
    results <- list(ll_results = ll_results, time = log.lst)
    saveRDS(results, filename)
  } else {
    results <- readRDS(filename)
  }
  results
}

global_search <- function(guesses, fixed_params, mf1, fn, seed, n_cores) {
  
  if(!file.exists(fn)) {
    message("...Starting global search...")
    
    registerDoParallel(cores = n_cores)
    registerDoRNG(seed)
    
    message(paste0("Number of working cores: "), getDoParWorkers())
    
    tic.clearlog()
    tic()
    
    foreach(guess = iter(guesses,"row"), .combine = c, 
            .errorhandling = 'pass') %dopar% {
              
              library(pomp)
              
              out <- tryCatch({
                mf1 |>
                  mif2(params = c(unlist(guess), fixed_params)) |> 
                  mif2(Nmif = 100) -> mf
                
                list(mf)
              },
              error = function(cond) {
                error_list <- list(list(guess = unlist(guess),
                                        error = cond))
                return(error_list)
              })
              
              return(out)
            } -> mf_results
    
    toc(quiet = FALSE, log = TRUE)
    log.lst <- tic.log(format = FALSE)
    results <- list(mif_results = mf_results, time = log.lst)
    saveRDS(results, fn)
  } else {
    results <- readRDS(fn)  
  }
  
  results
}

extract_mif_results <- function(mif_output) {
  n_iter <- length(mif_output$mif_results)
  
  sapply(mif_output$mif_results, function(x) class(x))-> class_vector
  
  valid_indexes <- which(class_vector == "mif2d_pomp")
  
  valid_results <- mif_output$mif_results[valid_indexes]
  
  n_valid <- length(valid_results)
  
  message(str_glue("{n_valid} out of {n_iter} are valid"))
  
  # needs to be improved
  
  mif_output <- c(valid_results[[1]], valid_results[[2]])
  
  for(i in 3:length(valid_results)) {
    mif_output <- c(mif_output, valid_results[[i]])
  }
  
  mif_output
}

extract_ll_df <- function(ll_output) {
  n_iter <- length(ll_output$ll_results)
  
  map_lgl(ll_output$ll_results, function(x) {
    result_class <- class(x)
    
    "data.frame" %in% result_class
  }) -> valid_indexes
  
  valid_results <- ll_output$ll_results[valid_indexes]
  
  n_valid <- length(valid_results)
  
  message(str_glue("{n_valid} out of {n_iter} are valid"))
  
  do.call("rbind", valid_results)
}

iter_filt_profile <- function(mf1, guesses, fixed_params, perturbations, 
                              filename, seed, n_cores = 7) {
  
  if(!file.exists(filename)) {
    
    registerDoRNG(seed)
    registerDoParallel(cores = n_cores)
    
    message(paste0("Number of working cores: "), getDoParWorkers())
    
    message(paste0("Starting at ", date()))
    
    tic.clearlog()
    tic()
    
    foreach(guess = iter(guesses,"row"), .combine = c,
            .errorhandling = 'pass') %dopar% {
              library(dplyr)
              library(pomp)
              
              out <- tryCatch({
                mf1 |> 
                  mif2(params = c(unlist(guess),fixed_params),
                       rw.sd = perturbations) |> 
                  mif2(Nmif = 100, cooling.fraction.50 = 0.3) -> mf
                list(mf)
              },
              error = function(cond) {
                error_list <- list(list(guess = unlist(guess),
                                        error = cond))
                return(error_list)
              })
              
              return(out)
            } -> mif_results
    toc(quiet = FALSE, log = TRUE)
    log.lst <- tic.log(format = FALSE)
    results <- list(mif_results = mif_results, time = log.lst)
    saveRDS(results, filename)
  } else {
    results <- readRDS(filename)
  }
  results
}