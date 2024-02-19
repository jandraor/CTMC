local_search <- function(pomp_mdl, ptb, fn, seed, n_cores) {
  
  if(!file.exists(fn)) {
    tic.clearlog()
    tic()
    registerDoParallel(cores = n_cores)
    registerDoRNG(seed)
    
    foreach(i = 1:20,.combine = c) %dopar% {
      
      library(pomp)
      
      pomp_mdl |> 
        mif2(Np = 2000, Nmif = 50,
             cooling.fraction.50 = 0.5,
             rw.sd = ptb) 
    } -> mifs_local
    
    toc(quiet = FALSE, log = TRUE)
    log.lst <- tic.log(format = FALSE)
    result_list  <- list(result = mifs_local, time = log.lst)
    saveRDS(result_list, fn)
  } else {
    result_list <- readRDS(fn)
  }
  result_list
}