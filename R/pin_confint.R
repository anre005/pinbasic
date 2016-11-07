#' PIN confidence intervals
#' 
#' Computes confidence intervals for the probability of informed trading by Monte-Carlo simulation
#' 
#' If names are not set for \code{param} or one or more of the vector names do not match the valid choices, they are internally set to
#' \code{'alpha'}, \code{'delta'}, \code{'epsilon_b'}, \code{'epsilon_s'}, \code{'mu'} (in this order). \cr
#' The \pkg{foreach}, \pkg{doParallel} and \pkg{parallel} packages are utilized for computation of confidence intervals in parallel. 
#' By default, \code{\link[parallel]{detectCores}} attempts to get the number of available cores.
#' This task may be slow and time-consuming on older or single-core machines. 
#' 
#' @inheritParams pin_ll
#' @inheritParams simulate_data
#' @inheritParams pin_est_core
#' @param n \emph{integer}: Number of simulation runs, defaults to 5000
#' @param level \emph{numeric}: Confidence level, defaults to 0.95
#' @param ncores \emph{integer}: Number of cpu cores utilized in computation, defaults to \code{\link[parallel]{detectCores}}
#' 
#' @return \emph{numeric}: confidence interval
#' 
#' @export pin_confint
#' 
pin_confint <- function(param = NULL, numbuys = NULL, numsells = NULL,
                        lower = rep(0, 5), upper = c(1,1, rep(Inf, 3)),
                        n = 5000, seed = NULL, level = 0.95, ncores = detectCores()) {
  if(!is.numeric(ncores) && ncores < 1) stop("No valid 'ncores' argument!")
  if(!is.null(seed)) set.seed(seed)
  sim_pin <- numeric(n)
  
  ndays <- length(numbuys)
  
  fn <- function(x, buys, sells) {
    pin_ll(param = x,
           numbuys = buys, numsells = sells,
           factorization = "Lin_Ke")
  }
  
  if(is.null(param)) {
    init_vals <- initial_vals(numbuys = numbuys, numsells = numsells,
                              method = "HAC")
    
    tmp <- nlminb(start = init_vals[1,], objective = function(x) -fn(x, numbuys, numsells),
                  lower = lower, upper = upper)
    
    param_dat <- tmp$par
  } else param_dat <- param
  
  if(ncores == 1) {
    buys_ind <- 1:ndays
    sells_ind <- (ndays + 1):(2*ndays)
    init_ind <- (2*ndays + 1):(2*ndays + 5)
    sim_dat <- replicate(n, simulate_data(param = param_dat, seed = NULL, ndays = ndays), simplify = "matrix")
    
    initial_mat <- apply(sim_dat, 2, function(x) {initial_vals(numbuys = x[buys_ind],
                                                               numsells = x[sells_ind],
                                                               method = "HAC")})
    comb <- rbind(sim_dat, initial_mat)
    
    param <- apply(comb, 2, function(z) { nlminb(start = z[init_ind],
                                                 objective = function(x) -fn(x, z[buys_ind], z[sells_ind]),
                                                 lower = lower, upper = upper)$par})
    
    sim_pin <- apply(param, 2, pin_calc)
  } else {
    cl <- makeCluster(ncores)
    registerDoParallel(cl)
    
    sim_pin <- foreach(i = 1:n, .combine = "c") %dopar% {
      sim_dat <- simulate_data(param = param_dat, seed = NULL, ndays = ndays)
      
      init_vals <- initial_vals(numbuys = sim_dat[,"Buys"], numsells = sim_dat[,"Sells"],
                                method = "HAC")
      
      param <- nlminb(start = init_vals[1,], objective = function(x) -fn(x, sim_dat[,"Buys"], sim_dat[,"Sells"]),
                      lower = lower, upper = upper)$par
      
      pin_calc(param)
    }
    stopCluster(cl)
  }
  conf <- quantile(sim_pin, probs = c((1-level)/2, 1 - (1-level)/2))
  conf
}