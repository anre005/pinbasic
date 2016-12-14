#' PIN confidence intervals
#'
#' Computes confidence intervals for the probability of informed trading by simulation
#'
#' If names are not set for \code{param} or one or more of the vector names do not match the valid choices, they are internally set to
#' \code{'alpha'}, \code{'delta'}, \code{'epsilon_b'}, \code{'epsilon_s'}, \code{'mu'} (in this order). \cr
#' The \pkg{foreach}, \pkg{doParallel} and \pkg{parallel} packages are utilized for computation of confidence intervals in parallel.
#' By default, \code{ncores} is set to \code{\link[parallel]{detectCores}} which attempts to utilize all available cores for computation.
#' This task may be slow and time-consuming on older or single-core machines.
#'
#' @inheritParams pin_ll
#' @inheritParams simulateBS
#' @inheritParams pin_est_core
#' @param n \emph{integer}: Number of simulation runs, defaults to 10000
#' @param level \emph{numeric}: Confidence level, defaults to 0.95
#' @param ncores \emph{integer}: Number of cpu cores utilized in computation, defaults to \code{\link[parallel]{detectCores}}
#'
#' @return \emph{numeric}: confidence interval
#'
#' @import foreach
#' @import doParallel
#' @import parallel
#' @importFrom iterators iter
#'
#' @export pin_confint
#'
pin_confint <- function(param = NULL, numbuys = NULL, numsells = NULL,
                        lower = rep(0, 5), upper = c(1,1, rep(Inf, 3)),
                        n = 10000, seed = NULL, level = 0.95, ncores = detectCores()) {
  param <- param_check(param)
  if(!is.numeric(ncores) && ncores < 1) stop("No valid 'ncores' argument!")
  if(length(numbuys) != length(numsells)) stop("Unequal lengths for 'numbuys' and 'numsells'")
  if(!is.null(seed)) {
    set.seed(seed)
  } else {
    seed <- sample(1e5,1)
  }

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

  sim_dat <- vector("list", n)
  seed_seq <- seq(from = seed, to = seed + (n - 1))

  for(i in 1:n) {
    sim_dat[[i]] <- simulateBS(param = param_dat, seed = seed_seq[i], ndays = ndays)
  }

  if(ncores == 1) {
    par_est <- vector("list", n)

    initial_mat <- lapply(sim_dat, function(x) {initial_vals(numbuys = x[,"Buys"],
                                                             numsells = x[, "Sells"],
                                                             method = "HAC")})

    for(i in 1:n) {
      par_est[[i]] <- nlminb(start = initial_mat[i,],
                             objective = function(x) -fn(x, sim_dat[[i]][,"Buys"], sim_dat[[i]][,"Sells"]),
                             lower = lower, upper = upper)$par
    }

    sim_pin <- unlist(lapply(par_est, function(x) pin_calc(x)))
  } else {
    itx <- iter(sim_dat)
    cl <- makeCluster(ncores)
    registerDoParallel(cl)

    sim_pin <- foreach(i = itx, .combine = "c") %dopar% {
      init_vals <- initial_vals(numbuys = i[,"Buys"], numsells = i[,"Sells"],
                                method = "HAC")

      param <- nlminb(start = init_vals[1,], objective = function(x) -fn(x, i[,"Buys"], i[,"Sells"]),
                      lower = lower, upper = upper)$par

      pin_calc(param)
    }

    stopCluster(cl)
  }
  conf <- quantile(sim_pin, probs = c((1-level)/2, 1 - (1-level)/2))
  conf
}