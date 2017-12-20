#' PIN confidence intervals
#'
#' Computes confidence intervals for the probability of informed trading by simulation
#'
#' If names are not set for \code{param} or one or more of the vector names do not match the valid choices, they are internally set to
#' \code{'alpha'}, \code{'delta'}, \code{'epsilon_b'}, \code{'epsilon_s'}, \code{'mu'} (in this order). \cr
#' By default, only one core is utilized in computations (\code{ncores} = 1).
#' Confidence intervals can also be calculated in parallel, however,
#' this only pays off for large values of \code{n}.
#'
#' @inheritParams pin_ll
#' @inheritParams simulateBS
#' @inheritParams pin_est_core
#' @inheritParams initial_vals
#' @param n \emph{integer}: Number of simulation runs, defaults to 10000
#' @param seed \emph{interpreted as integer or \code{NULL}}: defaults to \code{NULL}, for more details see \link[base]{set.seed}
#' @param level \emph{numeric}: Confidence level, defaults to 0.95
#' @param ncores \emph{integer}: Number of cpu cores utilized in computation, defaults to 1
#'
#' @return \emph{numeric}: confidence interval
#'
#' @importFrom stats nlminb quantile
#' @importFrom parallel makeCluster stopCluster clusterCall
#'
#' @export pin_confint
#'
pin_confint <- function(param = NULL, numbuys = NULL, numsells = NULL,
                        method = "HAC",
                        lower = rep(0, 5), upper = c(1,1, rep(Inf, 3)),
                        n = 10000, seed = NULL, level = 0.95,
                        ncores = 1) {
  param <- param_check(param)
  if(!is.numeric(ncores) && ncores < 1) stop("No valid 'ncores' argument!")
  if(length(numbuys) != length(numsells)) stop("Unequal lengths for 'numbuys' and 'numsells'")
  meth <- match.arg(method, choices = c("HAC", "HAC_Ref", "Grid"))
  set.seed(seed)

  sim_pin <- numeric(n)

  ndays <- length(numbuys)

  fn <- function(par, buys, sells) {
    pin_ll(param = par,
           numbuys = buys, numsells = sells,
           factorization = "Lin_Ke")
  }

  if(is.null(param)) {
    init_vals <- initial_vals(numbuys = numbuys, numsells = numsells,
                              method = "HAC")

    param_dat <- nlminb(start = init_vals[1,], objective = function(x) -fn(x, numbuys, numsells),
                 lower = lower, upper = upper)$par
  } else param_dat <- param

  sim_dat <- replicate(n = n, simulateBS(param = param_dat, ndays = ndays), simplify = FALSE)

  initial_mat <- lapply(sim_dat, function(x) {initial_vals(numbuys = x[,"Buys"],
                                                           numsells = x[, "Sells"],
                                                           method = meth)})
  if(ncores == 1) {
    par_est <- Map(function(x,y) nlminb(start = y[1,],
                                        objective = function(par) -fn(par, x[,"Buys"], x[,"Sells"]),
                                        lower = lower, upper = upper)$par,
                   x = sim_dat, y = initial_mat)
  } else {
    cl <- makeCluster(getOption("cl.cores", ncores))

    split_ind <- split(seq_len(n), seq_len(ncores))

    cl_export(cl, sim_dat, initial_mat, split_ind)

    par_est <- clusterCall(cl, fun = ci_mc_helper, fn = fn, lower = lower, upper = upper)

    par_est <- do.call(c, par_est)

    on.exit(stopCluster(cl))
  }
  sim_pin <- sapply(par_est, function(x) pin_calc(x))
  conf <- quantile(sim_pin, probs = c((1-level)/2, 1 - (1-level)/2))
  conf
}


#################################################
#
# helper function for parallel computation
#
#################################################

ci_mc_helper <- function(fn = NULL, lower = NULL, upper = NULL) {
  Map(function(x,y) nlminb(start = y[1,],
                           objective = function(par) -fn(par, x[,"Buys"], x[,"Sells"]),
                           lower = lower, upper = upper)$par,
      x = get("data_sub", envir = .GlobalEnv), y = get("init_sub", envir = .GlobalEnv))
}

cl_export <- function(cl = NULL, sim_data = NULL, init_mat = NULL, split_ind = NULL) {
  for (i in seq_along(cl)) {
    clusterCall(cl[i], function(data, init) {
      assign_to_global('data_sub', data)
      assign_to_global('init_sub', init)
      NULL  # don't return any data to the master
    }, data = sim_data[split_ind[[i]]], init = init_mat[split_ind[[i]]])
  }
}

assign_to_global <- function(string, object, pos=1){
  assign(string, object, envir=as.environment(pos))
}