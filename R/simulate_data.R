#' @import foreach
#' @import doParallel
#' @import parallel
#' @import stats

simulate_data <- function(param = NULL, seed = NULL, days = 60) {
  if(!is.null(seed)) set.seed(seed)
  # actual sampling
  param <- param_check(param)
  states <- sample(c("no", "good", "bad"),
                   size = days, replace = TRUE,
                   prob = c(1 - param["alpha"],
                            param["alpha"] * (1 - param["delta"]),
                            param["alpha"] * param["delta"]))

  # indices for no-, good- and bad-news days
  ind_no <- which(states == "no")
  ind_good <- which(states == "good")
  ind_bad <- which(states == "bad")

  len_no <- length(ind_no)
  len_good <- length(ind_good)
  len_bad <- length(ind_bad)

  buys <- sells <- numeric(days)

  # drawing Poisson distributed random numbers for daily buys and sells
  # according to the actual buy and sell intensities depending on the
  # condition of the trading day

  buys[ind_no] <- rpois(len_no, param["epsilon_b"])
  sells[ind_no] <- rpois(len_no, param["epsilon_s"])

  buys[ind_good] <- rpois(len_good, param["epsilon_b"] + param["mu"])
  sells[ind_good] <- rpois(len_good, param["epsilon_s"])

  buys[ind_bad] <- rpois(len_bad, param["epsilon_b"])
  sells[ind_bad] <- rpois(len_bad, param["epsilon_s"] + param["mu"])

  res <- cbind(buys, sells)
  colnames(res) <- c("Buys", "Sells")
  res
}

pin_confint <- function(param = NULL, numbuys = NULL, numsells = NULL,
                        lower = rep(0, 5), upper = c(1,1, rep(Inf, 3)),
                        n = 1000, seed = NULL, level = 0.95, ncores = detectCores()) {
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
    sim_dat <- replicate(n, simulate_data(param = param_dat, seed = NULL, days = ndays), simplify = "matrix")

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
      sim_dat <- simulate_data(param = param_dat, seed = NULL, days = ndays)

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
