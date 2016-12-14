#################################################
#
# Check for boundary solutions
#
#################################################

bound_hit <- function(param = NULL, lower = NULL, upper = NULL) {
  low.hit <- abs(lower - param) < 1e-10
  upper.hit <- abs(upper - param) < 1e-10
  bound.hit <- (low.hit | upper.hit)
  bound.hit
}

#################################################
#
# Covariance matrix for CAR models
#
#################################################

vcov_car <- function(param = NULL, numbuys = NULL, numsells = NULL,
                     factorization = NULL,
                     lower = NULL, upper = NULL, eigentol = 1e-12) {

  fun <- function(par) {
    pin_ll(par, numbuys = numbuys, numsells = numsells,
           factorization = factorization)
  }

  bound.hit <- bound_hit(param, lower, upper)
  names.param <- names(param)
  param.bound <- param[bound.hit]
  join_param <- function(x) c(param.bound,x)[names.param]

  hess <- tryCatch(stats::optimHess(fn = function(x) fun(join_param(x)), par = param[!bound.hit]),
                   error = function(e) e)

  if(is.matrix(hess)) {
    if(any(is.nan(hess)) | any(is.na(hess)) | any(is.infinite(hess))) {
      warning("NaN, NA or infinite values in Hesse matrix")
      return(NULL)
    }
  } else return(NULL)

  hess_eigen <- abs(eigen(hess, symmetric = TRUE, only.values = TRUE)$values)
  vcov_mat <- matrix(0, length(param[!bound.hit]), length(param[!bound.hit]))
  rownames(vcov_mat) <- colnames(vcov_mat) <- names(param[!bound.hit])

  if (min(hess_eigen) > (eigentol * max(hess_eigen))) {
    vcov_mat <- solve(-hess)
    vcov_mat <- (vcov_mat + t(vcov_mat))/2
  }
  else {
    vcov_mat <- NULL
    warning("Singular Hesse matrix")
  }
  vcov_mat
}

#################################################
#
# Function to produce summary.lm - like outputs
# with std.err, t-vals and p-vals
#
#################################################

summary_car <- function(param = NULL, numbuys = NULL, numsells = NULL,
                        factorization = NULL,
                        lower = NULL, upper = NULL, eigentol = 1e-12) {
  boundary <- bound_hit(param, lower, upper)

  vcov <- vcov_car(param = param, numbuys = numbuys, numsells = numsells,
                   factorization = factorization,
                   lower = lower, upper = upper, eigentol = eigentol)
  if(is.null(vcov)) return(NULL)

  if(any(diag(vcov) < 0) | any(is.infinite(vcov))) {
    warning("Infeasible Variance-Covariance Matrix with negative variances or infinite entries!")
    return(NULL)
  }
  std_err <- t_vals <- p_vals <- numeric(length(param))
  std_err[!boundary] <- sqrt(diag(vcov))
  std_err[boundary] <- NA
  t_vals[!boundary] <- param[!boundary]/std_err[!boundary]
  p_vals[!boundary] <- 2 * stats::pnorm(-abs(t_vals[!boundary]))
  t_vals[boundary] <- NA
  p_vals[boundary] <- NA
  results <- cbind(Estimate = param, `Std. error` = std_err, `t value` = t_vals, `Pr(> t)` = p_vals)
  results
}
