#' Likelihood factorizations
#'
#' Evaluates likelihood function either utilizing factorization by Easley et. al (2010)
#' or Lin and Ke (2011).
#'
#' If names are not set for \code{param} or one or more of the vector names do not match the valid choices, they are internally set to
#' \code{'alpha'}, \code{'delta'}, \code{'epsilon_b'}, \code{'epsilon_s'}, \code{'mu'} (in this order). 
#' Vectors for \code{numbuys} and \code{numsells} need to have same length.
#'
#' @param param \emph{numeric}: (named) vector of model parameters
#'                              (valid names: \code{'alpha'}, \code{'delta'}, \code{'epsilon_b'}, \code{'epsilon_s'}, \code{'mu'}), 
#'                              length must equal 5
#' @param numbuys \emph{numeric} vector of daily buys
#' @param numsells \emph{numeric} vector of daily sells
#' @param factorization \emph{character}: switch between EHO ('EHO') and Lin-Ke ('Lin_Ke') factorization
#'
#' @return \emph{numeric}: likelihood function value
#'
#' @export
#'
#' @references
#' Easley, David et al. (2010) \cr
#' Factoring Information into Returns \cr
#' \emph{Journal of Financial and Quantitative Analysis}, Volume 45, Issue 2, pp. 293 - 309 \cr
#' \doi{10.1017/S0022109010000074}
#'
#' Lin, Hsiou-Wei William and Ke, Wen-Chyan (2011) \cr
#' A computing bias in estimating the probability of informed trading \cr
#' \emph{Journal of Financial Markets}, Volume 14, Issue 4, pp. 625 - 640 \cr
#' \doi{10.1016/j.finmar.2011.03.001}

pin_ll <- function(param = NULL, numbuys = NULL, numsells = NULL,
                   factorization = c("Lin_Ke", "EHO")) {
  if(is.null(numbuys)) stop("No number of daily buys given!")
  if(is.null(numsells)) stop("No number of daily sells given!")
  if(length(numbuys) != length(numsells)) stop("Buys and Sells length differ!")

  param <- param_check(param)
  factr <- match.arg(factorization)

  switch(factr,
         Lin_Ke = {
           pin_ll_lin_ke_5par(param = param, numbuys = numbuys, numsells = numsells)
         },
         EHO = {
           pin_ll_eho_5par(param = param, numbuys = numbuys, numsells = numsells)
         }
  )
}

#################################################
#
# Factorization used in EHO (2010) for EHO
#
#################################################

# log-likelihood factorization by Easley, Hvidkjaer and O'Hara (2010)
# http://journals.cambridge.org/action/displayAbstract?fromPage=online&aid=7785676&fileId=S0022109010000074

pin_ll_eho_5par <- function(param = NULL , numbuys = NULL, numsells = NULL) {
  n <- length(numbuys)
  m <- pmin.int(numbuys, numsells) + 0.5 * pmax.int(numbuys, numsells)
  xs <- param["epsilon_s"]/(param["mu"] + param["epsilon_s"])
  xb <- param["epsilon_b"]/(param["mu"] + param["epsilon_b"])

  exp_mu <- exp(-param["mu"])
  xs_helper <- xs ** (numsells - m)
  xb_helper <- xb ** (numbuys - m)

  prob_no <- 1.0 - param["alpha"]
  prob_good <- param["alpha"] * (1.0 - param["delta"])
  prob_bad <- param["alpha"] * param["delta"]

  part1 <- -n * (param["epsilon_b"] + param["epsilon_s"]) +
    (log(xb) + log(xs)) * sum(m) +
    log(param["mu"] + param["epsilon_b"]) * sum(numbuys) +
    log(param["mu"] + param["epsilon_s"]) * sum(numsells)

  part2 <- log(prob_no * xs_helper * xb_helper +
                 prob_good * exp_mu * xs_helper * xb ** (-m) +
                 prob_bad * exp_mu * xb_helper * xs ** (-m))

  ll <- part1 + sum(part2)
  names(ll) <- NULL
  ll
}

#################################################
#
# Factorization used in EHO (2010) for EKOP
#
#################################################

pin_ll_eho_4par <- function(param = NULL , numbuys = NULL, numsells = NULL) {
  n <- length(numbuys)
  m <- pmin.int(numbuys, numsells) + 0.5 * pmax.int(numbuys, numsells)
  x <- param["epsilon"]/(param["mu"] + param["epsilon"])

  exp_mu <- exp(-param["mu"])
  x_helper_sells <- x ** (numsells - 2 * m)
  x_helper_buys <- x ** (numbuys - 2 * m)

  prob_no <- 1.0 - param["alpha"]
  prob_good <- param["alpha"] * (1.0 - param["delta"])
  prob_bad <- param["alpha"] * param["delta"]

  part1 <- -2 * n * param["epsilon"] + 2 * log(x) * sum(m) +
    log(param["mu"] + param["epsilon"]) * sum(numbuys + numsells)

  part2 <- log(prob_no * x ** (numbuys + numsells - 2 * m) +
                 prob_good * exp_mu * x_helper_sells +
                 prob_bad * exp_mu * x_helper_buys)

  ll <- part1 + sum(part2)
  names(ll) <- NULL
  ll
}


#################################################
#
# Lin & Ke factorization for EHO
#
#################################################

pin_ll_lin_ke_5par <- function(param = NULL , numbuys = NULL, numsells = NULL) {
  # Computing likelihood function (Lin and Ke factorization)
  # http://www.sciencedirect.com/science/article/pii/S0378426611002433 (equation 5)

  # Preparing the computation, storing some often used values in variables
  # to avoid unnecessary operations

  n <- length(numbuys)

  rat1 <- param["mu"]/param["epsilon_s"]
  rat2 <- param["mu"]/param["epsilon_b"]

  rat1log1p <- log1p(rat1)
  rat2log1p <- log1p(rat2)

  const1 <- log(param["mu"] + param["epsilon_s"])
  const2 <- log(param["mu"] + param["epsilon_b"])

  prob_no <- 1.0 - param["alpha"]
  prob_good <- param["alpha"] * (1.0 - param["delta"])
  prob_bad <- param["alpha"] * param["delta"]

  # First step to stabilize

  e1 <- -param["mu"] - numsells * rat1log1p
  e2 <- -param["mu"] - numbuys * rat2log1p
  e3 <- e2 + e1 + 2 * param["mu"]

  e_max <- pmax.int(e1, e2, e3)

  # computing the two parts of the log-likelihood

  part1 <- -n * (param["epsilon_b"] + param["epsilon_s"]) + sum(numbuys) * const2 +
            sum(numsells) * const1 + sum(e_max)

  part2 <- log(prob_no * exp(e3-e_max) + prob_good * exp(e1 - e_max) + prob_bad * exp(e2 - e_max))

  # value for the log-likelihood

  ll <- part1 + sum(part2)

  names(ll) <- NULL

  ll
}

#################################################
#
# Lin & Ke factorization for EKOP
#
#################################################


pin_ll_lin_ke_4par <- function(param = NULL , numbuys = NULL, numsells = NULL) {
  # Computing likelihood function (Lin and Ke factorization)
  # http://www.sciencedirect.com/science/article/pii/S0378426611002433 (equation 5)

  # Preparing the computation, storing some often used values in variables
  # to avoid unnecessary operations

  n <- length(numbuys)

  rat <- param["mu"]/param["epsilon"]

  ratlog1p <- log1p(rat)

  const <- log(param["mu"] + param["epsilon"])

  prob_no <- 1.0 - param["alpha"]
  prob_good <- param["alpha"] * (1.0 - param["delta"])
  prob_bad <- param["alpha"] * param["delta"]

  # First step to stabilize

  e1 <- -param["mu"] - numsells * ratlog1p
  e2 <- -param["mu"] - numbuys * ratlog1p
  e3 <- e2 + e1 + 2 * param["mu"]

  e_max <- pmax.int(e1, e2, e3)

  # computing the two parts of the log-likelihood

  part1 <- -2 * n * param["epsilon"] + const * sum(numbuys + numsells) + sum(e_max)

  part2 <- log(prob_no * exp(e3-e_max) +
               prob_good * exp(e1 - e_max) +
               prob_bad * exp(e2 - e_max))

  # value for the log-likelihood

  ll <- part1 + sum(part2)

  names(ll) <- NULL

  ll
}
