#' Simulate trading data
#'
#' Simulates a matrix consisting of synthetic data for daily buys and sells
#'
#' If names are not set for \code{param} or one or more of the vector names do not match the valid choices, they are internally set to
#' \code{'alpha'}, \code{'delta'}, \code{'epsilon_b'}, \code{'epsilon_s'}, \code{'mu'} (in this order).
#'
#' @return \emph{numeric}: Matrix with \code{ndays} rows and two columns which are named \code{'Buys'} and \code{'Sells'}.
#'
#' @inheritParams pin_ll
#' @param seed \emph{interpretted as integer}: setting seed for RNG, defaults to \code{NULL}; for more detail see
#'             \code{\link[base]{set.seed}}
#' @param ndays \emph{integer}: Number of trading days for which aggregated buys and sells are simulated, defaults to 60
#' @importFrom stats rpois
#'
#' @export simulateBS

simulateBS <- function(param = NULL, seed = NULL, ndays = 60) {
  if(!is.null(seed)) set.seed(seed)
  days <- as.integer(ndays)
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