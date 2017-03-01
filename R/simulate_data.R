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
#' @param seed \emph{interpretted as integer or \code{NULL}}: defaults to \code{NULL}, for more details see \code{\link[base]{set.seed}}
#' @param ndays \emph{integer}: Number of trading days for which aggregated buys and sells are simulated, defaults to 60
#'
#' @export simulateBS

simulateBS <- function(param = NULL, seed = NULL, ndays = 60) {
  set.seed(seed)
  res <- simBS(param, ndays = ndays)
  res
}