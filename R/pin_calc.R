#' Probability of Informed Trading
#'
#' Calculates the probability of informed trading.
#'
#' If names are not set for \code{param} or one or more of the vector names do not match the valid choices, they are internally set to
#' \code{'alpha'}, \code{'delta'}, \code{'epsilon_b'}, \code{'epsilon_s'}, \code{'mu'} (in this order).
#'
#' @inheritParams pin_ll
#'
#' @return \emph{numeric}: probability of informed trading
#'
#' @references
#'  Easley, Hvidkjaer and O'Hara (2002), \cr
#'  Is Information Risk a Determinant of Asset Returns?, \cr
#'  \emph{The Journal of Finance}, Volume 57, Number 5, pp. 2185 - 2221
#'
#'  Easley, Kiefer, O'Hara and Paperman (1996), \cr
#'  Liquidity, Information, and Infrequently Traded Stocks,\cr
#'  \emph{The Journal of Finance}, Volume 51, Number 4, pp. 1405 - 1436
#'
#' @export

pin_calc <- function(param = NULL) {
  param <- param_check(param)

  res <- (param["alpha"] * param["mu"])/
         (param["alpha"] * param["mu"] + param["epsilon_b"] + param["epsilon_s"])

  names(res) <- NULL
  res
}
