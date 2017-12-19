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
#' @param numbuys \emph{numeric}: vector of daily buys
#' @param numsells \emph{numeric}: vector of daily sells
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
                   factorization = "Lin_Ke") {
  if(is.null(numbuys)) stop("No number of daily buys given!")
  if(is.null(numsells)) stop("No number of daily sells given!")
  if(length(numbuys) != length(numsells)) stop("Buys and Sells length differ!")

  param <- param_check(param)
  factr <- match.arg(factorization, choices = c("Lin_Ke", "EHO"))

  switch(factr,
         Lin_Ke = {
             linke(param = param, numbuys = numbuys, numsells = numsells)
         },
         EHO = {
             eho(param = param, numbuys = numbuys, numsells = numsells)
         }
  )
}