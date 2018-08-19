#' Estimating PIN
#'
#' Estimates the probability of informed trading (PIN) for daily buys and sells trading data for arbitrary number of trading days.
#'
#' User-friendly wrapper around workhorse function \code{pin_est_core}.
#' \code{\link{nlminb}} function in the \pkg{stats} package is used for maximization.
#' In the literature, at least data for 60 trading days is recommended to ensure convergence of optimization.
#' No information about the trading days' dates is needed.
#' Vectors for \code{numbuys} and \code{numsells} need to have same length. \cr
#' Calculation of confidence interval for the probability of informed trading is disabled by default.
#' For more details see \code{\link{pin_est_core}} or \code{\link{pin_confint}}.
#'
#' @inheritParams pin_ll
#' @inheritParams pin_est_core
#' @inheritParams pin_confint
#' @param ci_control \emph{list}: see \code{\link{pin_est_core}}
#'
#' @seealso \code{\link{nlminb}},
#'          \code{\link{initial_vals}}
#'          \code{\link{pin_est_core}}
#'          \code{\link{qpin}}
#'
#' @references
#' Easley, David et al. (2002) \cr
#' Is Information Risk a Determinant of Asset Returns? \cr
#' \emph{The Journal of Finance}, Volume 57, Number 5, pp. 2185 - 2221 \cr
#' \doi{10.1111/1540-6261.00493}
#'
#' Easley, David et al. (1996) \cr
#' Liquidity, Information, and Infrequently Traded Stocks\cr
#' \emph{The Journal of Finance}, Volume 51, Number 4, pp. 1405 - 1436 \cr
#' \doi{10.1111/j.1540-6261.1996.tb04074.x}
#'
#' Easley, David et al. (2010) \cr
#' Factoring Information into Returns \cr
#' \emph{Journal of Financial and Quantitative Analysis}, Volume 45, Issue 2, pp. 293 - 309 \cr
#' \doi{10.1017/S0022109010000074}
#'
#' Ersan, Oguz and Alici, Asli (2016) \cr
#' An unbiased computation methodology for estimating the probability of informed trading (PIN) \cr
#' \emph{Journal of International Financial Markets, Institutions and Money}, Volume 43, pp. 74 - 94 \cr
#' \doi{10.1016/j.intfin.2016.04.001}
#'
#' Gan, Quan et al. (2015) \cr
#' A faster estimation method for the probability of informed trading
#' using hierarchical agglomerative clustering \cr
#' \emph{Quantitative Finance}, Volume 15, Issue 11, pp. 1805 - 1821 \cr
#' \doi{10.1080/14697688.2015.1023336}
#'
#' Lin, Hsiou-Wei William and Ke, Wen-Chyan (2011) \cr
#' A computing bias in estimating the probability of informed trading \cr
#' \emph{Journal of Financial Markets}, Volume 14, Issue 4, pp. 625 - 640 \cr
#' \doi{10.1016/j.finmar.2011.03.001}
#'
#' Yan, Yuxing and Zhang, Shaojun (2012) \cr
#' An improved estimation method and empirical properties of the probability of informed trading \cr
#' \emph{Journal of Banking & Finance}, Volume 36, Issue 2, pp. 454 - 467 \cr
#' \doi{10.1016/j.jbankfin.2011.08.003}
#'
#' @return
#' A list with the following components:
#' \describe{
#' \item{Results}{Matrix containing the parameter estimates as well as their estimated standard errors,
#'  t-values and p-values.}
#'  \item{ll}{Value of likelihood function returned by \code{nlminb}}
#'  \item{pin}{Estimated probability of informed trading}
#'  \item{conv}{Convergence code for nlminb optimization}
#'  \item{message}{Convergence message returned by the nlminb optimizer}
#'  \item{iterations}{Number of iterations until convergence of nlminb optimizer}
#'  \item{init_vals}{Vector of initial values}
#'  \item{confint}{If \code{confint = TRUE}; confidence interval for the probability of informed trading}
#'  }
#'
#' @examples
#' # Loading simulated data for frequently traded stock
#'
#' data("BSfrequent")
#'
#' # Optimization with HAC initial values and Lin-Ke likelihood factorization
#'
#' pin_freq <- pin_est(numbuys = BSfrequent[,"Buys"],
#'                     numsells = BSfrequent[,"Sells"])
#' @export

pin_est <- function(numbuys = NULL, numsells = NULL, nlminb_control = list(),
                    confint = FALSE, ci_control = list(), posterior = TRUE) {
  if(is.null(numbuys)) stop("Missing data for 'numbuys'")
  if(is.null(numsells)) stop("Missing data for 'numsells'")
  if(length(numbuys) != length(numsells)) stop("Unequal lengths for 'numbuys' and 'numsells'")

  init_vals <- initial_vals(numbuys = numbuys, numsells = numsells, method = "HAC")

  res <- pin_est_core(numbuys = numbuys, numsells = numsells,
                      factorization = "Lin_Ke",
                      init_vals = init_vals,
                      nlminb_control = nlminb_control,
                      confint = confint, ci_control = ci_control,
                      posterior = posterior)
  res
}