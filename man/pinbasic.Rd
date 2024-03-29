% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pinbasic.R
\docType{package}
\name{pinbasic}
\alias{pinbasic}
\title{A Package for Fast and Stable Estimation of the Probability of Informed Trading (PIN)}
\description{
Utilities for fast and stable
estimation of the probability of informed trading (PIN) in the model introduced by Easley, Hvidkjaer and O'Hara (EHO, 2002) are implemented.
Since the model developed by Easley, Kiefer, O'Hara and Paperman (EKOP, 1996) is nested in the EHO model due to
equating the intensity of uninformed buys and sells, functionalities can also be applied
to this simpler model structure, if needed.
State-of-the-art factorization of the model likelihood function as well as hierarchical agglomerative clustering algorithm
for generating initial values for optimization routines are provided.
In total, two different likelihood factorizations and three methodologies generating starting values are implemented.
The probability of informed trading can be estimated for arbitrary length of daily buys and sells data with
\code{\link{pin_est}} function which is a wrapper around the workhorse function \code{\link{pin_est_core}}.
No information about the time span of the underlying data is required to perform optimizations.
However, recommendation given in the literature is using at least data for 60 trading days to ensure convergence of the
likelihood maximization.
The \code{\link{qpin}} function delivers quarterly estimates.
The number of available quarters in the data are detected utilizing functions from the \code{\link[lubridate]{lubridate}} package.
Quarterly estimates can be visualized with the \code{\link[ggplot2]{ggplot}} function.
Datasets of daily aggregated numbers of buys and sells can be simulated with \code{\link{simulateBS}}.
Calculation of confidence intervals for the probability of informed trading can be enabled by \code{confint} argument in
optimization routines (\code{\link{pin_est_core}}, \code{\link{pin_est}} and \code{\link{qpin}}) or by calling \code{\link{pin_confint}} directly.
Additionally, posterior probabilities for conditions of trading days can be computed with \code{\link{posterior}} and
plotted with \code{\link[ggplot2]{ggplot}}.
}
\section{Functions}{

\describe{
 \item{\code{\link{ggplot.posterior}}}{Visualization method for results of \code{\link{posterior}} with ggplot2.}
 \item{\code{\link{ggplot.qpin}}}{Visualization method for results of \code{\link{qpin}} with ggplot2.}
 \item{\code{\link{initial_vals}}}{Generating initial values by brute force grid search, hierarchical agglomerative clustering algorithm or
                                   refined hierarchical agglomerative clustering technique.}
 \item{\code{\link{posterior}}}{Calculation of posterior probabilities of trading days' conditions.}
 \item{\code{\link{pin_calc}}}{Computing the probability of informed trading (PIN).}
 \item{\code{\link{pin_confint}}}{Calculation of confidence intervals for the probability of informed trading.}
 \item{\code{\link{pin_est_core}}}{Core function of maximization routines for PIN likelihood function. It grants the most control over optimization procedure.
                                 However, the settings chosen in \code{\link{pin_est}} will be sufficient in most applications.}
 \item{\code{\link{pin_est}}}{User-friendly wrapper around \code{\link{pin_est_core}}. Default method for creating initial values is set to
                              hierarchical agglomerative clustering, the likelihood formulation defaults to the one proposed by
                              Lin and Ke (2011).}
 \item{\code{\link{pin_ll}}}{Evaluating likelihood function values either utilizing the factorization by Easley et. al (2010) or
                             Lin and Ke (2011).}
 \item{\code{\link{qpin}}}{Returns quarterly estimates, function is a wrapper around \code{\link{pin_est}} and
                           inherits its optimization settings.}
 \item{\code{\link{simulateBS}}}{Simulate datasets of aggregated daily buys and sells.}
}
}

\section{Datasets}{

\describe{
 \item{\code{\link{BSinfrequent}}}{A matrix containing three months of synthetic daily buys and sells data representing an infrequently traded stock.}
 \item{\code{\link{BSfrequent}}}{A matrix containing three months of synthetic daily buys and sells data representing a frequently traded stock.}
 \item{\code{\link{BSheavy}}}{A matrix containing three months of synthetic daily buys and sells data representing a heavily traded stock.}
 \item{\code{\link{BSfrequent2015}}}{A matrix containing one year of synthetic daily buys and sells data representing a frequently traded stock.
                                     Rownames equal the business days in 2015.}
}
Source of all included datasets: own simulation
}

\section{Author}{

Andreas Recktenwald (Saarland University, Statistics & Econometrics) \cr
Email: \email{a.recktenwald@mx.uni-saarland.de}
}

\section{Github}{

\url{https://github.com/anre005/pinbasic}
}

\section{References}{

Easley, David et al. (2002) \cr
Is Information Risk a Determinant of Asset Returns? \cr
\emph{The Journal of Finance}, Volume 57, Number 5, pp. 2185 - 2221 \cr
\doi{10.1111/1540-6261.00493}

Easley, David et al. (1996) \cr
Liquidity, Information, and Infrequently Traded Stocks\cr
\emph{The Journal of Finance}, Volume 51, Number 4, pp. 1405 - 1436 \cr
\doi{10.1111/j.1540-6261.1996.tb04074.x}

Easley, David et al. (2010) \cr
Factoring Information into Returns \cr
\emph{Journal of Financial and Quantitative Analysis}, Volume 45, Issue 2, pp. 293 - 309 \cr
\doi{10.1017/S0022109010000074}

Ersan, Oguz and Alici, Asli (2016) \cr
An unbiased computation methodology for estimating the probability of informed trading (PIN) \cr
\emph{Journal of International Financial Markets, Institutions and Money}, Volume 43, pp. 74 - 94 \cr
\doi{10.1016/j.intfin.2016.04.001}

Gan, Quan et al. (2015) \cr
A faster estimation method for the probability of informed trading
using hierarchical agglomerative clustering \cr
\emph{Quantitative Finance}, Volume 15, Issue 11, pp. 1805 - 1821 \cr
\doi{10.1080/14697688.2015.1023336}

Grolemund, Garett and Wickham, Hadley (2011) \cr
Dates and Times Made Easy with lubridate \cr
\emph{Journal of Statistical Software}, Volume 40, Issue 3, pp. 1 - 25 \cr
\doi{10.18637/jss.v040.i03}

Lin, Hsiou-Wei William and Ke, Wen-Chyan (2011) \cr
A computing bias in estimating the probability of informed trading \cr
\emph{Journal of Financial Markets}, Volume 14, Issue 4, pp. 625 - 640 \cr
\doi{10.1016/j.finmar.2011.03.001}

Wickham, Hadley (2009) \cr
ggplot2: Elegant Graphics for Data Analysis \cr
\emph{Springer-Verlag New York} \cr
\doi{10.1007/978-0-387-98141-3}

Wickham, Hadley (2007) \cr
Reshaping Data with the reshape Package \cr
\emph{Journal of Statistical Software}, Volume 21, Issue 12, pp. 1 - 20 \cr
\doi{10.18637/jss.v021.i12}

Wickham, Hadley (2016) \cr
scales: Scale Functions for Visualization \cr
\emph{R package version 0.4.0}

Yan, Yuxing and Zhang, Shaojun (2012) \cr
An improved estimation method and empirical properties of the probability of informed trading \cr
\emph{Journal of Banking & Finance}, Volume 36, Issue 2, pp. 454 - 467 \cr
\doi{10.1016/j.jbankfin.2011.08.003}
}

