% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/initialCAR.R
\name{initial_vals}
\alias{initial_vals}
\title{Initial values for PIN optimization}
\usage{
initial_vals(
  numbuys = NULL,
  numsells = NULL,
  method = "HAC",
  length = 5,
  num_clust = 5,
  details = FALSE
)
}
\arguments{
\item{numbuys}{\emph{numeric}: vector of daily buys}

\item{numsells}{\emph{numeric}: vector of daily sells}

\item{method}{\emph{character} Switch between algorithms for generating initial values,
valid choices are: 'Grid', 'HAC' and 'HAC_Ref'}

\item{length}{\emph{numeric} length of equidistant sequence from 0.1 to 0.9 for parameters of grid search algorithm,
defaults to 5, irrelevant for HAC and refined HAC method}

\item{num_clust}{\emph{numeric} only relevant for refined HAC method, total number of clusters trading data is grouped into
equals \code{num_clust} + 1}

\item{details}{\emph{logical} only relevant for grid search,
if \code{TRUE} and \code{method = 'Grid'} the number of infeasible sets of initial values are returned,}
}
\value{
Matrix with set(s) of initial values for PIN model optimization.
 If \code{method = 'Grid'} and \code{details = TRUE} a list with four elements is returned:
 \describe{
 \item{inits}{Matrix of sets of initial values}
 \item{neg_eps}{Number of infeasible sets due to negative values for intensity of uninformed sells}
 \item{irr_mu}{Number of infeasible sets due to intensity of informed trading larger than any daily buys and sells data}
 \item{rem}{Total number of removed sets of initial values}
 }
}
\description{
Generates set(s) of initial values which can be used in PIN optimization routines.
}
\examples{
# Loading simulated datasets

data("BSinfrequent")
data("BSfrequent")
data("BSheavy")

# Grid Search

grid <- initial_vals(numbuys = BSinfrequent[,"Buys"],
                     numsells = BSinfrequent[,"Sells"],
                     method = "Grid")

# Grid Search: Detailed Output

grid_detailed <- initial_vals(numbuys = BSinfrequent[,"Buys"],
                              numsells = BSinfrequent[,"Sells"],
                              method = "Grid", details = TRUE)

# HAC

hac <- initial_vals(numbuys = BSfrequent[,"Buys"],
                    numsells = BSfrequent[,"Sells"],
                    method = "HAC")

# Refined HAC

hac_ref <- initial_vals(numbuys = BSheavy[,"Buys"],
                        numsells = BSheavy[,"Sells"],
                        method = "HAC_Ref")

}
\references{
Ersan, Oguz and Alici, Asli (2016) \cr
An unbiased computation methodology for estimating the probability of informed trading (PIN) \cr
\emph{Journal of International Financial Markets, Institutions and Money}, Volume 43, pp. 74 - 94 \cr
\doi{10.1016/j.intfin.2016.04.001}

Gan, Quan et al. (2015) \cr
A faster estimation method for the probability of informed trading
using hierarchical agglomerative clustering \cr
\emph{Quantitative Finance}, Volume 15, Issue 11, pp. 1805 - 1821 \cr
\doi{10.1080/14697688.2015.1023336}

Yan, Yuxing and Zhang, Shaojun (2012) \cr
An improved estimation method and empirical properties of the probability of informed trading \cr
\emph{Journal of Banking & Finance}, Volume 36, Issue 2, pp. 454 - 467 \cr
\doi{10.1016/j.jbankfin.2011.08.003}
}
