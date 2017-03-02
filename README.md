
<!-- README.md is generated from README.Rmd. Please edit that file -->
[![Travis-CI Build Status](https://travis-ci.org/anre005/pinbasic.svg?branch=master)](https://travis-ci.org/anre005/pinbasic) [![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/pinbasic)](https://cran.r-project.org/package=pinbasic) [![Research software impact](http://depsy.org/api/package/cran/pinbasic/badge.svg)](http://depsy.org/package/r/pinbasic)

pinbasic: Fast and Stable Estimation of the Probability of Informed Trading (PIN)
=================================================================================

The `pinbasic` package ships utilities for fast and stable estimation of the probability of informed trading in the static $\\pintext$ framework. The function design is chosen to fit the extended EHO model setup but can also be applied to the simpler EKOP model by equating the intensities of uninformed buys and sells. State-of-the-art factorization of the model likelihood function as well as most recent algorithms for generating initial values for optimization routines are implemented. In total, two likelihood factorizations and three methodologies for starting values are included. Likelihood functions are evaluated with `pin_ll` and sets of starting values are returned by `initial_vals`. The probability of informed trading can be estimated for arbitrary length of daily buys and sells data with `pin_est` which is a wrapper around the workhorse function `pin_est_core`. No information about the time span of the underlying data is required to perform optimizations with `pin_est`. However, the recommendation given in the literature is using at least data for 60 trading days to ensure convergence of the likelihood maximization. Quarterly estimates are returned by `qpin` which can be visualized with `ggplot`. Datasets of daily aggregated numbers of buys and sells can be simulated with `simulateBS`. Calculation of confidence intervals for the probability of informed trading can be enabled by `confint` argument in optimization routines (`pin_est_core`, `pin_est` and `qpin`) or by calling `pin_confint` directly. Additionally, posterior probabilities for conditions of trading days can be computed with `posterior` and plotted with `ggplot`.

Examples
--------

The dataset `BSfrequent` cover 60 trading days and represent a frequently traded equity. Model parameters and the probability of informed trading can be estimated with `pin_est`.

``` r
library(pinbasic)

# Loading data
data("BSfrequent")

# Estimation
pin_freq <- pin_est(numbuys = BSfrequent[,"Buys"], numsells = BSfrequent[,"Sells"])
pin_freq
#> $Results
#>            Estimate  Std. error    t value      Pr(> t)
#> alpha        0.2000  0.05163873   3.873062 0.0001074766
#> delta        0.5000  0.14433702   3.464115 0.0005319794
#> epsilon_b 1805.4354  5.67230494 318.289553 0.0000000000
#> epsilon_s 1700.6753  5.50984189 308.661358 0.0000000000
#> mu         597.6107 14.60600145  40.915421 0.0000000000
#> 
#> $ll
#> loglike 
#> 1415607 
#> 
#> $pin
#>        PIN 
#> 0.03296587 
#> 
#> $conv
#> Convergence 
#>           0 
#> 
#> $message
#> [1] "relative convergence (4)"
#> 
#> $iterations
#> Iterations 
#>          1 
#> 
#> $init_vals
#>     alpha     delta epsilon_b epsilon_s        mu 
#>    0.2000    0.5000 1805.4259 1700.6852  597.6111
```

`BSfrequent2015` contains simulated daily buys and sells for a frequently traded equity for business days in 2015. `qpin` returns quarterly estimates which can be visualized with `ggplot`.

``` r
# Quarterly PIN estimates
# Confidence interval computation enabled:
#   * using only 1000 simulated datasets
#   * confidence level set to 0.95
#   * seed set to 123

data('BSfrequent2015')
qpin2015 <- qpin(numbuys = BSfrequent2015[,"Buys"], numsells = BSfrequent2015[,"Sells"],
                 dates = as.Date(rownames(BSfrequent2015), format = "%Y-%m-%d"),
                 confint = TRUE, ci_control = list(n = 1000, seed = 123))

# Print confidence intervals for all four quarters
ci_quarters <- lapply(qpin2015, function(x) x$confint)
ci_quarters
#> $`2015.1`
#>       2.5%      97.5% 
#> 0.02752256 0.05943952 
#> 
#> $`2015.2`
#>        2.5%       97.5% 
#> 0.006405346 0.033145972 
#> 
#> $`2015.3`
#>       2.5%      97.5% 
#> 0.03658681 0.07322781 
#> 
#> $`2015.4`
#>       2.5%      97.5% 
#> 0.01014942 0.03794254

# Visualization of estimated parameters
library(ggplot2)
ggplot(qpin2015)
```

![](README-unnamed-chunk-3-1.png)

Posterior probabilities of trading days' condition are returned by `posterior` and can be displayed with `ggplot`. The following code chunk shows how posterior probabilities for `BSfrequent2015` in the third quarter can be calculated and visualized.

``` r
# Corresponding parameter estimates
freq_2015.3 <- qpin2015$'2015.3'$Results[,"Estimate"]

# Subsetting data
third_quarter <- subset(BSfrequent2015, subset = lubridate::quarter(rownames(BSfrequent2015)) == 3)

# Calculating posterior probabilities
post_third <- posterior(param = freq_2015.3, 
                        numbuys = third_quarter[,"Buys"], numsells = third_quarter[,"Sells"])

# Plotting
ggplot(post_third)
```

![](README-postdates-1.png)

Installation
------------

You may install the stable version from **CRAN**, or the development version from GitHub using **devtools**:

``` r
# install from CRAN
install.packages("pinbasic")

# install from github using devtools
devtools::install_github("anre005/pinbasic")
```
