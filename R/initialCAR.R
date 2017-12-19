#' Initial values for PIN optimization
#'
#' Generates set(s) of initial values which can be used in PIN optimization routines.
#'
#' @inheritParams pin_ll
#' @param method \emph{character} Switch between algorithms for generating initial values,
#'               valid choices are: 'Grid', 'HAC' and 'HAC_Ref'
#' @param length \emph{numeric} length of equidistant sequence from 0.1 to 0.9 for parameters of grid search algorithm,
#'               defaults to 5, irrelevant for HAC and refined HAC method
#' @param num_clust \emph{numeric} only relevant for refined HAC method, total number of clusters trading data is grouped into
#'                   equals \code{num_clust} + 1
#' @param details \emph{logical} only relevant for grid search,
#'                if \code{TRUE} and \code{method = 'Grid'} the number of infeasible sets of initial values are returned,
#'
#' @return
#'  Matrix with set(s) of initial values for PIN model optimization.
#'  If \code{method = 'Grid'} and \code{details = TRUE} a list with four elements is returned:
#'  \describe{
#'  \item{inits}{Matrix of sets of initial values}
#'  \item{neg_eps}{Number of infeasible sets due to negative values for intensity of uninformed sells}
#'  \item{irr_mu}{Number of infeasible sets due to intensity of informed trading larger than any daily buys and sells data}
#'  \item{rem}{Total number of removed sets of initial values}
#'  }
#'
#' @importFrom stats dist cutree
#' @importFrom fastcluster hclust
#'
#' @examples
#' # Loading simulated datasets
#'
#' data("BSinfrequent")
#' data("BSfrequent")
#' data("BSheavy")
#'
#' # Grid Search
#'
#' grid <- initial_vals(numbuys = BSinfrequent[,"Buys"],
#'                      numsells = BSinfrequent[,"Sells"],
#'                      method = "Grid")
#'
#' # Grid Search: Detailed Output
#'
#' grid_detailed <- initial_vals(numbuys = BSinfrequent[,"Buys"],
#'                               numsells = BSinfrequent[,"Sells"],
#'                               method = "Grid", details = TRUE)
#'
#' # HAC
#'
#' hac <- initial_vals(numbuys = BSfrequent[,"Buys"],
#'                     numsells = BSfrequent[,"Sells"],
#'                     method = "HAC")
#'
#' # Refined HAC
#'
#' hac_ref <- initial_vals(numbuys = BSheavy[,"Buys"],
#'                         numsells = BSheavy[,"Sells"],
#'                         method = "HAC_Ref")
#'
#' @export
#'
#' @references
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
#' Yan, Yuxing and Zhang, Shaojun (2012) \cr
#' An improved estimation method and empirical properties of the probability of informed trading \cr
#' \emph{Journal of Banking & Finance}, Volume 36, Issue 2, pp. 454 - 467 \cr
#' \doi{10.1016/j.jbankfin.2011.08.003}



initial_vals <- function(numbuys = NULL, numsells = NULL,
                         method = "HAC",
                         length = 5, num_clust = 5, details = FALSE) {
  if(is.null(numbuys)) stop("Missing data for 'numbuys'")
  if(is.null(numsells)) stop("Missing data for 'numsells'")
  if(length(numbuys) != length(numsells)) stop("Unequal lengths for 'numbuys' and 'numsells'")

  meth <- match.arg(method, choices = c("HAC", "HAC_Ref", "Grid"))

  res <- switch(meth,
                Grid = {
                  init_grid_search(numbuys = numbuys, numsells = numsells,
                                   length = length, details = details)
                },
                HAC = {
                  init_hac(numbuys = numbuys, numsells = numsells)
                },
                HAC_Ref = {
                  init_hac_ref(numbuys = numbuys, numsells = numsells,
                               j = num_clust)
                }
  )
  res
}

init_grid_search <- function(numbuys = NULL, numsells = NULL,
                             length = 5, details = FALSE) {
  max_obs <- max(c(numbuys, numsells))
  avg_buys <- mean(numbuys)
  avg_sells <- mean(numsells)

  alpha <- seq(from = 0.1, to = 0.9, length.out = length)
  delta <- alpha
  gamma <- alpha

  epsilon_b <- gamma * avg_buys
  mat <- expand.grid(alpha, delta, epsilon_b)

  colnames(mat) <- c("alpha", "delta", "epsilon_b")

  mu <- (avg_buys - mat[,"epsilon_b"])/(mat[,"alpha"] * (1.0 - mat[,"delta"]))
  epsilon_s <- avg_sells - mat[,"alpha"] * mat[,"delta"] * mu

  mat <- cbind(mat, epsilon_s, mu)

  neg_eps <- which(mat[,"epsilon_s"] < 0)
  irrelevant_mu <- which(mat[,"mu"] > max_obs)
  rem_lines <- unique(c(neg_eps, irrelevant_mu))

  if(length(rem_lines) > 0) {
    mat <- mat[-rem_lines,]
  }

  res <- data.matrix(mat)
  if(details) {
    res_list <- vector("list", 4)
    names(res_list) <- c("inits", "neg_eps", "irr_mu", "rem")
    res_list[["inits"]] <- res
    res_list[["neg_eps"]] <- length(neg_eps)
    res_list[["irr_mu"]] <- length(irrelevant_mu)
    res_list[["rem"]] <- length(rem_lines)
    return(res_list)
  } else return(res)
}

init_hac <- function(numbuys = NULL, numsells = NULL) {
  N <- length(numbuys)
  order_imb <- numbuys - numsells
  clust <- hclust(dist(order_imb), method = "complete")
  clust3 <- cutree(clust, k = 3)

  cluster_means <- numeric(3)

  for(k in 1:3){
    cluster_means[k] <- mean(order_imb[clust3 == k])
  }

  max_ind  <- which.max(cluster_means)
  min_ind <- which.min(cluster_means)

  no_ind <- (1:3)[(!((1:3) %in% c(max_ind, min_ind)))]

  cluster_bad_ind <- which(clust3 == min_ind)
  cluster_good_ind <- which(clust3 == max_ind)
  cluster_no_ind <- which(clust3 == no_ind)

  cluster_bad <- cbind(numbuys[cluster_bad_ind], numsells[cluster_bad_ind])
  cluster_good <- cbind(numbuys[cluster_good_ind], numsells[cluster_good_ind])
  cluster_no <- cbind(numbuys[cluster_no_ind], numsells[cluster_no_ind])


  weights <- c(nrow(cluster_bad)/N,
               nrow(cluster_good)/N,
               nrow(cluster_no)/N)

  clusters <- vector("list",3)

  clusters[[1]] <- cluster_bad
  clusters[[2]] <- cluster_good
  clusters[[3]] <- cluster_no

  mean_daily_buys <- sapply(clusters, function(x) mean(x[,1]))
  mean_daily_sells <- sapply(clusters, function(x) mean(x[,2]))

  mat <- cbind(mean_daily_buys, mean_daily_sells, weights)
  colnames(mat) <- c("mean_daily_buys", "mean_daily_sells", "weight")

  rownames(mat) <- c("BadNews","GoodNews","NoNews")
  mat[which(is.nan(mat))] <- 0
  if(mat["BadNews", "weight"] == 0) mu_s <- 0
  if(mat["GoodNews", "weight"] == 0) mu_b <- 0

  alpha <- mat["GoodNews","weight"] + mat["BadNews","weight"]
  delta <- mat["BadNews","weight"]/alpha

  eps_b_helper <- mat["BadNews","weight"] + mat["NoNews","weight"]
  eps_s_helper <- mat["GoodNews","weight"] + mat["NoNews","weight"]

  eps_b <- (mat["BadNews","weight"]/eps_b_helper) * mat["BadNews","mean_daily_buys"] +
           (mat["NoNews","weight"]/eps_b_helper) * mat["NoNews","mean_daily_buys"]

  eps_s <- (mat["GoodNews","weight"]/eps_s_helper) * mat["GoodNews","mean_daily_sells"] +
           (mat["NoNews","weight"]/eps_s_helper) * mat["NoNews","mean_daily_sells"]

  if(mat["GoodNews", "weight"] > 0) mu_b <- mat["GoodNews","mean_daily_buys"] - eps_b
  if(mat["BadNews", "weight"] > 0) mu_s <- mat["BadNews","mean_daily_sells"] - eps_s

  if(mu_b < 0) mu_b <- 0
  if(mu_s < 0) mu_s <- 0

  mu <- (mat["GoodNews","weight"]/alpha) * mu_b +
        (mat["BadNews","weight"]/alpha) * mu_s

  res <- matrix(data = c(alpha, delta, eps_b, eps_s, mu), nrow = 1, ncol = 5)
  colnames(res) <- c("alpha", "delta", "epsilon_b", "epsilon_s", "mu")
  res
}

init_hac_ref <- function(numbuys = NULL, numsells = NULL, j = 5) {
  N <- length(numbuys)
  ordered_clusters <- vector("list", j + 1)
  order_imb <- numbuys - numsells
  abs_order_imb <- abs(order_imb)

  res <- matrix(data = NA, nrow = j, ncol = 5)
  colnames(res) <- c("alpha", "delta", "epsilon_b", "epsilon_s", "mu")#, "ll")

  clust <- hclust(dist(abs_order_imb), method = "complete")
  clustj <- cutree(clust, k = j + 1)

  cluster_means <- numeric(j + 1)
  for(k in 1:(j + 1)){
    cluster_means[k] <- mean(abs_order_imb[clustj == k])
  }

  cluster_means_ord_ind <- order(cluster_means, decreasing = FALSE)

  for(k in 1:j) {
    no_event_ind <- clustj %in% cluster_means_ord_ind[1:k]
    event_ind <- clustj %in% cluster_means_ord_ind[(k + 1):(j + 1)]
    alpha <- sum(event_ind)/N
    # if(!mu_old)
    mu <- mean(abs_order_imb[event_ind]) - mean(abs_order_imb[no_event_ind])

    good_ind <- bad_ind <- numeric()

    for(l in cluster_means_ord_ind[(k + 1):(j + 1)]) {
      mean_event_cluster <- mean(order_imb[clustj == l])
      if(mean_event_cluster > 0) good_ind <- c(good_ind, l)
      else bad_ind <- c(bad_ind, l)
    }

    good_news_ind <- clustj %in% good_ind
    bad_news_ind <- clustj %in% bad_ind

    cluster_no <- cbind(numbuys[no_event_ind], numsells[no_event_ind])
    cluster_good <- cbind(numbuys[good_news_ind], numsells[good_news_ind])
    cluster_bad <- cbind(numbuys[bad_news_ind], numsells[bad_news_ind])

    weights <- c(nrow(cluster_bad)/N,
                 nrow(cluster_good)/N,
                 nrow(cluster_no)/N)

    clusters <- vector("list",3)

    clusters[[1]] <- cluster_bad
    clusters[[2]] <- cluster_good
    clusters[[3]] <- cluster_no

    mean_daily_buys <- sapply(clusters, function(x) mean(x[,1]))
    mean_daily_sells <- sapply(clusters, function(x) mean(x[,2]))

    mat <- cbind(mean_daily_buys, mean_daily_sells, weights)
    colnames(mat) <- c("mean_daily_buys", "mean_daily_sells", "weight")
    rownames(mat) <- c("BadNews","GoodNews","NoNews")

    mat[which(is.nan(mat))] <- 0

    if(mat["BadNews", "weight"] == 0) {
      mu_s <- 0
    }
    if(mat["GoodNews", "weight"] == 0) {
      mu_b <- 0
    }

    delta <- mat["BadNews","weight"]/alpha

    eps_b_helper <- mat["BadNews","weight"] + mat["NoNews","weight"]
    eps_s_helper <- mat["GoodNews","weight"] + mat["NoNews","weight"]

    eps_b <- (mat["BadNews","weight"]/eps_b_helper) * mat["BadNews","mean_daily_buys"] +
      (mat["NoNews","weight"]/eps_b_helper) * mat["NoNews","mean_daily_buys"]

    eps_s <- (mat["GoodNews","weight"]/eps_s_helper) * mat["GoodNews","mean_daily_sells"] +
      (mat["NoNews","weight"]/eps_s_helper) * mat["NoNews","mean_daily_sells"]

    if(mat["GoodNews", "weight"] > 0) mu_b <- mat["GoodNews","mean_daily_buys"] - eps_b
    if(mat["BadNews", "weight"] > 0) mu_s <- mat["BadNews","mean_daily_sells"] - eps_s

    # if(mu_old) {
    #   mu <- (mat["GoodNews","weight"]/alpha) * mu_b + (mat["BadNews","weight"]/alpha) * mu_s
    # }

    res[k,1:5] <- c(alpha, delta, eps_b, eps_s, mu)

    # ll_k <- pin_ll(param = res[k, 1:5], numbuys = numbuys, numsells = numsells,
    #                factorization = "Lin_Ke")
    #
    # res[k, 6] <- ll_k
  }

  return(res)
}
