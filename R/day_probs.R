#' Posterior Probabilities
#'
#' Calculates posterior probabilities of conditions of trading days
#'
#' For more details see corresponding section in vignette
#'
#' @inheritParams pin_ll
#'
#' @return \emph{numeric} matrix with columns 'no', 'good' and 'bad' representing posterior probabilities for the corresponding trading day conditions
#'
#' @examples \dontrun{See Vignette \code{browseVignette(package = 'pinbasic')}}
#'
#' @seealso \code{\link{ggplot.posterior}}
#' @export

posterior <- function(param = NULL, numbuys = NULL, numsells = NULL) {
  param <- param_check(param)
  if(is.null(numbuys)) stop("Missing data for 'numbuys'")
  if(is.null(numsells)) stop("Missing data for 'numsells'")
  if(length(numbuys) != length(numsells)) stop("Unequal lengths for 'numbuys' and 'numsells'")

  # nb_dates <- check_dates(numbuys)
  # ns_dates <- check_dates(numsells)
  #
  # if(is.na(nb_dates) || is.na(ns_dates)) {
  #   warning("Names of 'numbuys' and/or 'numsells' can't be converted to dates. Setting first date to '01-01-2016'.")
  # }
  #
  # if(nb_dates != ns_dates) {
  #   warning("Names of 'numbuys' and 'numsells' differ. Setting first date to '01-01-2016'.")
  # }

  # if((!is.null(start) || !is.null(end)) &&
  #    (!is.na(nb_dates) && !is.na(ns_dates)) &&
  #    nb_dates == ns_dates) {
  #
  #   if(!is.null(start) && !is.null(end)) {
  #     numbuys <- subset(numbuys,
  #                       subset = (nb_dates >= as.Date(start)) & (nb_dates > as.Date(end)))
  #     numbuys <- subset(numsells,
  #                       subset = (ns_dates >= as.Date(start)) & (ns_dates > as.Date(end)))
  #   }
  #   if(!is.null(start) && is.null(end)) {
  #     numbuys <- subset(numbuys,
  #                       subset = (nb_dates >= as.Date(start)))
  #     numbuys <- subset(numsells,
  #                       subset = (ns_dates >= as.Date(start)))
  #   }
  #   if(is.null(start) && is.null(end)) {
  #     numbuys <- subset(numbuys,
  #                       subset = (nb_dates <= as.Date(end)))
  #     numbuys <- subset(numsells,
  #                       subset = (ns_dates <= as.Date(end)))
  #   }
  # }

  rat1 <- param["mu"]/param["epsilon_s"]
  rat2 <- param["mu"]/param["epsilon_b"]

  rat1log1p <- log1p(rat1)
  rat2log1p <- log1p(rat2)

  prob_no <- 1.0 - param["alpha"]
  prob_good <- param["alpha"] * (1.0 - param["delta"])
  prob_bad <- param["alpha"] * param["delta"]

  e1 <- -param["mu"] + numsells * rat1log1p
  e2 <- -param["mu"] + numbuys * rat2log1p
  e_max <- pmax.int(e1, e2, 0)

  denom_helper <- e_max + log(prob_no * exp(-e_max) + prob_good * exp(e2-e_max) + prob_bad * exp(e1-e_max))

  no_prob <- log(prob_no) - denom_helper
  good_prob <- log(prob_good) + e2 - denom_helper
  bad_prob <- log(prob_bad) + e1 - denom_helper

  res <- cbind(exp(no_prob), exp(good_prob), exp(bad_prob))
  colnames(res) <- c("no", "good", "bad")
  class(res) <- c("matrix", "posterior")
  res
}

# check_dates <- function (date_in) {
#   return(tryCatch(as.Date(date_in), error=function(e) NA))
# }
