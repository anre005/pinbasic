#' PIN Visualization
#'
#' Visualization of quarterly estimates and probability of informed trading with ggplot2.
#'
#' Facets are grouped by probability parameters, intensity parameters and the probability of informed trading.
#'
#' @param x List returned by \code{\link{qpin}}
#'
#' @seealso \code{\link{qpin}}
#'
#' @importFrom ggplot2 ggplot aes_string facet_grid
#' @importFrom ggplot2 geom_line geom_point scale_y_continuous
#' @importFrom ggplot2 theme element_blank
#' @importFrom scales pretty_breaks
#' @importFrom reshape2 melt
#'
#' @examples
#' # Loading one year of simulated daily buys and sells
#'
#' data('BSfrequent2015')
#'
#' # Quarterly estimates for model parameters and the probability of informed trading
#' # Rownames of 'BSfrequent2015' equal the business days in 2015.
#'
#' qpin_list <- qpin(numbuys = BSfrequent2015[,"Buys"], numsells = BSfrequent2015[,"Sells"],
#'                   dates = as.Date(rownames(BSfrequent2015), format = "%Y-%m-%d"))
#'
#' # Visualization of quarterly estimates
#' library(ggplot2)
#' ggplot(qpin_list[["res"]])
#'
#' @references
#' Wickham, Hadley (2009) \cr
#' ggplot2: Elegant Graphics for Data Analysis \cr
#' \emph{Springer-Verlag New York} \cr
#' \doi{10.1007/978-0-387-98141-3}
#'
#' Wickham, Hadley (2007) \cr
#' Reshaping Data with the reshape Package \cr
#' \emph{Journal of Statistical Software}, Volume 21, Issue 12, pp. 1 - 20 \cr
#' \doi{10.18637/jss.v021.i12}
#'
#' Wickham, Hadley (2016) \cr
#' scales: Scale Functions for Visualization \cr
#' \emph{R package version 0.4.0}
#'
#' @method ggplot qpin
#' @return An object of class \code{\link[ggplot2]{ggplot}}.
#' @export
#'
ggplot.qpin <- function(x) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 is required for this function to work. Please install it.",
         call. = FALSE)
  }
  if (!requireNamespace("reshape2", quietly = TRUE)) {
    stop("reshape2 is required for this function to work. Please install it.",
         call. = FALSE)
  }
  alpha <- delta <- epsilon_b <-
    epsilon_s <- mu <- pin <- numeric()

  quat <- names(x)

  alpha <- sapply(x, function(x) x$Results["alpha", "Estimate"])
  delta <- sapply(x, function(x) x$Results["delta", "Estimate"])
  epsilon_b <- sapply(x, function(x) x$Results["epsilon_b", "Estimate"])
  epsilon_s <- sapply(x, function(x) x$Results["epsilon_s", "Estimate"])
  mu <- sapply(x, function(x) x$Results["mu", "Estimate"])
  pin <- sapply(x, function(x) x$pin)

  qpin_df <- data.frame(Quarter = quat, alpha = alpha, delta = delta, epsilon_b = epsilon_b,
                        epsilon_s = epsilon_s, mu = mu, PIN = pin)
  qpin_df <- melt(qpin_df, id.vars = "Quarter")

  ID <- character(nrow(qpin_df))
  ID[qpin_df[,"variable"] %in% c("alpha", "delta")] <- "Probability Parameters"
  ID[qpin_df[,"variable"] %in% c("epsilon_b", "epsilon_s", "mu")] <- "Intensity Parameters"
  ID[qpin_df[,"variable"] %in% c("PIN")] <- "Prob. of Informed Trading"

  qpin_df <- transform(qpin_df, facet = ID)
  qpin_df$facet_ordered <- factor(qpin_df$facet,
                                  levels = c("Probability Parameters",
                                             "Intensity Parameters",
                                             "Prob. of Informed Trading"))

  p <- ggplot(data = qpin_df, aes_string(x = "Quarter", y = "value", group = "variable")) +
    facet_grid(facet_ordered~., scales = "free_y") +
    geom_line(aes_string(colour = "variable", x = "Quarter", y = "value")) +
    geom_point(shape = 19, size = 1.25, aes_string(colour = "variable")) +
    # xlab("\n Quarter") +
    scale_y_continuous(breaks = pretty_breaks(n = 3)) +
    theme(legend.position="right",
                   axis.title.y = element_blank(),
                   axis.title.x = element_blank(),
                   legend.title = element_blank())
  p
}