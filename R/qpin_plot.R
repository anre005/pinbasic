#' PIN Visualization
#'
#' Visualization of quarterly estimates and probability of informed trading with ggplot2.
#'
#' @param qpin_obj List returned by \code{\link{qpin}}
#'
#' @seealso \code{\link{qpin}}
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
#'                    
#' qpin_plot(qpin_list)
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
#' @export qpin_plot
#'
qpin_plot <- function(qpin_obj) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 needed for this function to work. Please install it.",
         call. = FALSE)
  }
  if (!requireNamespace("reshape2", quietly = TRUE)) {
    stop("reshape2 needed for this function to work. Please install it.",
         call. = FALSE)
  }
  alpha <- delta <- epsilon_b <-
    epsilon_s <- mu <- pin <- numeric()

  quat <- names(qpin_obj)

  alpha <- sapply(qpin_obj, function(x) x$Results["alpha", "Estimate"])
  delta <- sapply(qpin_obj, function(x) x$Results["delta", "Estimate"])
  epsilon_b <- sapply(qpin_obj, function(x) x$Results["epsilon_b", "Estimate"])
  epsilon_s <- sapply(qpin_obj, function(x) x$Results["epsilon_s", "Estimate"])
  mu <- sapply(qpin_obj, function(x) x$Results["mu", "Estimate"])
  pin <- sapply(qpin_obj, function(x) x$pin)

  qpin_df <- data.frame(Quarter = quat, alpha = alpha, delta = delta, epsilon_b = epsilon_b,
                        epsilon_s = epsilon_s, mu = mu, pin = pin)
  qpin_df <- reshape2::melt(qpin_df, id.vars = "Quarter")

  p <- ggplot2::ggplot(data = qpin_df, ggplot2::aes_string(x = "Quarter", y = "value", group = 1)) +
    ggplot2::geom_point(shape = 19, size = 1.25) +
    ggplot2::geom_line() + ggplot2::xlab("\n Quarter") +
    ggplot2::facet_grid(variable ~ ., scales = "free") +
    ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(n = 3)) +
    ggplot2::theme(legend.position="none",
                   axis.title.y = ggplot2::element_blank())
  p
}