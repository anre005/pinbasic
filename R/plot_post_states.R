#' Visualization of Posterior Probabilities
#'
#' Visualization of posterior probabilities returned by \code{post_states} with ggplot2
#'
#' @param x \emph{numeric} matrix returned by \code{\link{posterior}}
#'
#' @seealso \code{\link{posterior}}
#'
#' @importFrom ggplot2 ggplot aes geom_bar
#' @importFrom ggplot2 ylab scale_fill_discrete guide_legend
#' @importFrom ggplot2 theme element_blank
#' @importFrom reshape2 melt
#'
#' @return An object of class \code{\link[ggplot2]{ggplot}}.
#'
#' @examples \dontrun{See Vignette \code{browseVignette(package = 'pinbasic')}}
#'
#' @method ggplot posterior
#' @export

ggplot.posterior <- function(x) {
  if(is.null(rownames(x))) {
    df <- data.frame(bs_date = 1:nrow(x),
                     no = x[,"no"], good = x[,"good"], bad = x[,"bad"])
  } else {
    check_rows <- check_bs_dates(rownames(x))
    if(!all(is.na(check_rows))) {
      df <- data.frame(bs_date = as.Date(rownames(x)),
                       no = x[,"no"], good = x[,"good"], bad = x[,"bad"])
    }
    if(!is.null(rownames(x)) && is.na(check_rows)) {
      df <- data.frame(bs_date = rownames(x),
                       no = x[,"no"], good = x[,"good"], bad = x[,"bad"])
    }
  }

  df_melt <- melt(df, id = "bs_date")

  dp_plot <- ggplot(df_melt, aes(x = bs_date, y = value, fill = variable)) +
    geom_bar(stat = "identity", position = "fill", width = 0.5) +
    ylab("Posterior Probabilities \n of Trading Days' Conditions") +
    scale_fill_discrete(breaks=c("no", "good", "bad"),
                        labels=c("no-news",
                                 "good-news",
                                 "bad-news"),
                        guide = guide_legend(nrow = 1, keywidth = 0.5, keyheight = 0.5, title = NULL,
                                             label.position = "right")) +
    theme(axis.title.x = element_blank(), legend.position = "bottom")

  dp_plot
}

check_bs_dates <- function (bs_date_in) {
  return(tryCatch(as.Date(bs_date_in), error=function(e) NA))
}

if(getRversion() >= "2.15.1")  utils::globalVariables(c("bs_date","value", "variable"))