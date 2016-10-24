param_check <- function(param = NULL) {
  if(is.null(param)) stop("No parameter vector provided!")
  if(!(length(param) == 5)) stop("Parameter vector need to have 5 elements)!")
  
  if(is.null(names(param))) {
    names(param) <- c("alpha", "delta", "epsilon_b", "epsilon_s", "mu")
  }
  
  if(!is.null(names(param)) && 
     !all(names(param) %in% c("alpha", "delta", "epsilon_b", "epsilon_s", "mu"))) {
    names(param) <- c("alpha", "delta", "epsilon_b", "epsilon_s", "mu")
  }
  
  param
}