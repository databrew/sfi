#' SFI plot unlist
#' 
#' Unlist and print the plots in an object generated by \code{sfi_plot}
#' @export
#' @return plots are printed
#' @param x A list of plots
#' @import ggplot2
#' @import dplyr

sfi_plot_unlist <- function(x){
  if(!is.list(x)){
    stop('x must be a list')
  }
  ll <- length(x)
  for(i in 1:ll){
    print(x[[i]])
  }
}