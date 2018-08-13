#' Dumas 1
#'
#' Generate a plot for Dumas Figure 1
#' @export
#' @return a list of plots
#' @import ggplot2
#' @import dplyr
#' @import knitr


sfi_plot_dumas_1 <- function() {

  # Get data
  data <- all_data$dumas$f1

  # plot the true negative (x) against true positve (y)
  ggplot(data, 
         aes())
  
  # out <- list(g1, g2, g3, g4)
  return(out)

}


