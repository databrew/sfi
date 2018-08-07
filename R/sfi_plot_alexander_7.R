#' Alexander 7
#' 
#' Generate a plot for Alexander Figure 7
#' @export
#' @return a list of plots
#' @import ggplot2
#' @import dplyr

sfi_plot_alexander_7 <- function(){
  
  # Get data
  data <- all_data$alexander$f7
  data$value <- data$value * 100
  
  # need to find plots that represent simple pie chart data 
  
  # pie chart 
  
  # side bar chart
  
  return(list(g1, g2, g3, g4, g5))
}