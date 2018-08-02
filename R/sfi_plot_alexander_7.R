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
  
  g <- 
    ggplot(data = data,
         aes(x = key,
             y = value)) +
    geom_bar(stat = 'identity',
             alpha = 0.8) +
    theme_sfi() +
    labs(y = 'Percent',
         x = 'Judges\' Actions') +
    geom_text(aes(label = round(value, digits = 1)),
              nudge_y = 4,
              size = 4,
              alpha = 0.7)
  return(list(g))
}