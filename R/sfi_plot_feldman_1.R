#' Feldman 1
#' 
#' Generate a plot for Feldman Figure 1 
#' @export
#' @return a list of plots
#' @import ggplot2
#' @import dplyr
#' @import knitr


sfi_plot_feldman_1 <- function(){
  
  # Get data
  data <- all_data$feldman$f1 
  
  # recode federal
  data$federal <- ifelse(data$federal == '1', 'Federal', 'State')
 
  # histogram with density
   g1 <-  ggplot(data, 
                aes(x = clarity_score, 
                    fill = federal))  +
    geom_histogram(aes(y=..density..), 
                   bins = 75) +
    labs(x = 'Clarity score', 
         y = 'Density', 
         title = 'Figure 1') + 
    theme_sfi() + 
      scale_fill_manual(name = '',
                        values = make_colors(length(data$federal),
                                            categorical = TRUE, 
                                            bw = TRUE))
   # box plot
   ggplot(data, aes(x=federal, y=clarity_score)) + geom_jitter(width = 0.1,
                                                               alpha = 0.2) +
     geom_violin() + 
     scale_y_log10()
   
  
  
  return(out)
  
}


