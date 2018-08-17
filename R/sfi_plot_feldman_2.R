#' Feldman 2
#' 
#' Generate a plot for Feldman Figure 2
#' @export
#' @return a list of plots
#' @import ggplot2
#' @import dplyr
#' @import knitr


sfi_plot_feldman_2 <- function(){
  
  options(scipen = '999')
  
  # Get data
  data <- all_data$feldman$f2 
  
  # recode federal
  data$federal <- ifelse(data$federal == '1', 'Federal', 'State')

  
  
  g1 <- 
  ggplot(data, 
        aes(x=federal,
            y=words)) + 
   
   geom_violin(alpha = 0.8, 
               fill = 'black') +
   geom_jitter(width = 0.3, 
               alpha = 0.1,
               color = 'black',
               size = 1.5,
               stroke = 1,
               pch = 1) +
   labs(x = '',
        y = 'Words',
        title = 'Version 1') + 
   theme_sfi() +
   scale_fill_manual(name = '',
                     values = make_colors(length(unique(data$federal)),
                                          categorical = TRUE, 
                                          bw = TRUE))
  

  
  
  
  out <- list(g1, g2, g3, g4,
              g5, g6, g7)
  
  return(out)
  
}


