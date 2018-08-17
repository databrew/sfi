#' Feldman 1
#' 
#' Generate a plot for Feldman Figure 1 
#' @export
#' @return a list of plots
#' @import ggplot2
#' @import dplyr
#' @import knitr


sfi_plot_feldman_1 <- function() {
  
  # Get data
  data <- all_data$feldman$f1 
  
  # recode federal
  data$federal <- ifelse(data$federal == '1', 'Federal', 'State')

  
  
  g1 <- 
    ggplot(data, 
           aes(x=federal,
               y=clarity_score)) + 
    
    geom_violin(alpha = 0.8, 
                fill = 'black') +
    geom_jitter(width = 0.3, 
                alpha = 0.1,
                color = 'black',
                size = 1.5,
                stroke = 1,
                pch = 1) +
    labs(x = '',
         y = 'Clarity Score',
         title = 'Version 1') + 
    theme_sfi() +
    scale_fill_manual(name = '',
                      values = make_colors(length(unique(data$federal)),
                                           categorical = TRUE, 
                                           bw = TRUE))
  
    
  out <- list(g1, g2, g3, g4)
  return(out)
  
}


