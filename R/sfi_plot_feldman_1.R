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
  
  # histogram with density
  #g1 <-  
    ggplot(data, 
                aes(x = clarity_score, 
                    fill = federal))  +
    geom_histogram(aes(y=..density..), 
                   bins = 75,
                   alpha = 0.6) +
    labs(x = 'Clarity score', 
         y = 'Density', 
         title = 'Version 1') + 
    theme_sfi() + 
    scale_fill_manual(name = '',
                      values = make_colors(length(unique(data$federal)),
                                           categorical = TRUE, 
                                           bw = TRUE))
  
  # violin plot
  #g4 <- 
    ggplot(data, 
               aes(x=federal,
                   y=clarity_score)) + 
    geom_violin(alpha = 0.5, 
                fill = 'darkgrey') +
      geom_jitter(width = 0.4, 
                  alpha = 0.3,
                  size = 0.7,
                  color = '#464646', 
                  stroke = 0
                  ) +
    labs(x = '',
         y = 'Clarity Score',
         title = 'Version 4',
         caption = 'violin plot') + 
    theme_sfi() +
    scale_fill_manual(name = '',
                      values = make_colors(length(unique(data$federal)),
                                           categorical = TRUE, 
                                           bw = TRUE))
  
    #g5 <- 
    ggplot(data, 
           aes(x=federal,
               y=clarity_score)) + 
      geom_boxplot(alpha = 0.5, 
                  fill = 'darkgrey') +
      geom_jitter(width = 0.4, 
                  alpha = 0.1,
                  size = 1,
                  color = 'black', 
                  stroke = 0
      ) +
      labs(x = '',
           y = 'Clarity Score',
           title = 'Version 4',
           caption = 'violin plot') + 
      theme_sfi() +
      scale_fill_manual(name = '',
                        values = make_colors(length(unique(data$federal)),
                                             categorical = TRUE, 
                                             bw = TRUE))
    
  
  
  out <- list(g1, g2, g3, g4)
  return(out)
  
}


