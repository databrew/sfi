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
                   bins = 75,
                   alpha = 0.6) +
    labs(x = 'Clarity score', 
         y = 'Density', 
         title = '') + 
    theme_sfi() + 
    scale_fill_manual(name = '',
                      values = make_colors(length(unique(data$federal)),
                                           categorical = TRUE, 
                                           bw = TRUE))
  
  
  # jitter
  g2 <- ggplot(data, 
               aes(x=federal,
                   y=clarity_score)) + 
    geom_jitter(width = 0.3,
                alpha = 0.1) + 
    labs(x = '',
         y = 'Clarity Score') + 
    theme_sfi()
  
  # density
  g3 <- ggplot(data, 
               aes(x=clarity_score, 
                   fill = federal)) + 
    geom_density() + 
    labs(x = 'Clarity Score',
         y = 'Density') + 
    theme_sfi() +
    scale_fill_manual(name = '',
                      values = make_colors(length(unique(data$federal)),
                                           categorical = TRUE, 
                                           bw = TRUE))
  
  # violin plot
  g4 <- ggplot(data, 
               aes(x=federal,
                   y=clarity_score)) + 
    geom_jitter(width = 0.4, 
                alpha = 0.2) +
    geom_violin(alpha = 0.5, fill = 'darkgrey') +
    labs(x = '',
         y = 'Clarity Score') + 
    theme_sfi() +
    scale_fill_manual(name = '',
                      values = make_colors(length(unique(data$federal)),
                                           categorical = TRUE, 
                                           bw = TRUE))
  
  
  
  out <- list(g1, g2, g3, g4)
  return(out)
  
}


