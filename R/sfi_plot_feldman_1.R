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

  
  # VERSION 1
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
         title = 'Version 1',
         caption = paste0('The violin plot is a mirrored density plot showing', '\n', 
                          'a compact display of a continuous distribution')) + 
    theme_sfi() +
    scale_fill_manual(name = '',
                      values = make_colors(length(unique(data$federal)),
                                           categorical = TRUE, 
                                           bw = TRUE))
  
  # VERSION 2
  g2 <- 
    ggplot(data, 
           aes(x=federal,
               y=clarity_score)) + 
    
    geom_violin(alpha = 0.8, 
                fill = 'black') +
    geom_jitter(width = 0.3, 
                alpha = 0.1,
                color = 'black',
                size = 0.5,
                stroke = 0.5,
                pch = 1) +
    labs(x = '',
         y = 'Clarity Score',
         title = 'Version 2',
         caption = paste0('The violin plot is a mirrored density plot showing', '\n', 
                          'a compact display of a continuous distribution')) + 
    theme_sfi() +
    scale_fill_manual(name = '',
                      values = make_colors(length(unique(data$federal)),
                                           categorical = TRUE, 
                                           bw = TRUE))

  # VERSION 3
  g3 <- 
    ggplot(data, 
           aes(x=federal,
               y=clarity_score)) + 
    
    geom_violin(alpha = 0.8, 
                fill = 'black') +
    geom_jitter(width = 0.3, 
                alpha = 0.2,
                color = 'grey',
                size = 0.5,
                stroke = 1,
                pch = 16) +
    labs(x = '',
         y = 'Clarity Score',
         title = 'Version 3',
         caption = paste0('The violin plot is a mirrored density plot showing', '\n', 
                          'a compact display of a continuous distribution')) + 
    theme_sfi() +
    scale_fill_manual(name = '',
                      values = make_colors(length(unique(data$federal)),
                                           categorical = TRUE, 
                                           bw = TRUE))
  
  # VERSION 4
  g4 <- 
    ggplot(data, 
           aes(x=federal,
               y=clarity_score)) + 
    
    geom_violin(alpha = 0.8, 
                fill = 'black') +
    geom_jitter(width = 0.2, 
                alpha = 0.1,
                color = 'lightgrey',
                size = 0.5,
                stroke = 0.5,
                pch = 19) +
    labs(x = '',
         y = 'Clarity Score',
         title = 'Version 4',
         caption = paste0('The violin plot is a mirrored density plot showing', '\n', 
                          'a compact display of a continuous distribution')) + 
    theme_sfi() +
    scale_fill_manual(name = '',
                      values = make_colors(length(unique(data$federal)),
                                           categorical = TRUE, 
                                           bw = TRUE))
  
  # VERSION 5
  g5 <- 
    ggplot(data, 
           aes(x=federal,
               y=clarity_score)) + 
    geom_violin(alpha = 0.8, 
                fill = 'grey') +
    geom_jitter(width = 0.3, 
                alpha = 0.1,
                color = 'black',
                size = 0.5,
                stroke = 1,
                pch = 16) +
    labs(x = '',
         y = 'Clarity Score',
         title = 'Version 5',
         caption = paste0('The violin plot is a mirrored density plot showing', '\n', 
                          'a compact display of a continuous distribution')) + 
    theme_sfi() +
    scale_fill_manual(name = '',
                      values = make_colors(length(unique(data$federal)),
                                           categorical = TRUE, 
                                           bw = TRUE))
  
  out <- list(g1, g2, g3, g4, g5)
  return(out)
  
}


