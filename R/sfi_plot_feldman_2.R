#' Feldman 2
#' 
#' Generate a plot for Feldman Figure 1 
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
  
  # density with line type for groups
  g1 <- ggplot(data, aes(x = words)) +
    stat_density(aes(group = federal, linetype = federal),
                 position="identity",
                 geom="line", kernel = "gaussian", size = 0.5) +
    labs(x = 'Words', 
         y = 'Density', 
         title = '') + 
    theme_sfi() 
  
  # density with color for groups
  g1 <- ggplot(data, aes(x = words)) +
    stat_density(aes(group = federal, color = federal),
                 position="identity",
                 geom="line", kernel = "gaussian", size = 0.5) +
    labs(x = 'Words', 
         y = 'Density', 
         title = '') + 
    theme_sfi() +
    scale_fill_manual(name = '',
                      values = make_colors(length(unique(data$federal)),
                                           categorical = TRUE, 
                                           bw = TRUE))
  
  # histogram with density
  g2 <-  ggplot(data, 
                aes(x = words, 
                    fill = federal))  +
    geom_histogram(aes(y=..density..), 
                   bins = 30,
                   alpha = 0.6,
                   color = 'black') +
    labs(x = 'Words', 
         y = 'Density', 
         title = '') + 
    theme_sfi() + 
    scale_fill_manual(name = '',
                      values = make_colors(length(unique(data$federal)),
                                           categorical = TRUE, 
                                           bw = TRUE))
  
  
  # boxplot
  g2 <- ggplot(data, 
               aes(x=federal,
                   y=words)) + 
    geom_boxplot() + 
    labs(x = '',
         y = 'Words') + 
    theme_sfi() +
    
  
  # boxplot
  g2 <- ggplot(data, 
               aes(x=federal,
                   y=words)) + 
    geom_boxplot() + 
    labs(x = '',
         y = 'Words') + 
    theme_sfi() +
    scale_y_log10()
  
  # jitter
  g2 <- ggplot(data, 
               aes(x=federal,
                   y=words)) + 
    geom_jitter(width = 0.3,
                alpha = 0.1) + 
    labs(x = '',
         y = 'Words') + 
    theme_sfi()
  
  # density
  g3 <- ggplot(data, 
               aes(x=words, 
                   fill = federal)) + 
    geom_density(alpha = 0.7) + 
    labs(x = 'Words',
         y = 'Density') + 
    theme_sfi() +
    scale_fill_manual(name = '',
                      values = make_colors(length(unique(data$federal)),
                                           categorical = TRUE, 
                                           bw = TRUE))
  
  # violin plot
  g4 <- ggplot(data, 
               aes(x=federal,
                   y=words)) + 
    geom_jitter(width = 0.2, 
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


