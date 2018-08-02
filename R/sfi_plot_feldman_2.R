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
  g2 <- ggplot(data, aes(x = words)) +
    stat_density(aes(group = federal, 
                     color = federal),
                 position="identity",
                 geom="line", 
                 kernel = "gaussian", 
                 size = 0.5) +
    labs(x = 'Words', 
         y = 'Density', 
         title = '') + 
    theme_sfi() +
    scale_color_manual(name = '',
                      values = make_colors(length(unique(data$federal)),
                                           categorical = TRUE, 
                                           bw = TRUE))
  
  # histogram with density
  g3 <-  ggplot(data, 
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
  g4 <- ggplot(data, 
               aes(x=federal,
                   y=words,
                   fill = federal)) + 
    geom_boxplot() + 
    labs(x = '',
         y = 'Words') + 
    theme_sfi() + 
    scale_fill_manual(name = '',
                      values = make_colors(length(unique(data$federal)),
                                           categorical = TRUE, 
                                           bw = TRUE))
    
  
  # boxplot
  g5 <- ggplot(data, 
               aes(x=federal,
                   y=words)) + 
    geom_boxplot() + 
    labs(x = '',
         y = 'Words (Log scale)') + 
    theme_sfi() +
    scale_y_log10() +
    scale_fill_manual(name = '',
                      values = make_colors(length(unique(data$federal)),
                                           categorical = TRUE, 
                                           bw = TRUE)) 
  
  # jitter with no legend for color difference
  g6 <- ggplot(data, 
               aes(x=federal,
                   y=words)) + 
    geom_jitter(width = 0.3,
                alpha = 0.3) + 
    labs(x = '',
         y = 'Words') + 
    theme_sfi()
  
  # jitter with legned
  g7 <- ggplot(data, 
               aes(x=federal,
                   y=words,
                   color = federal)) + 
    geom_jitter(width = 0.3,
                alpha = 0.3) + 
    labs(x = '',
         y = 'Words') + 
    theme_sfi() +
    scale_color_manual(name = '',
                      values = make_colors(length(unique(data$federal)),
                                           categorical = TRUE, 
                                           bw = TRUE)) 
  
  # density
  g8 <- ggplot(data, 
               aes(x=words, 
                   fill = federal)) + 
    geom_density(alpha = 0.7, color = 'grey') + 
    labs(x = 'Words',
         y = 'Density') + 
    theme_sfi() +
    scale_fill_manual(name = '',
                      values = make_colors(length(unique(data$federal)),
                                           categorical = TRUE, 
                                           bw = TRUE))
  
  # violin plot
  g9 <- ggplot(data, 
               aes(x=federal,
                   y=words)) + 
    geom_jitter(width = 0.2, 
                alpha = 0.9,
                color = 'grey') +
    geom_violin(alpha = 0.5, 
                fill = 'white') +
    labs(x = '',
         y = 'Words') + 
    theme_sfi() +
    scale_fill_manual(name = '',
                      values = make_colors(length(unique(data$federal)),
                                           categorical = TRUE, 
                                           bw = TRUE))
  
  # violin plot
   g11 <- ggplot(data, 
               aes(x=federal,
                   y=words)) + 
    geom_jitter(width = 0.2, 
                alpha = 0.9,
                color = 'black') +
    geom_violin(alpha = 0.5, 
                fill = 'white') +
    labs(x = '',
         y = 'Words') + 
    theme_sfi() +
    scale_fill_manual(name = '',
                      values = make_colors(length(unique(data$federal)),
                                           categorical = TRUE, 
                                           bw = TRUE))
   
   # violin plot
   g12 <- ggplot(data, 
                 aes(x=federal,
                     y=words)) + 
     geom_violin(alpha = 0.5, 
                 fill = 'black') +
     labs(x = '',
          y = 'Words') + 
     theme_sfi() +
     scale_fill_manual(name = '',
                       values = make_colors(length(unique(data$federal)),
                                            categorical = TRUE, 
                                            bw = TRUE))
  
  
  
  out <- list(g1, g2, g3, g4,
              g5, g6, g7, g8, 
              g9, g10, g11, g12)
  
  return(out)
  
}


