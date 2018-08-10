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

  
  # boxplot
  g1 <- 
    ggplot(data, 
               aes(x=federal,
                   y=words,
                   fill = federal)) + 
    geom_boxplot(alpha = 0.8, 
                 width = 0.7,
                 fill = 'black') + 
      geom_jitter(size = 0.8, 
                  alpha = 0.1,
                  pch = 1) +
    labs(x = '',
         y = 'Words',
         title = 'Version 1') + 
    theme_sfi() + 
    scale_fill_manual(name = '',
                      values = make_colors(length(unique(data$federal)),
                                           categorical = TRUE, 
                                           bw = TRUE)) + guides(fill = FALSE)
    
    # boxplot
   g2 <- 
    ggplot(data, 
           aes(x=federal,
               y=words,
               fill = federal)) + 
      geom_boxplot(alpha = 0.8, 
                   width = 0.7,
                   fill = 'darkgrey') + 
      geom_jitter(size = 1, 
                  alpha = 0.1,
                  pch = 22) +
      labs(x = '',
           y = 'Words',
           title = 'Version 2') + 
      theme_sfi() + 
      scale_fill_manual(name = '',
                        values = make_colors(length(unique(data$federal)),
                                             categorical = TRUE, 
                                             bw = TRUE)) + guides(fill = FALSE)
 
  # jitter with no legend for color difference
  g3<- 
    ggplot(data, 
               aes(x=federal,
                   y=words)) + 
    geom_jitter(width = 0.3,
                alpha = 0.6,
                size = 3,
                pch = 44) + 
    labs(x = '',
         y = 'Words',
         title = 'Version 3') + 
    theme_sfi()
  

  
  # violin plot
   g4 <- 
     ggplot(data, 
               aes(x=federal,
                   y=words)) + 

    geom_violin(alpha = 0.5, 
                fill = 'grey') +
       geom_jitter(width = 0.4, 
                   alpha = 0.1,
                   color = 'black',
                   size = 1) +
    labs(x = '',
         y = 'Words',
         title = 'Version 4') + 
    theme_sfi() +
    scale_fill_manual(name = '',
                      values = make_colors(length(unique(data$federal)),
                                           categorical = TRUE, 
                                           bw = TRUE))
     
     g5 <- 
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
            title = 'Version 5') + 
       theme_sfi() +
       scale_fill_manual(name = '',
                         values = make_colors(length(unique(data$federal)),
                                              categorical = TRUE, 
                                              bw = TRUE))
     
     # violin plot
     g6 <- 
     ggplot(data, 
            aes(x=federal,
                y=words)) + 
       geom_violin(alpha = 0.5, 
                   fill = 'grey') +
       geom_jitter(width = 0.5, 
                   alpha = 0.1,
                   color = 'black',
                   size = 1,
                   pch = 2) +
       labs(x = '',
            y = 'Words',
            title = 'Version 6') + 
       theme_sfi() +
       scale_fill_manual(name = '',
                         values = make_colors(length(unique(data$federal)),
                                              categorical = TRUE, 
                                              bw = TRUE))
   
  
     # density with line type for groups
     g7 <- 
     ggplot(data, aes(x = words)) +
       stat_density(aes(group = federal, linetype = federal),
                    position="identity",
                    geom="line", kernel = "gaussian", size = 0.5) +
       labs(x = 'Words', 
            y = 'Density', 
            title = 'Version 7 (similar to authro') + 
       theme_sfi() + scale_linetype_discrete(name = '')
     
  
  
  
  out <- list(g1, g2, g3, g4,
              g5, g6, g7)
  
  return(out)
  
}


