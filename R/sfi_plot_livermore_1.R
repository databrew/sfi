#' Laqueur 2
#' 
#' Generate a plot for Laqueur Figure 2
#' @export
#' @return a list of plots
#' @import ggplot2
#' @import dplyr
#' @import knitr
#' @import kableExtra
#' @import scales
#' @import Hmisc


sfi_plot_livermore_1 <- function(){
  
  # no scientific notation
  options(scipen = '999')
  
  # get data 
  data <- all_data$livermore$f1
  
  # version 1 
  g1 <- ggplot(data, 
               aes(x = median_year, 
                   y = friendscr)) +
    geom_point(size = 3.5, 
               alpha = 0.5,
               pch = 21,
               stroke = 2,
               fill = 'black', 
               color = 'grey') +
    geom_smooth(method = 'lm', 
                linetype = 0,
                se = TRUE,
                size = 0.7,
                fill = 'black',
                alpha = 0.5) +
    labs(x = '',
         y = 'Friendliness score',
         title = 'Version 1',
         caption = 'Standard errors estimated with a linear regression') +
    scale_y_continuous(labels = percent, 
                       limits = c(-0.018, 0.004),
                       breaks=c(-0.018,-0.016,-0.014,-0.012,-0.01, -0.008, 
                                -0.006, -0.004, -0.002, 0, 0.002, 0.004)) +
    theme_sfi() +
    geom_text(data=subset(data, justice == 'alito'),
              aes(median_year, 
                  friendscr, 
                  label=paste0('Justice ', Hmisc::capitalize(justice))), 
              vjust = 1.5, 
              hjust = 1) 
  
  # version 1 
  g2 <- ggplot(data, 
               aes(x = median_year, 
                   y = friendscr)) +
    geom_point(size = 1.5, 
               alpha = 0.3,
               pch = 19,
               stroke = 2,
               color = 'black') +
    geom_smooth(method = 'loess', 
                linetype = 0,
                se = TRUE,
                fill = 'black',
                alpha = 0.8) +
    labs(x = '',
         y = 'Friendliness score',
         title = 'Version 2',
         caption = paste0('Smoothed with a local regression', '\n', 
         'with bands representing standard errors')) +
    scale_y_continuous(labels = percent, 
                       limits = c(-0.018, 0.004),
                       breaks=c(-0.018,-0.016,-0.014,-0.012,-0.01, -0.008, 
                                -0.006, -0.004, -0.002, 0, 0.002, 0.004)) +
    theme_sfi() +
    geom_text(data=subset(data, justice == 'alito'),
              aes(median_year, 
                  friendscr, 
                  label=paste0('Justice ', Hmisc::capitalize(justice))), 
              vjust = 1.5, 
              hjust = 1) 
  
  
  # version 3
  g3 <- ggplot(data, 
               aes(x = median_year, 
                   y = friendscr)) +
    geom_point(size = 3.5, 
               alpha = 0.4,
               pch = 16,
               color = 'black') +
    geom_smooth(method = 'lm', 
                linetype = 0,
                se = TRUE,
                fill = 'black',
                alpha = 0.8) +
    labs(x = '',
         y = 'Friendliness score',
         title = 'Version 3',
         caption = 'Standard errors estimated with a linear regression') +
    scale_y_continuous(labels = percent, 
                       limits = c(-0.018, 0.004),
                       breaks=c(-0.018,-0.016,-0.014,-0.012,-0.01, -0.008, 
                                -0.006, -0.004, -0.002, 0, 0.002, 0.004)) +
    theme_sfi() +
    geom_text(data=subset(data, justice == 'alito'),
              aes(median_year, 
                  friendscr, 
                  label=paste0('Justice ', Hmisc::capitalize(justice))), 
              vjust = 1.5, 
              hjust = 1) 
  
  
  # version 4
  g4 <- ggplot(data, 
               aes(x = median_year, 
                   y = friendscr)) +
    geom_point(size = 3.5, 
               alpha = 0.7,
               pch = 16,
               color = 'black') +
    geom_smooth(method = 'lm', 
                linetype = 0,
                se = TRUE,
                fill = 'black',
                alpha = 0.4) +
    labs(x = '',
         y = 'Friendliness score',
         title = 'Version 4',
         caption = 'Standard errors estimated with a linear regression') +
    scale_y_continuous(labels = percent, 
                       limits = c(-0.018, 0.004),
                       breaks=c(-0.018,-0.016,-0.014,-0.012,-0.01, -0.008, 
                                -0.006, -0.004, -0.002, 0, 0.002, 0.004)) +
    theme_sfi() +
    geom_text(data=subset(data, justice == 'alito'),
              aes(median_year, 
                  friendscr, 
                  label=paste0('Justice ', Hmisc::capitalize(justice))), 
              vjust = 1.5, 
              hjust = 1)
  

  out <- list(g1, g2, g3, g4)
  
  return(out)
}
