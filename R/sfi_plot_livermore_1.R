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
  data <- all_data$livemore$f1
  
  # version 1 
  g1 <- ggplot(data, 
               aes(x = median_year, 
                   y = friendscr)) +
    geom_point(size = 1, 
               alpha = 0.6) +
    geom_smooth(method = 'lm', 
                se = TRUE,
                size = 0.5,
                color = 'black',
                alpha = 0.6) +
    labs(x = '',
         y = 'Friendliness score',
         title = 'Version 1',
         caption = '*The line fitted line was estimated with a linear regression (showing standard errors).') +
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
  
  # version 2
  g2 <- ggplot(data, 
               aes(x = median_year, 
                   y = friendscr)) +
    geom_point(size = 1, 
               alpha = 0.6) +
    geom_smooth(method = 'loess', 
                se = TRUE,
                size = 0.5,
                color = 'black',
                alpha = 0.6) +
    labs(x = '',
         y = 'Friendliness score',
         title = 'Version 2',
         caption = '*The line was smoothed with a local regression.') +
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
  g3 <- ggplot(data, 
               aes(x = median_year, 
                   y = friendscr)) +
    geom_point(size = 1.5, 
               alpha = 0.6,
               color = 'black',
               pch = 1) +
    geom_smooth(method = 'lm', 
                se = TRUE,
                size = 0.5,
                color = 'black',
                alpha = 0.6,
                linetype = 2) +
    labs(x = '',
         y = 'Friendliness score',
         title = 'Version 3',
         caption = '*The line fitted line was estimated with a linear regression.') +
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
  
  
  # version 2
  g4 <- ggplot(data, 
               aes(x = median_year, 
                   y = friendscr)) +
    geom_point(size = 2, 
               alpha = 0.8,
               color = 'black') +
    geom_smooth(method = 'loess', 
                se = TRUE,
                size = 1,
                color = 'darkgrey',
                alpha = 0.2) +
    labs(x = '',
         y = 'Friendliness score',
         title = 'Version 4',
         caption = '*The line was smoothed with a local regression.') +
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
