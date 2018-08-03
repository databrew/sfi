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

sfi_plot_laqueur_2 <- function(){
  
  # no scientific notation
  options(scipen = '999')
  
  # get data 
  data <- all_data$livemore$f1
  
  # version 1 
  ggplot(data, 
               aes(x = median_year, 
                   y = friendscr)) +
    geom_point(size = 1, 
              alpha = 0.6) +
    geom_smooth(method = 'lm', 
                se = FALSE,
                size = 0.5,
                color = 'black',
                alpha = 0.6) +
    labs(x = '',
         y = 'Friendliness score',
         title = 'Version 1') +
    scale_y_continuous(labels = percent, 
                       limits = c(-0.018, 0.004),
                       breaks=c(-0.018,-0.016,-0.014,-0.012,-0.01, -0.008, 
                                -0.006, -0.004, -0.002, 0, 0.002, 0.004)) +
    theme_sfi()

  
  return(out)
}