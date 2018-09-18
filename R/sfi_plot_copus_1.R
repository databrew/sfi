#' Copus 1
#' 
#' Generate a plot for Copus Figure 1
#' @export
#' @return a list of plots
#' @import ggplot2
#' @import dplyr
#' @import scales
#' @import Hmisc
#' @import reshape2


sfi_plot_copus_1 <- function(){
  
  # # no scientific notation
  # options(scipen = '999')
  # 
  # # get data 
  data <- all_data$copus$f1
  
  # plot point, line, smooth data
  g1 <- ggplot(data, 
               aes(x, y)) +
    geom_point(size = 1,
               color = 'black', 
               alpha = 0.6) +
    geom_smooth(method = 'loess',
                linetype =0,
                fill = 'black',
                alpha = 0.4) +
    geom_smooth(method = 'lm',
                color = 'black',
                se = FALSE) +
    labs(title = 'Figure 1. In Sample Prediction') +
    theme_sfi(lp = 'none',
              x_axis_title_style = 'bold',
              y_axis_title_style = 'bold',
              title_style = 'bold')
  
  # plot point, line, smooth data
  g2 <- ggplot(data, 
               aes(x, y)) +
    geom_point(size = 1.5,
               color = 'black', 
               alpha = 0.9) +
    geom_smooth(method = 'loess',
                linetype = 0,
                se = TRUE,
                fill = 'black',
                alpha = 0.7) +
    geom_smooth(method = 'lm',
                color = 'darkgrey',
                linetype = 'dashed',
                se = FALSE,
                alpha = 0.4) +
    labs(title = 'Figure 1. In Sample Prediction',
         subtitle = 'Version 2') +
    theme_sfi(lp = 'none',
              x_axis_title_style = 'bold',
              y_axis_title_style = 'bold',
              title_style = 'bold')
  
  return(list(g1, g2))
}
