#' Copus 2
#' 
#' Generate a plot for Copus Figure 2
#' @export
#' @return a list of plots
#' @import ggplot2
#' @import dplyr
#' @import scales
#' @import Hmisc
#' @import reshape2


sfi_plot_copus_2 <- function(){
  
  # # no scientific notation
  # options(scipen = '999')
  # 
  # # get data 
  data <- all_data$copus$f2
  
  # plot point, line, smooth data
  g1 <- ggplot(data, 
               aes(Reinhardt, Leavy,
                   color = Percent.Defendant.Win)) +
    scale_color_gradient(name = 'Defendant Trial Winner %', 
                         low = 'grey', high = 'black') +
    xlim(c(0, 1)) + 
    ylim(c(0,1)) +
    geom_point(size = 1,
               alpha = 0.8) +
    geom_abline(intercept = 0,
                slope = 1,
                color = 'black') +
    labs(title = 'Figure 2. Predicting the Votes of Ninth Circuit Judges') +
    theme_sfi(lp = 'bottom',
              y_axis_title_style = 'bold',
              x_axis_title_style = 'bold',
              title_style = 'bold')
  
  # plot point, line, smooth data
  g2 <- ggplot(data, 
               aes(Pregerson, Kleinfeld,
                   color = Percent.Defendant.Win)) +
    scale_color_gradient(name = 'Defendant Trial Winner %', 
                         low = 'grey', high = 'black') +
    xlim(c(0, 1)) + 
    ylim(c(0,1)) +
    geom_point(size = 1,
               alpha = 0.8) +
    geom_abline(intercept = 0,
                slope = 1,
                color = 'black') +
    labs(title = 'Figure 2. Predicting the Votes of Ninth Circuit Judges') +
    theme_sfi(lp = 'bottom',
              y_axis_title_style = 'bold',
              x_axis_title_style = 'bold',
              title_style = 'bold')
  

  
  return(list(g1, g2))
}
