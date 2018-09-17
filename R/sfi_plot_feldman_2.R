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

  
  #Version 1
  g1<- ggplot(data, 
              aes(x = federal, 
                  y = words)) +
    stat_ydensity(geom="segment", 
                  adjust = 20,
                  scale = 'area',
                  aes(xend=..x..+..scaled../3.5, 
                      yend=..y.., 
                      alpha=(..scaled../3)^5), 
                  size=3, 
                  color = 'darkgrey',
                  trim=TRUE) +
    stat_ydensity(geom="segment", 
                  adjust = 20,
                  scale = 'area',
                  aes(xend=..x..-..scaled../3.5, 
                      yend=..y.., 
                      alpha=(..scaled../3)^5), 
                  size=3, 
                  linetype = 1,
                  color = 'darkgrey',
                  
                  trim=TRUE) +
    labs(x = '',
         y = 'Clarity Score',
         title = 'Figure 2. Distribution of Federal and State Court Opinion Word Length (Version 1)',
         caption = 'The shading is a function of the point distribution') +
    geom_jitter(size = 1,
                color = 'black',
                width = 0.3,
                alpha = 0.5,
                pch = 16) +
    theme_sfi(lp = 'none',
              y_axis_title_style = 'bold',
              x_axis_title_style = 'bold',
              title_style = 'bold')
  
  # Version 2
  g2 <- ggplot(data,
               aes(x = federal,
                   y = words)) +
    stat_ydensity(geom="segment",
                  adjust = 20,
                  scale = 'area',
                  aes(xend=..x..+..scaled../3,
                      yend=..y..,
                      alpha=..scaled../2),
                  size=3,
                  color = 'darkgrey',
                  trim=FALSE) +
    stat_ydensity(geom="segment",
                  adjust = 20,
                  scale = 'area',
                  aes(xend=..x..-..scaled../3,
                      yend=..y..,
                      alpha=..scaled../2),
                  size=3,
                  linetype = 1,
                  color = 'darkgrey',
                  trim=FALSE) +
    labs(x = '',
         y = 'Clarity Score',
         title = 'Figure 2. Distribution of Federal and State Court Opinion Word Length (Version 2)',
         caption = 'The shading is a function of the point distribution') +
    geom_jitter(size = 1,
                color = 'black',
                width = 0.3,
                alpha = 0.8,
                pch = 16) +
    theme_sfi(lp = 'none',
              y_axis_title_style = 'bold',
              x_axis_title_style = 'bold',
              title_style = 'bold') 
  
  out <- list(g1, g2)
  
  return(out)
  
}


