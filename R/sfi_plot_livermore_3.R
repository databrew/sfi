#' Livermore 3
#'
#' Generate a plot for Livermore Figure 3
#' @export
#' @return a list of plots
#' @import ggplot2
#' @import dplyr
#' @import scales
#' @import Hmisc


sfi_plot_livermore_3 <- function(){
  
  # no scientific notation
  options(scipen = '999')
  
  # get data
  data <- all_data$livermore$f3
  
  # melt data by x1
  data <- melt(data, id.vars = 'x1')
  
  # livermore 3
  g1 <- ggplot(data,
               aes(variable, value)) +
    geom_jitter(width = 0.3,
                alpha = 0.2,
                size = 1) +
    geom_violin(fill = 'grey') +
    labs(x = '',
         y = 'Accuracy',
         title = 'Supreme Court vs Appellate court (cert. granted)',
         subtitle = 'Figure 3a. Prediction of Supreme Court and Appellate Court Opinions.',
         caption = 'Mirrored density plot') +
    theme_sfi(lp = 'none',
              y_axis_title_style = 'bold',
              title_style = 'bold')
  
  
  g2 <- ggplot(data, 
               aes(x = variable, 
                   y = value)) +
    stat_ydensity(geom="segment", 
                  adjust = 20,
                  scale = 'area',
                  aes(xend=..x..+..scaled../4, 
                      yend=..y.., 
                      alpha=(..scaled../3)^2), 
                  size=3, 
                  color = 'darkgrey',
                  trim=TRUE) +
    stat_ydensity(geom="segment", 
                  adjust = 20,
                  scale = 'area',
                  aes(xend=..x..-..scaled../4, 
                      yend=..y.., 
                      alpha=(..scaled../3)^2), 
                  size=3, 
                  linetype = 1,
                  color = 'darkgrey',
                  
                  trim=TRUE) +
    labs(x = '',
         y = 'Accuracy',
         title = 'Supreme Court vs Appellate court',
         subtitle = 'Figure 3a. Prediction of Supreme Court and Appellate Court Opinions.',
         caption = 'Mirrored density plot') +
    scale_alpha_continuous(range= c(-0, .5)) +
    geom_jitter(size = 1,
                color = 'black',
                width = 0.3,
                alpha = 0.2,
                pch = 16) +
    theme_sfi(lp = 'none',
              y_axis_title_style = 'bold',
              title_style = 'bold') 
  
  

 
  
  
  return(list(g1,g2))
}
