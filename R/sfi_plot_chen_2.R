#' Chen 2
#' 
#' Generate a plot for chen Figure 2
#' @export
#' @return a list of plots
#' @import ggplot2
#' @import dplyr
#' @import scales
#' @import Hmisc


sfi_plot_chen_2 <- function(){
  
  # # no scientific notation
  # options(scipen = '999')
  # 
  # # get data 
  data <- all_data$chen$f2
  
  
  # version 11
  g1 <- ggplot(data,
               aes(x = x,
                   y = y)) +
    ylim(c(-7, 18)) +
    geom_text(aes(label = `decade-s`),
              size = 2,
              alpha = 0.6,
              color = 'black') +
    labs(x = 'X',
         y = 'Y',
         title = 'Figure 2.',
         subtitle = 'Centered by Court Topic, Averaged by Court-Year, Labeled by Decade',
         caption = 'Court Decade, SC & CC Court Decade Vector, Demeaned by Circuit and Big Topic') +
    theme_sfi(lp = 'none',
              y_axis_title_style = 'bold',
              x_axis_title_style = 'bold',
              title_style = 'bold')
  

  # 
  # version 11
  g2 <- ggplot(data,
               aes(x = x,
                   y = y)) +
    geom_text(aes(label = `decade-s`),
              size = 2,
              alpha = 0.6,
              color = 'black') +
    stat_ellipse(aes(x, y,color= `decade-s`),type = "norm", size = 0.3, linetype = 'dashed', alpha = 0.7) +
    scale_color_manual(name = 'Confidence elipse',
                       values = c('#0D0D0D','#0D0D0D', '#0D0D0D','#0D0D0D','#0D0D0D','#0D0D0D',
                                  '#0D0D0D','#0D0D0D','#0D0D0D','#0D0D0D','#0D0D0D','#0D0D0D','#0D0D0D')) +
    labs(x = 'X',
         y = 'Y',
         title = 'Figure 2.',
         subtitle = 'Centered by Court Topic, Averaged by Court-Year, Labeled by Decade',
         caption = 'Court Decade, SC & CC Court Decade Vector, Demeaned by Circuit and Big Topic') +
    theme_sfi(lp = 'none',
              y_axis_title_style = 'bold',
              x_axis_title_style = 'bold',
              title_style = 'bold')
  return(list(g1, g2))
}
