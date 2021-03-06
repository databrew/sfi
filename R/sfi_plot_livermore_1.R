#' Livermore 1
#' 
#' Generate a plot for Livermore Figure 1
#' @export
#' @return a list of plots
#' @import ggplot2
#' @import dplyr
#' @import scales
#' @import Hmisc


sfi_plot_livermore_1 <- function(){
  
  # no scientific notation
  options(scipen = '999')
  
  # get data 
  data <- all_data$livermore$f1
  
  # version 11
  g1 <- ggplot(data, 
               aes(x = median_year, 
                   y = friendscr)) +
    geom_smooth(method = 'lm', 
                linetype = 0,
                se = TRUE,
                fill = '#373737',
                alpha = 0.4) +
    geom_point(size = 1.5, 
               alpha = 0.8,
               pch = 16,
               color = 'black') +
    labs(x = '',
         y = 'Friendliness score',
         title = '',
         caption = 'Figure 1: Sentiment Score by Authoring Justice') +
    scale_y_continuous(labels = percent, 
                       limits = c(-0.018, 0.004),
                       breaks=c(-0.018,-0.016,-0.014,-0.012,-0.01, -0.008, 
                                -0.006, -0.004, -0.002, 0, 0.002, 0.004)) +
    theme_sfi(lp = 'none',
              y_axis_title_style = 'bold',
              title_style = 'bold') +
    geom_text(data=subset(data, justice == 'alito'),
              aes(median_year, 
                  friendscr, 
                  label=paste0('Justice ', Hmisc::capitalize(justice))), 
              vjust = 1.5, 
              hjust = 1) 
  
  return(list(g1))
}
