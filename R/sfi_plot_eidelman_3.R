#' Eidelman 3
#' 
#' Generate a plot for eidelman Figure 3
#' @export
#' @return a list of plots
#' @import ggplot2
#' @import dplyr
#' @import scales
#' @import Hmisc
#' @import reshape2


sfi_plot_eidelman_3 <- function(){
  
  # # no scientific notation
  # options(scipen = '999')
  # 
  # # get data 
  data <- all_data$eidelman$f3
  names(data) <- c('State', 'value')
  
  g1 <- ggplot(data,
               aes(reorder(x = State, value),
                   y = value)) +
    geom_bar(stat = 'identity', width = 0.8) +
    scale_y_continuous(labels = percent) +
    labs(x = '',
         y = '',
         title = 'Figure 3',
         subtitle = 'Change from baseline with text only features',
         caption = 'Version 1') +
    theme_sfi(lp = 'bottom',
              gM = FALSE,
              y_axis_title_style = 'bold',
              title_style = 'bold') +
    theme(axis.text.x=element_text(angle=90, vjust = 0.1, size = 6))
  
  
  

  
  # candlestick
  g2 <- ggplot(data,
               aes(reorder(x = State, 
                           value),
                   y = value)) +
    geom_segment(aes(x=reorder(State, -value), 
                     xend=State, 
                     y=0, 
                     yend=value),
                 size = 1,
                 color = 'black',
                 alpha = 0.5) + 
    geom_point(size= 4, 
               alpha = 0.5) + 
    scale_y_continuous(labels = percent) +
    labs(x = '',
         y = '',
         title = 'Figure 3',
         subtitle = 'Change from baseline with text only features',
         caption = 'Version 2') +
    theme_sfi(lp = 'bottom',
              gM = FALSE,
              y_axis_title_style = 'bold',
              title_style = 'bold') +
    theme(axis.title.x=element_blank(),
          axis.ticks.length = unit(0, "lines"),
          axis.ticks.x=element_blank()) +
    theme(axis.text.x=element_text(angle=90, vjust = 0.1, size = 7))
  
  
  
  
  
  
  return(list(g1, g2, g3))
}
