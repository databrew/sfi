#' Eidelman 5
#' 
#' Generate a plot for eidelman Figure 5
#' @export
#' @return a list of plots
#' @import ggplot2
#' @import dplyr
#' @import scales
#' @import Hmisc
#' @import reshape2


sfi_plot_eidelman_5 <- function(){
  
  # # no scientific notation
  # options(scipen = '999')
  # 
  # # get data 
  data <- all_data$eidelman$f5
  
  # make data long format
  data <- melt(data, id.vars = 'State')
  
  # barplot
  g1 <- ggplot(data,
               aes(reorder(x = State, value),
                   y = value,
                   fill = variable)) +
    scale_fill_manual(name = '',
                      values = c('black', 'darkgrey')) +
    geom_bar(stat = 'identity') +
    labs(x = 'State/Chamber',
         y = '',
         title = 'Figure 5',
         subtitle = 'Prediction accuracy on bills with combined models',
         caption = 'Version 1') +
    theme_sfi(lp = 'bottom',
              gM = FALSE,
              gm = FALSE,
              y_axis_title_style = 'bold',
              title_style = 'bold') +
    theme(axis.text.x=element_text(angle=90, vjust = 0.1, size = 6)) 
    
  
  
  # candlestick
  g2 <- ggplot(data, 
               aes(State, 
                   value, 
                   color = variable)) + 
    geom_segment(aes(x=reorder(State, -value), 
                     xend=State, 
                     y=0, 
                     yend=value),
                 size = 1,
                 color = 'black',
                 alpha = 0.5) + 
    geom_point(size= 2, 
               alpha = 1) + 
    scale_color_manual(name = '', values = c('#000000', '#BFBFBF')) +
    labs(x = 'State/Chamber',
         y = '',
         title = 'Figure 5',
         subtitle = 'Prediction accuracy on bills with combined models',
         caption = 'Version 2') +
    theme_sfi(lp = 'bottom',
              gM = FALSE,
              y_axis_title_style = 'bold',
              title_style = 'bold') +
    theme(axis.text.x=element_text(angle=90, vjust = 0.1, size = 6))
  
  # candlestick flipped

  
  return(list(g1, g2))
}
