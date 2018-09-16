#' Eidelman 1
#' 
#' Generate a plot for eidelman Figure 1
#' @export
#' @return a list of plots
#' @import ggplot2
#' @import dplyr
#' @import scales
#' @import Hmisc
#' @import reshape2


sfi_plot_eidelman_1 <- function(){
  
  # # no scientific notation
  # options(scipen = '999')
  # 
  # # get data 
  data <- all_data$eidelman$f1
  
  # make data long format
  data <- melt(data, id.vars = 'State')
   
  # make states upper case
  data$State <- toupper(data$State)
  # barplot
  g1 <- ggplot(data,
               aes(reorder(x = State, value),
                   y = value,
                   fill = variable)) +
    scale_fill_manual(name = '',
                      values = c('black', 'darkgrey')) +
    geom_bar(stat = 'identity') +
    labs(x = '',
         y = '',
         title = 'Figure 1',
         subtitle = 'Number of bills introduced and receiving floor action for each state',
         caption = 'Version 1') +
    theme_sfi(lp = 'bottom',
              gM = FALSE,
              y_axis_title_style = 'bold',
              title_style = 'bold') +
    theme(axis.text.x=element_text(angle=90, vjust = 0.1, size = 7))
  
  # barplot with coord flip
  g2 <- ggplot(data,
               aes(reorder(x = State, value),
                   y = value,
                   fill = variable)) +
    scale_fill_manual(name = '',
                      values = c('black', 'darkgrey')) +
    geom_bar(stat = 'identity') +
    coord_flip() +
    labs(x = '',
         y = '',
         title = 'Figure 1',
         subtitle = 'Number of bills introduced and receiving floor action for each state',
         caption = 'Version 2') +
    theme_sfi(lp = 'bottom',
              gM = FALSE,
              y_axis_title_style = 'bold',
              title_style = 'bold') +
    theme(axis.text.y=element_text(size = 6, hjust = 1))
  
  # candlestick
  g3 <- ggplot(data, 
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
    labs(x = '',
         y = '',
         title = 'Figure 1',
         subtitle = 'Number of bills introduced and receiving floor action for each state',
         caption = 'Version 3') +
    theme_sfi(lp = 'bottom',
              gM = FALSE,
              y_axis_title_style = 'bold',
              title_style = 'bold') +
    theme(axis.text.x=element_text(angle=90, vjust = 0.1, size = 7))
  
  # candlestick flipped
  g4 <- ggplot(data, 
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
    coord_flip() +
    geom_point(size= 2, 
               alpha = 1) + 
    scale_color_manual(name = '', values = c('#000000', '#BFBFBF')) +
    labs(x = '',
         y = '',
         title = 'Figure 1',
         subtitle = 'Number of bills introduced and receiving floor action for each state',
         caption = 'Version 3') +
    theme_sfi(lp = 'bottom',
              gM = FALSE,
              y_axis_title_style = 'bold',
              title_style = 'bold') +
    theme(axis.text.y=element_text(size = 6))

  
  return(list(g1, g2, g3, g4))
}
