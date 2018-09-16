#' Chen 4
#' 
#' Generate a plot for chen Figure 4
#' @export
#' @return a list of plots
#' @import ggplot2
#' @import dplyr
#' @import scales
#' @import Hmisc


sfi_plot_chen_4 <- function(){
  
  # # no scientific notation
  # options(scipen = '999')
  # 
  # # get data 
  data <- all_data$chen$f4
  
  # recode party affiliation
  data <- data[data$party %in% c(1,0),]
  data$party <- ifelse(data$party == 0, 'R', 'D')
  
  # 
  # version 11
  g1 <- ggplot(data,
               aes(x = x,
                   y = y,
                   label = party,
                   color = party)) +
    geom_point(size = 0, 
               alpha = 0) +
    geom_text(size = 2,
              alpha = 0.6,
              show.legend = FALSE) +
    scale_color_manual(name = 'Party affiliation',
                       values = c('black', '#4F4F4F'),
                       breaks = c('D', 'R'),
                       labels = c('D = Democrat', 'R = Republican')) +
      guides(label = guide_legend(override.aes = list(text = 0)),
             color = guide_legend(override.aes = list(text = 0))) +
    labs(x = 'X',
         y = 'Y',
         title = 'Figure 4: Centered by Court-Topic-Year, Averaged by Judge, Labeled by Political Party',
         subtitle = 'Party affiliation, SC & CC Judge Vector, Demeaned by Circuit, Big Topics, and year',
         caption = 'Version 1') +
    theme_sfi(lp = 'bottom',
              y_axis_title_style = 'bold',
              title_style = 'bold')
  
  
  
  g2 <- ggplot(data,
               aes(x = x,
                   y = y,
                   shape = party)) +
    geom_point(size = 2, 
               alpha = 0.7) +
    scale_shape_manual(name = 'Party affiliation',
                       values = c(0, 16),
                       labels = c('Democrat', 'Republican')) +
    guides(label = guide_legend(override.aes = list(text = 0)),
           color = guide_legend(override.aes = list(text = 0))) +
    labs(x = 'X',
         y = 'Y',
         title = 'Figure 4: Centered by Court-Topic-Year, Averaged by Judge, Labeled by Political Party',
         subtitle = 'Party affiliation, SC & CC Judge Vector, Demeaned by Circuit, Big Topics, and year',
         caption = '') +
    theme_sfi(lp = 'bottom',
              y_axis_title_style = 'bold',
              title_style = 'bold')

  
  
  return(list(g1, g2))
}
