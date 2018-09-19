#' Eidelman 2
#' 
#' Generate a plot for eidelman Figure 2
#' @export
#' @return a list of plots
#' @import ggplot2
#' @import dplyr
#' @import scales
#' @import Hmisc
#' @import reshape2


sfi_plot_eidelman_2 <- function(){
  
  # # no scientific notation
  # options(scipen = '999')
  # 
  # # get data 
  data <- all_data$eidelman$f2
 
  # make states upper case
  data$State <- toupper(data$State)
  names(data) <- c('State','value')
 
  # forloop that inserts a skipped line between states 
  for(i in 1:nrow(data)) {
    sub_state <- data$State[i]
    paste(sub_state[1])
    sub_state <- unlist(strsplit(sub_state, ''))
    final_state <- paste0(sub_state[1], '\n', sub_state[2])
    data$State[i] <- final_state
  }
  
  # barplot
  g1 <- ggplot(data,
               aes(reorder(x = State, value),
                   y = value)) +
    geom_bar(stat = 'identity', width = 0.9) +
    geom_text(aes(label = State), size = 2, vjust = 1.2, color = 'white') +
    geom_text(aes(label = round((data$value*100))), size = 2, vjust = -1, color = 'black') +
    scale_y_continuous(labels = percent) +
    labs(x = '',
         y = '',
         title = 'Figure 2',
         subtitle = 'Percent of bills reaching floor per state',
         caption = 'Version 1') +
    theme_sfi(lp = 'bottom',
              gM = FALSE,
              y_axis_title_style = 'bold',
              title_style = 'bold') +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank())

  
  # # get data 
  data <- all_data$eidelman$f2
  
  # make states upper case
  data$State <- toupper(data$State)
  names(data) <- c('State','value')
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
               alpha = 1) + 
    scale_y_continuous(labels = percent) +
    labs(x = '',
         y = '',
         title = 'Figure 2',
         subtitle = 'Percent of bills reaching floor per state',
         caption = 'Version 2') +
    theme_sfi(lp = 'bottom',
              gM = FALSE,
              y_axis_title_style = 'bold',
              title_style = 'bold') +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_text(angle=90, hjust = 1, size = 7),
          axis.ticks.length = unit(0, "lines"),
          axis.ticks.x=element_blank()) 

  
  return(list(g1, g2))
}

