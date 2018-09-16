#' Chen 1
#' 
#' Generate a plot for chen Figure 1
#' @export
#' @return a list of plots
#' @import ggplot2
#' @import dplyr
#' @import scales
#' @import Hmisc


sfi_plot_chen_1 <- function(){
  
  # # no scientific notation
  # options(scipen = '999')
  # 
  # # get data 
  data <- all_data$chen$f1
  
  # get label numbers
  data$text_label <- unlist(lapply(strsplit(data$circuit, '-', fixed = TRUE), function(x) x[length(x)]))
  
  # remove 11 with FC and SC with 12, and 0 with dc
  data$text_label <- gsub('^0$', 'DC', data$text_label)
  data$text_label <- gsub('12', 'FC', data$text_label)

  # 
  # version 11
  g1 <- ggplot(data,
               aes(x = x,
                   y = y)) +
    geom_text(aes(label = text_label),
              size = 2,
              alpha = 0.6,
              pch = 12,
              color = 'black') +
    labs(x = 'X',
         y = 'Y',
         title = 'Figure 1: Centered by Topic-Year, Averaged by Judge, Labeled by Court',
         subtitle = 'Circuit, CC Judge Vector, Demeaned by Year and Big Topic',
         caption = '') +
    theme_sfi(lp = 'none',
              y_axis_title_style = 'bold',
              title_style = 'bold')
  
  
  return(list(g1))
}
