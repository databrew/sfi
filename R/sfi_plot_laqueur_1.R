#' Laqueur 1
#' 
#' Generate a plot for Laqueur Figure 1 
#' @export
#' @return a list of plots
#' @import ggplot2
#' @import dplyr
#' @import knitr
#' @import kableExtra

sfi_plot_laqueur_1 <- function(){
  
  # get data 
  data <- all_data$laqueur$f1
  
  # version 1
  g1 <- 
    ggplot(data, 
           aes(x = year, 
               y = number_of_grants)) +
    geom_smooth(method = 'loess',
                alpha = 0.7,
                color = 'black',
                linetype = 0) + 
    geom_point(size = 3, 
               color = 'black',
               alpha = 0.8,
               pch = 16) +
    geom_text(aes(label = number_of_grants),
              size = 2,
              color = 'black',
              nudge_y = 0,
              vjust = -2) +
    labs(x = '',
         y = 'Number of grants',
         title = 'Figure 1',
         subtitle = 'Number of Hearings Resulting in a Grant: 1978-2015',
         caption = paste0('Smoothed with a local regression', '\n', 
                          'with bands representing standard errors')) +
    theme_sfi(lp = 'none',
              y_axis_title_style = 'bold',
              x_axis_title_style = 'bold',
              title_style = 'bold')
  


  out <- list(g1)
  
  return(out)
}