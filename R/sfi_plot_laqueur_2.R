#' Laqueur 2
#' 
#' Generate a plot for Laqueur Figure 2
#' @export
#' @return a list of plots
#' @import ggplot2
#' @import dplyr
#' @import knitr
#' @import kableExtra
#' @import scales

sfi_plot_laqueur_2 <- function(){
  
  # get data 
  data <- all_data$laqueur$f2
  
  # version 6
  g1 <- 
    ggplot(data, 
           aes(x = year, 
               y = percent_of_conducted_hearings_resulting_in_a_grant)) +
    ylim(c(0, 50)) +
    geom_smooth(method = 'lm',
                alpha = 0.4,
                fill = 'black',
                linetype = 0) + 
    geom_point(size = 4, 
               color = 'black',
               alpha = 0.8,
               pch = 16) +
    geom_text(aes(label = paste0(percent_of_conducted_hearings_resulting_in_a_grant, 
                                 '%')),
              size = 5,
              color = 'black',
              nudge_y = 0,
              vjust = -2) +
    labs(x = '',
         y = '% of hearings resulting in a grant',
         title = 'Figure 2',
         subtitle = 'Rate of Parole Grant: 2007-2014',
         caption = paste0('Smoothed with a local regression', '\n', 
                          'with bands representing standard errors')) +
    theme_sfi(lp = 'none',
              y_axis_title_style = 'bold',
              x_axis_title_style = 'bold',
              title_style = 'bold')
 
  
  out <- list(g1)
  
  return(out)
}

