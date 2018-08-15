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
  
  # version 1 
  g1 <- 
    ggplot(data, 
           aes(x = year, 
               y = percent_of_conducted_hearings_resulting_in_a_grant)) +
    geom_point(size = 4, 
               alpha = 0.8,
               color = 'black',
               stroke = 2,
               pch = 21) +
    geom_line(size = 1,
              linetype = 'dashed',
              color = '#4D4D4D') +
    geom_text(aes(label = paste0(percent_of_conducted_hearings_resulting_in_a_grant, 
                                 '%')),
                  size = 4,
              nudge_y = 2,
              alpha = 0.9) +
    theme(legend.position="none") +
    scale_size_area(name = '',
                    max_size = 5) +
    labs(x = 'Year',
         y = 'Hearings resulting in grant',
         title = 'Version 1') +
    theme_sfi(lp = 'none')
  

  
  # version 2
  g2 <- 
    ggplot(data, 
           aes(x = year, 
               y = percent_of_conducted_hearings_resulting_in_a_grant)) +
    geom_smooth(method = 'loess',
                alpha = 0.7,
                color = 'black',
                fill = 'black',
                linetype = 0) + 
    geom_point(aes(size = percent_of_conducted_hearings_resulting_in_a_grant), 
               color = 'black',
               fill = 'white',
               alpha = 0.8,
               pch = 21) +
    geom_text(aes(label = paste0(percent_of_conducted_hearings_resulting_in_a_grant, 
                                 '%'),
                  size = percent_of_conducted_hearings_resulting_in_a_grant/8),
              nudge_x = 0) +
    scale_size_area(name = '',
                    max_size = 30) +
    xlim(c(2007, 2015)) + ylim(c(0, 50)) +
    theme(legend.position="none") +
    labs(x = 'Year',
         y = 'Hearings resulting in grant',
         title = 'Version 2',
         caption = paste0('Smoothed with a local regression', '\n', 
                          'with bands representing standard errors')) +
    theme_sfi(lp = 'none')

  # version 3
  g3 <- 
    ggplot(data, 
           aes(x = year, 
               y = percent_of_conducted_hearings_resulting_in_a_grant)) +
    geom_point(aes(size = percent_of_conducted_hearings_resulting_in_a_grant), 
               color = 'grey',
               fill = 'black',
               stroke = 2,
               alpha = 0.7,
               pch = 21) +
    geom_text(aes(label = paste0(percent_of_conducted_hearings_resulting_in_a_grant, 
                                 '%'),
              size = percent_of_conducted_hearings_resulting_in_a_grant/8),
              color = 'white',
              nudge_x = 0) +
    scale_size_area(name = '',
                    max_size = 30) +
    xlim(c(2007, 2015)) + ylim(c(0, 50)) +
    theme(legend.position="none") +
    labs(x = 'Year',
         y = 'Hearings resulting in grant',
         title = 'Version 3',
         caption = paste0('Smoothed with a local regression', '\n', 
                         'with bands representing standard errors')) +
    theme_sfi(lp = 'none', )
  
  
  # version 4
  g4 <- 
    ggplot(data, 
           aes(x = year, 
               y = percent_of_conducted_hearings_resulting_in_a_grant)) +
    geom_smooth(method = 'loess',
                alpha = 0.6,
                fill = 'black',
                linetype = 0) + 
    geom_point(size = 17, 
               color = 'black',
               fill = 'white',
               alpha = 0.6,
               pch = 21) +
    geom_text(aes(label = paste0(percent_of_conducted_hearings_resulting_in_a_grant, 
                                 '%')),
                  size = 5,
              color = 'black',
              nudge_y = 0) +
    theme(legend.position = 'none') +
    xlim(c(2007, 2015)) + ylim(c(0, 50)) +
    labs(x = 'Year',
         y = 'Hearings resulting in grant',
         title = 'Version 4',
         caption = paste0('Smoothed with a local regression', '\n', 
                          'with bands representing standard errors')) +
    theme_sfi(lp = 'none')
  
  
  # version 5
  g5 <- 
    ggplot(data, 
           aes(x = year, 
               y = percent_of_conducted_hearings_resulting_in_a_grant)) +
    geom_smooth(method = 'loess',
                alpha = 0.6,
                fill = 'grey',
                linetype = 0) + 
    geom_point(size = 25, 
               color = 'black',
               fill = 'black',
               alpha = 0.6,
               pch = 21) +
    geom_text(aes(label = paste0(percent_of_conducted_hearings_resulting_in_a_grant, 
                                 '%')),
              size = 5,
              color = 'grey',
              nudge_y = 0) +
    theme(legend.position = 'none') +
    xlim(c(2007, 2015)) + ylim(c(0, 50)) +
    labs(x = 'Year',
         y = 'Hearings resulting in grant',
         title = 'Version 5',
         caption = paste0('Smoothed with a local regression', '\n', 
                          'with bands representing standard errors')) +
    theme_sfi(lp = 'none')

  
  # version 6
  g6 <- 
    ggplot(data, 
           aes(x = year, 
               y = percent_of_conducted_hearings_resulting_in_a_grant)) +
    geom_smooth(method = 'loess',
                alpha = 0.7,
                fill = 'black',
                linetype = 0) + 
    geom_point(size = 4, 
               color = 'black',
               fill = 'grey',
               alpha = 0.8,
               pch = 21) +
    labs(x = 'Year',
         y = 'Hearings resulting in grant',
         title = 'Version 6',
         caption = paste0('Smoothed with a local regression', '\n', 
                          'with bands representing standard errors')) +
    theme_sfi()
  
  # version 7
  g7 <- 
    ggplot(data, 
           aes(x = year, 
               y = percent_of_conducted_hearings_resulting_in_a_grant)) +
    geom_bar(stat = 'identity', 
             alpha = 0.8,
             fill = 'black',
             color = 'grey') +
    geom_text(aes(label = paste0(percent_of_conducted_hearings_resulting_in_a_grant, '%')),
              size = 4, 
              color = 'black',
              nudge_y = 2) +
    labs(x = 'Year',
         y = 'Hearings resulting in grant',
         title = 'Version 7') +
    theme_sfi()
  
  out <- list(g1, g2, g3, 
              g4, g5, g6, 
              g7)
  
  return(out)
}

