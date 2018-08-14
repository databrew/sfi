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
    geom_point(size = 2, 
               alpha = 0.9,
               color = 'black',
               stroke = 1.5,
               pch = 1) +
    geom_line(size = 1,
              color = '#4D4D4D') +
    labs(x = 'Year',
         y = 'Hearings resulting in grant',
         title = 'Version 1') +
    theme_sfi()
  
  # version 2
  g2 <- 
    ggplot(data, 
           aes(x = year, 
               y = number_of_grants)) +
    geom_smooth(method = 'loess',
                alpha = 0.7,
                color = 'black',
                linetype = 0) + 
    geom_point(size = 2, 
               color = 'black',
               fill = 'black',
               alpha = 0.8,
               pch = 21) +
    labs(x = 'Year',
         y = 'Hearings resulting in grant',
         title = 'Version 2',
         caption = paste0('Fitted line smoothed with a local regression', '\n', 
                          'with bands representing standard errors')) +
    theme_sfi()
  
  # version 3
  g3 <- 
    ggplot(data, 
           aes(x = year, 
               y = number_of_grants)) +
    geom_smooth(method = 'loess',
                alpha = 0.5,
                color = 'black',
                linetype = 0) + 
    geom_point(size = 3, 
               color = 'black',
               fill = 'black',
               alpha = 0.8,
               pch = 21) +
    labs(x = 'Year',
         y = 'Hearings resulting in grant',
         title = 'Version 3',
         caption = paste0('Fitted line smoothed with a local regression', '\n', 
                          'with bands representing standard errors')) +
    theme_sfi()
  
  
  # version 4
  g4 <- 
    ggplot(data, 
           aes(x = year, 
               y = number_of_grants)) +
    geom_smooth(method = 'loess',
                alpha = 0.7,
                color = 'black',
                linetype = 0) + 
    geom_point(size = 4, 
               color = 'grey',
               fill = 'black',
               alpha = 0.8,
               pch = 21) +
    labs(x = 'Year',
         y = 'Hearings resulting in grant',
         title = 'Version 4',
         caption = paste0('Fitted line smoothed with a local regression', '\n', 
                          'with bands representing standard errors')) +
    theme_sfi()
  
  
  
  # version 5
  g5 <- 
    ggplot(data, 
           aes(x = year, 
               y = number_of_grants)) +
    geom_smooth(method = 'loess',
                fill = 'black',
                alpha = 0.6,
                linetype = 0,
                fullrange = TRUE) + 
    geom_point(size = 3, 
               color = 'black',
               fill = '#B2B2B2',
               alpha = 0.8,
               pch = 21) +
    labs(x = 'Year',
         y = 'Hearings resulting in grant',
         title = 'Version 5',
         caption = paste0('Fitted line smoothed with a local regression', '\n', 
                          'with bands representing standard errors')) +
    theme_sfi(bc = 'white')
  
  
  # version 6
  g6 <- 
    ggplot(data, 
           aes(x = year, 
               y = number_of_grants)) +
    geom_smooth(method = 'loess',
                alpha = 0.6,
                fill = 'black',
                linetype = 0) + 
    geom_point(size = 3, 
               colour = 'white',
               alpha = 1,
               pch = 1) +
    labs(x = 'Year',
         y = 'Hearings resulting in grant',
         title = 'Version 6',
         caption = 'Fitted line smoothed with a local regression') +
    theme_sfi(bc = 'grey')
  
  
  # version 7
  g7 <- 
    ggplot(data, 
           aes(x = year, 
               y = number_of_grants)) +
    geom_bar(stat = 'identity', 
             alpha = 0.8,
             fill = 'black',
             color = 'grey') +
    geom_text(aes(label = number_of_grants),
              size = 1.8, 
              color = 'black',
              nudge_y = 25) +
    labs(x = 'Year',
         y = 'Hearings resulting in grant',
         title = 'Version 7') +
    theme_sfi()
  
  
  out <- list(g1, g2, g3, 
              g4, g5, g6, 
              g7)
  
  return(out)
}