#' Laqueur 2
#' 
#' Generate a plot for Laqueur Figure 2
#' @export
#' @return a list of plots
#' @import ggplot2
#' @import dplyr
#' @import knitr
#' @import kableExtra

sfi_plot_laqueur_2 <- function(){
  
  # get data 
  data <- all_data$laquer$f2
  
  # version 1 
  g1 <- ggplot(data, 
               aes(x = year, 
                   y = number_of_grants)) +
    geom_line(size = 1, 
              alpha = 0.6) +
    labs(x = 'Year',
         y = 'Number of hearings resulting in grant',
         title = 'Version 1') +
    theme_sfi()
  
  # version 2 
  g2 <- ggplot(data, 
               aes(x = year, 
                   y = number_of_grants)) +
    geom_point(size = 3, 
               alpha = 0.6) +
    labs(x = 'Year',
         y = 'Hearings resulting in grant',
         title = 'Version 2') +
    theme_sfi()
  
  # version 3 
  g3 <- ggplot(data, 
               aes(x = year, 
                   y = number_of_grants)) +
    geom_point(size = 3, 
               alpha = 0.6) +
    geom_line(size = 1, 
              alpha = 0.6) +    
    labs(x = 'Year',
         y = 'Hearings resulting in grant',
         title = 'Version 3') +
    theme_sfi()
  
  # version 4
  g4 <- ggplot(data, 
               aes(x = year, 
                   y = number_of_grants)) +
    geom_point(size = 3, 
               alpha = 0.6) +
    geom_smooth(size = 0.5, 
                alpha = 0.6,
                color = 'black') +    
    labs(x = 'Year',
         y = 'Hearings resulting in grant',
         title = 'Version 4') +
    theme_sfi()
  
  # version 5
  g5 <- ggplot(data, 
               aes(x = year, 
                   y = number_of_grants)) +
    geom_bar(stat = 'identity', alpha = 0.8) +
    labs(x = 'Year',
         y = 'Hearings resulting in grant',
         title = 'Version 5') +
    theme_sfi()
  
  # version 6
  g6 <- ggplot(data, 
               aes(x = year, 
                   y = number_of_grants)) +
    geom_bar(stat = 'identity', alpha = 0.6) +
    geom_smooth(size = 0.5, color = 'black', se = F) +
    labs(x = 'Year',
         y = 'Hearings resulting in grant',
         title = 'Version 6') +
    theme_sfi()
  
  
  out <- list(g1, g2, g3, 
              g4, g5, g6)
  
  return(out)
}