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
  data <- all_data$laquer$f1
  
  # version 1 
  g1 <- ggplot(data, 
               aes(x = year, 
                   y = number_of_grants)) +
    geom_line(size = 1, 
              alpha = 0.6) +
    labs(x = 'Year',
         y = 'Hearings resulting in grant',
         title = 'Version 1') +
    theme_sfi()
  
  # version 2 
  g2 <- ggplot(data, 
               aes(x = year, 
                   y = number_of_grants)) +
    geom_point(size = 2, 
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
    geom_smooth(method = 'lm',
                size = 0.5, 
                alpha = 0.6,
                color = 'black',
                linetype = 2) +    
    geom_point(size = 3, 
               alpha = 0.6) +
    labs(x = 'Year',
         y = 'Hearings resulting in grant',
         title = 'Version 4',
         caption = 'Fitted line estimated with a linear regression') +
    theme_sfi()
  
  # version 4
  g5 <- ggplot(data, 
               aes(x = year, 
                   y = number_of_grants)) +
    geom_smooth(method = 'loess',
                size = 0.5, 
                alpha = 0.6,
                color = 'black',
                linetype = 2) + 
    geom_point(size = 3, 
               alpha = 0.6) +
    labs(x = 'Year',
         y = 'Hearings resulting in grant',
         title = 'Version 5',
         caption = 'Fitted line smoothed with a local regression') +
    theme_sfi()
  
  
  # version 6
  g6 <- ggplot(data, 
               aes(x = year, 
                   y = number_of_grants)) +
    geom_bar(stat = 'identity', 
             alpha = 0.8,
             fill = 'white',
             color = 'black') +
    geom_text(aes(label = number_of_grants),
              size = 1.8, 
              color = 'black'
              nudge_y = 25) +
    labs(x = 'Year',
         y = 'Hearings resulting in grant',
         title = 'Version 6') +
    theme_sfi()
  
  
  # version 7
  g7 <- ggplot(data, 
               aes(x = year, 
                   y = number_of_grants)) +
    geom_bar(stat = 'identity', 
             alpha = 0.8,
             fill = 'white',
             color = 'black') +
    geom_text(aes(label = number_of_grants),
              size = 1.8, 
              color = 'black'
              nudge_y = 25) +
    labs(x = 'Year',
         y = 'Hearings resulting in grant',
         title = 'Version 7') +
    theme_sfi()
  
  
  # version 8
  g8 <- ggplot(data, 
               aes(x = year, 
                   y = number_of_grants)) +
    geom_bar(stat = 'identity', alpha = 0.8) +
    geom_text(aes(label = number_of_grants),
              size = 1.5, 
              alpha = 0.6,
              nudge_y = 25) +
    labs(x = 'Year',
         y = 'Hearings resulting in grant',
         title = 'Version 8') +
    theme_sfi()
  
  
  # version 9
  g9 <- ggplot(data, 
               aes(x = year, 
                   y = number_of_grants)) +
    geom_bar(stat = 'identity', alpha = 0.6) +
    geom_smooth(size = 0.5, color = 'black', se = F) +
    labs(x = 'Year',
         y = 'Hearings resulting in grant',
         title = 'Version 9') +
    theme_sfi()
  
  # version 10
  g10 <- ggplot(data, 
                aes(x = year, 
                    y = number_of_grants)) +
    geom_point(alpha = 0.1,
               size = 1)+
    geom_area(stat = 'identity', 
              alpha = 0.9,
              color = 'grey') +
    labs(x = 'Year',
         y = 'Hearings resulting in grant',
         title = 'Version 10') +
    theme_sfi()
  
  
  
  out <- list(g1, g2, g3, 
              g4, g5, g6, 
              g7, g8, g9,
              g10)
  
  return(out)
}