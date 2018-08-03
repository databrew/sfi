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
  data <- all_data$laquer$f2
  
  # version 1 
  g1 <- ggplot(data, 
               aes(x = year, 
                   y = percent_of_conducted_hearings_resulting_in_a_grant)) +
    geom_line(size = 1, 
              alpha = 0.6) +
    labs(x = 'Year',
         y = 'Rate of parole grant',
         title = 'Version 1') +
    theme_sfi()
  
  # version 2
  g2 <- ggplot(data, 
               aes(x = year, 
                   y = percent_of_conducted_hearings_resulting_in_a_grant)) +
    geom_area(size = 1, 
              alpha = 0.6) +
    labs(x = 'Year',
         y = 'Rate of parole grant',
         title = 'Version 2') +
    theme_sfi()
  
  # version 3 
  g3 <- ggplot(data, 
               aes(x = year, 
                   y = percent_of_conducted_hearings_resulting_in_a_grant)) +
    geom_bar(stat = 'identity', 
             alpha = 0.6) +
    labs(x = 'Year',
         y = 'Rate of parole grant',
         title = 'Version 3') +
    theme_sfi()
  
  # version 4
  g4 <- ggplot(data, 
               aes(x = year, 
                   y = percent_of_conducted_hearings_resulting_in_a_grant/100)) +
    geom_bar(stat = 'identity', 
             alpha = 0.6) +
    labs(x = 'Year',
         y = 'Rate of parole grant',
         title = 'Version 4') +
    geom_text(aes(label = paste0(percent_of_conducted_hearings_resulting_in_a_grant, '%')), 
              vjust = -0.09, alpha = 0.8) +
    theme_sfi() + scale_y_continuous(labels = percent)
  
  # version 5
  g5 <- ggplot(data, 
               aes(x = year, 
                   y = percent_of_conducted_hearings_resulting_in_a_grant/100)) +
    geom_bar(stat = 'identity', 
             alpha = 0.6,
             color = 'black') +
    labs(x = 'Year',
         y = 'Rate of parole grant',
         title = 'Version 5') +
    geom_text(aes(label = paste0(percent_of_conducted_hearings_resulting_in_a_grant, '%')), 
              vjust = -0.09, alpha = 0.8) +
    theme_sfi() + scale_y_continuous(labels = percent)
  
  # version 6
  g6 <- ggplot(data, 
               aes(x = year, 
                   y = percent_of_conducted_hearings_resulting_in_a_grant/100)) +
    geom_bar(stat = 'identity', 
             alpha = 0.6,
             color = 'black') +
    labs(x = 'Year',
         y = 'Rate of parole grant',
         title = 'Version 6') +
    geom_text(aes(label = paste0(percent_of_conducted_hearings_resulting_in_a_grant, '%')), 
              vjust = 2, alpha = 0.8) +
    theme_sfi() + scale_y_continuous(labels = percent)
  
  
  # version 7
  g7 <- ggplot(data, 
               aes(x = year, 
                   y = percent_of_conducted_hearings_resulting_in_a_grant/100)) +
    geom_bar(stat = 'identity', 
             alpha = 1,
             color = 'black') +
    labs(x = 'Year',
         y = 'Rate of parole grant',
         title = 'Version 7') +
    geom_text(aes(label = paste0(percent_of_conducted_hearings_resulting_in_a_grant, '%')), 
              vjust = 2, alpha = 0.8, color = 'white') +
    theme_sfi() + scale_y_continuous(labels = percent)
  
  # version 8
  g8 <- ggplot(data, 
               aes(x = year, 
                   y = percent_of_conducted_hearings_resulting_in_a_grant/100)) +
    geom_bar(stat = 'identity', 
             alpha = 0.7,
             color = 'black') +
    labs(x = 'Year',
         y = 'Rate of parole grant',
         title = 'Version 8') +
    geom_text(aes(label = paste0(percent_of_conducted_hearings_resulting_in_a_grant, '%')), 
              vjust = 2, alpha = 0.8, color = 'white') +
    theme_sfi() + scale_y_continuous(labels = percent)
  
  # version 9
  g9 <- ggplot(data, 
               aes(x = year, 
                   y = percent_of_conducted_hearings_resulting_in_a_grant/100)) +
    geom_bar(stat = 'identity', 
             alpha = 0.7,
             color = 'black') +
    coord_flip() +
    labs(x = 'Year',
         y = 'Rate of parole grant',
         title = 'Version 9') +
    geom_text(aes(label = paste0(percent_of_conducted_hearings_resulting_in_a_grant, '%')), 
              hjust = 1.5, alpha = 0.8, color = 'white') +
    theme_sfi() + scale_y_continuous(labels = percent)
  
  # version 10
  g10 <- ggplot(data, 
                aes(x = year, 
                    y = percent_of_conducted_hearings_resulting_in_a_grant/100)) +
    geom_bar(stat = 'identity', 
             alpha = 0.7,
             color = 'grey') +
    coord_flip() +
    labs(x = 'Year',
         y = 'Rate of parole grant',
         title = 'Version 10') +
    geom_text(aes(label = paste0(percent_of_conducted_hearings_resulting_in_a_grant, '%')), 
              hjust = 1.2, alpha = 0.8, color = 'black') +
    theme_sfi() + scale_y_continuous(labels = percent)
  
  # version 11
  g11 <- ggplot(data, 
                aes(x = year, 
                    y = percent_of_conducted_hearings_resulting_in_a_grant/100)) +
    geom_bar(stat = 'identity', 
             alpha = 0.7,
             fill = 'black',
             color = 'grey') +
    coord_flip() +
    labs(x = 'Year',
         y = 'Rate of parole grant',
         title = 'Version 11') +
    geom_text(aes(label = paste0(percent_of_conducted_hearings_resulting_in_a_grant, '%')), 
              hjust = 1.2, alpha = 0.8, color = 'white') +
    theme_sfi() + scale_y_continuous(labels = percent)
  
  # version 12
  g12 <- ggplot(data, 
                aes(x = year, 
                    y = percent_of_conducted_hearings_resulting_in_a_grant/100)) +
    geom_bar(stat = 'identity', 
             alpha = 0.7,
             fill = 'white',
             color = 'black') +
    coord_flip() +
    labs(x = 'Year',
         y = 'Rate of parole grant',
         title = 'Version 12') +
    geom_text(aes(label = paste0(percent_of_conducted_hearings_resulting_in_a_grant, '%')), 
              hjust = 1.2, alpha = 0.8, color = 'black') +
    theme_sfi() + scale_y_continuous(labels = percent)
  
  out <- list(g1, g2, g3, g4, g5, g6,
              g7, g8, g9, g10, g11, g12)
  
  return(out)
}