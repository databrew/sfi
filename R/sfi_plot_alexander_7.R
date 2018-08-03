#' Alexander 7
#' 
#' Generate a plot for Alexander Figure 7
#' @export
#' @return a list of plots
#' @import ggplot2
#' @import dplyr

sfi_plot_alexander_7 <- function(){
  
  # Get data
  data <- all_data$alexander$f7
  data$value <- data$value * 100
  
  g1 <- 
    ggplot(data = data,
         aes(x = key,
             y = value)) +
    geom_bar(stat = 'identity',
             alpha = 0.8) +
    theme_sfi() +
    labs(y = 'Percent',
         x = 'Judges\' Actions',
         title = 'Version 1') +
    geom_text(aes(label = round(value, digits = 1)),
              nudge_y = 4,
              size = 4,
              alpha = 0.7)
  
  g2 <- 
    ggplot(data = data,
           aes(x = key,
               y = value)) +
    geom_bar(stat = 'identity',
             alpha = 0.8) +
    theme_sfi() +
    labs(y = 'Percent',
         x = 'Judges\' Actions',
         title = 'Version 2') +
    geom_text(aes(label = round(value, digits = 1)),
              nudge_y = 3,
              size = 4,
              alpha = 0.7) +
    coord_flip()
  
  g3 <- 
    ggplot(data = data,
           aes(x = key,
               y = value)) +
    geom_bar(stat = 'identity',
             alpha = 0.8) +
    theme_sfi() +
    labs(y = 'Percent',
         x = 'Judges\' Actions',
         title = 'Version 3') +
    geom_label(aes(label = paste0(round(value), '')), 
              nudge_y = 5, alpha = 0.8) +
    coord_flip()
  
  data4 <- data %>%
    arrange(value)
  data4$key <- factor(data4$key, levels = data4$key)
  g4 <- 
    ggplot(data = data4,
           aes(x = key,
               y = value)) +
    geom_bar(stat = 'identity',
             alpha = 0.8) +
    theme_sfi() +
    labs(y = 'Percent',
         x = 'Judges\' Actions',
         title = 'Version 4') +
    geom_label(aes(label = paste0(round(value), '')), 
               nudge_y = 3, alpha = 0.8) +
    coord_flip()
  
  g5 <- 
    ggplot(data = data4,
           aes(x = key,
               y = value)) +
    geom_point() +
    geom_line(aes(group = 1), alpha = 0.8, lty = 2) +
    theme_sfi() +
    labs(y = 'Percent',
         x = 'Judges\' Actions',
         title = 'Version 5') +
    geom_label(aes(label = paste0(round(value), '')), 
               nudge_y = 8, alpha = 0.8) 
  
  return(list(g1, g2, g3, g4, g5))
}