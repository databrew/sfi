#' Alexander 4
#' 
#' Generate a plot for Alexander Figure 4 
#' @export
#' @return a list of plots
#' @import ggplot2
#' @import dplyr

sfi_plot_alexander_4 <- function(){
  
  # Get data
  data <- all_data$alexander$f4
  
  # Flag the groups
  data$group <-
    ifelse(data$key %in% 
             c('Exemption',
               'Color',
               'Race',
               'National origin',
               'Religion',
               'Sex',
               'Retaliation'), 
           'Claim type',
           'National origin')
  
  # Get percentage
  data$value <- data$value * 100
  
  g1 <- 
    ggplot(data = data %>% filter(group == 'Claim type'),
           aes(x = key,
               y = value)) +
    geom_bar(stat = 'identity',
             alpha = 0.8) +
    theme_sfi() +
    labs(x = 'Claim type',
         y = 'Percent') +
    geom_text(aes(label = round(value, digits = 1)),
              nudge_y = 4,
              size = 4,
              alpha = 0.7) +
    theme(axis.text.x = element_text(angle = 90,
                                     hjust = 0.5)) +
    ylim(0, 75)
  g2 <- 
    ggplot(data = data %>% filter(group == 'National origin') %>%
             mutate(key = gsub(' ', '\n', key)),
           aes(x = key,
               y = value)) +
    geom_bar(stat = 'identity',
             alpha = 0.8) +
    theme_sfi() +
    labs(x = 'National origin',
         y = 'Percent') +
    geom_text(aes(label = round(value, digits = 1)),
              nudge_y = 4,
              size = 4,
              alpha = 0.7) +
    theme(axis.text.x = element_text(angle = 90,
                                     hjust = 0.5)) +
    ylim(0, 75)
  
  return(list(Rmisc::multiplot(g1, g2, cols = 2)))
  }