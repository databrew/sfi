#' Alexander 4
#' 
#' Generate a plot for Alexander Figure 4 
#' @export
#' @return a list of plots
#' @import ggplot2
#' @import dplyr
#' @import waffle 


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

  # ---------------------------------------------
  # waffle chart
  
  # create names vectors for values and colors 
  values <- data$value[data$group == 'Claim type']
  names(values) <- paste0(data$key[data$group == 'Claim type'], ' (',values,'%)')
  values <- sort(values, decreasing = TRUE)
  
  values_2 <- data$value[data$group == 'National origin']
  names(values_2) <- paste0(data$key[data$group == 'National origin'], ' (',values_2,'%)')
  values_2 <- sort(values_2, decreasing = TRUE)
  
  colors_2 <- make_colors(length(values_2), bw = TRUE)
  
  # plot with 10 rows
  g1 <- waffle(parts = values, 
               title = 'Version 1 (Claim type)',
               colors = colors, 
               rows = 10,
               size  = 1,
               flip = FALSE)
  
  g2 <- waffle(parts = values_2, 
               title = 'Version 1 (National origin)',
               colors = colors_2, 
               rows = 10,
               size  = 1,
               flip = FALSE)
  
  # small size
  g3 <- waffle(parts = values,
               title = 'Version 2 (Claim type)',
               colors = colors, 
               rows = 10,
               size  = 0.1,
               flip = FALSE)
  
  # small size
  g4 <- waffle(parts = values_2,
               title = 'Version 2 (National origin)',
               colors = colors_2, 
               rows = 10,
               size  = 0.1,
               flip = FALSE)
  
  # no size
  g5 <- waffle(parts = values,
               title = 'Version 3 (Claim type)',
               colors = colors, 
               rows = 10,
               size  = 0,
               flip = FALSE)
  
  # no size
  g6 <- waffle(parts = values_2,
               title = 'Version 3 (National origin)',
               colors = colors_2, 
               rows = 10,
               size  = 0,
               flip = FALSE)
  

  
  # Floating bubbles
  
  # sliced (size) pie charts
  
  # verticle lines circle
  
    
  # # with percentage labels
  # ggplot(data = data %>% filter(group == 'Claim type'),
  #          aes(reorder(x = key, value),
  #              y = value)) +
  #   geom_bar(stat = 'identity',
  #            alpha = 0.8,
  #            color = 'darkgrey') +
  #   coord_flip() +
  #   theme_sfi() +
  #   labs(x = 'Claim type',
  #        y = 'Percent',
  #        title = 'Version 1') +
  #   geom_text(aes(label = paste0(round(value, digits = 1), '%')),
  #             nudge_y = 3,
  #             size = 4,
  #             alpha = 0.7) +
  #   theme(axis.text.x = element_text(hjust = 0.5)) +
  #   ylim(0, 55)
  # 
  # # with percentage labels
  # ggplot(data = data %>% filter(group == 'National origin'),
  #        aes(reorder(x = key, value),
  #            y = value)) +
  #   geom_bar(stat = 'identity',
  #            alpha = 0.8,
  #            color = 'darkgrey') +
  #   coord_flip() +
  #   theme_sfi() +
  #   labs(x = 'Claim type',
  #        y = 'Percent',
  #        title = 'Version 1') +
  #   geom_text(aes(label = paste0(round(value, digits = 1), '%')),
  #             nudge_y = 5.5,
  #             size = 4,
  #             alpha = 0.7) +
  #   theme(axis.text.x = element_text(hjust = 0.5)) +
  #   ylim(0, 100)
  # 
  # # with percentage labels without borders
  # ggplot(data = data %>% filter(group == 'Claim type'),
  #          aes(reorder(x = key, value),
  #              y = value)) +
  #   geom_bar(stat = 'identity',
  #            alpha = 0.8) +
  #   coord_flip() +
  #   theme_sfi() +
  #   labs(x = 'Claim type',
  #        y = 'Percent',
  #        title = 'Version 1') +
  #   geom_text(aes(label = paste0(round(value, digits = 1), '%')),
  #             nudge_y = 3,
  #             size = 4,
  #             alpha = 0.7) +
  #   theme(axis.text.x = element_text(hjust = 0.5)) +
  #   ylim(0, 55)
  # 
  # # with percentage labels without borders
  # ggplot(data = data %>% filter(group == 'National origin'),
  #        aes(reorder(x = key, value),
  #            y = value)) +
  #   geom_bar(stat = 'identity',
  #            alpha = 0.8) +
  #   coord_flip() +
  #   theme_sfi() +
  #   labs(x = 'Claim type',
  #        y = 'Percent',
  #        title = 'Version 1') +
  #   geom_text(aes(label = paste0(round(value, digits = 1), '%')),
  #             nudge_y = 5.5,
  #             size = 4,
  #             alpha = 0.7) +
  #   theme(axis.text.x = element_text(hjust = 0.5)) +
  #   ylim(0, 100)
  # 
 
 
  
  return(list(g1, g2, g3, g4, g5, g6))
  }