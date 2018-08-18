#' Alexander 4
#' 
#' Generate a plot for Alexander Figure 4 
#' @export
#' @return a list of plots
#' @import ggplot2
#' @import dplyr
#' @import waffle 
#' @import plotrix

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
  colors <- make_colors(length(values), bw = TRUE)
  
  # do the same for the national origin filter
  values_2 <- data$value[data$group == 'National origin']
  names(values_2) <- paste0(data$key[data$group == 'National origin'], ' (',values_2,'%)')
  values_2 <- sort(values_2, decreasing = TRUE)
  colors_2 <- make_colors(length(values_2), bw = TRUE)
  
  # plot - Each plot has two version 1: the first for Claim type the second for national origin
  g1 <-
    waffle(parts = values, 
               title = 'Version 1 (Claim type)',
               colors = colors, 
               rows = 3,
               size  = 1,
               flip = FALSE) +
    theme_sfi(lp = c(0.5,-1))
  
  g1
  
  
  g2 <-
    waffle(parts = values_2, 
               title = 'Version 1 (National origin)',
               colors = colors_2, 
               rows = 10,
               size  = 1,
               flip = FALSE)
  
  # small size
  g3 <-
    waffle(parts = values,
               title = 'Version 2 (Claim type)',
               colors = colors, 
               rows = 10,
               size  = 0.1,
               flip = FALSE)
  
  # small size
  g4 <-
    
    waffle(parts = values_2,
               title = 'Version 2 (National origin)',
               colors = colors_2, 
               rows = 10,
               size  = 0.1,
               flip = FALSE)
  
  
  
  # Floating bubbles diagnol for national origin
  
  gd <- data %>% filter(group == 'National origin') %>%
    arrange(value) %>%
    mutate(nudgey = seq(0.5, 2, length = 6)) %>%
    mutate(x = seq(0, 6, length = 6),
           y = c(1, 2, 3, 4, 5, 6))
  g5 <-ggplot(data = gd, 
               aes(x = x, y = y)) +
    xlim(c(-1,7)) + ylim(c(0,8)) +
    geom_point(aes(size = value), 
               alpha = 0.4,
               color ='black') + 
    geom_text(aes(y = y + nudgey,
                  label = paste0(key, ' ', '\n', value,'%')),
              alpha = 0.7,
              size = 3,
              color = 'black')+
    labs(x = '',
         y = '',
         title = 'Version 3 (National origin)') + 
    scale_size_area(name = '',
                          max_size = 40) +
    theme(legend.position = 'none',
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank()) + 
    theme_sfi(gM = FALSE,
              lp = 'none',
              gm = FALSE)
  

    
  # g13 THE REST ARE THE SIDE BAR CHARTS THAT THEY KINDA LIKED
    g6 <-
  ggplot(data = data %>% filter(group == 'Claim type'),
           aes(reorder(x = key, value),
               y = value)) +
    geom_bar(stat = 'identity',
             alpha = 0.8) +
    coord_flip() +
    theme_sfi() +
    labs(x = 'Claim type',
         y = 'Percent',
         title = 'Version 4 (Claim type)') +
    geom_text(aes(label = paste0(round(value, digits = 1), '%')),
              nudge_y = 3,
              size = 4,
              alpha = 0.7) +
    theme(axis.text.x = element_text(hjust = 0.5)) +
    ylim(0, 55)

  #g14 with percentage labels
 g7 <-
   ggplot(data = data %>% filter(group == 'National origin'),
         aes(reorder(x = key, value),
             y = value)) +
    geom_bar(stat = 'identity',
             alpha = 0.8) +
    coord_flip() +
    theme_sfi() +
    labs(x = 'Claim type',
         y = 'Percent',
         title = 'Version 4 (National origin)') +
    geom_text(aes(label = paste0(round(value, digits = 1), '%')),
              nudge_y = 5.5,
              size = 4,
              alpha = 0.7) +
    theme(axis.text.x = element_text(hjust = 0.5)) +
    ylim(0, 100)

 
 
  return(list(g1, g2, g3, g4, g5, g6, g7))
}
