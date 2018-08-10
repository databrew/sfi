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
               rows = 10,
               size  = 1,
               flip = FALSE)
  
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
  
  # no size
  g5 <-
    waffle(parts = values,
               title = 'Version 3 (Claim type)',
               colors = colors, 
               rows = 10,
               size  = 0,
               flip = FALSE)
  
  # no size
  g6 <-
    waffle(parts = values_2,
               title = 'Version 3 (National origin)',
               colors = colors_2, 
               rows = 10,
               size  = 0,
               flip = FALSE)
  

  
  # Floating bubbles diagnol for claim type
  g7 <-
  ggplot(data = data %>% filter(group == 'Claim type') %>%
                 arrange(value), 
               aes(x = c(1, 2, 3, 4, 5, 6, 7),
                   y = c(1, 2, 3, 4, 5, 6, 7))) +
    xlim(c(0, 8)) + ylim(c(0,8)) +
    geom_point(aes(size = value), 
               alpha = 0.9,
               color ='grey') + 
    geom_text(aes(label = key), 
              size = 3,
              color = '#535353')+
    labs(x = '',
         y = '',
         title = 'Version 4 (Claim type)') + 
    scale_size_continuous(name = '',
                          breaks = c(0, 10, 20, 30),
                          labels = c('0%','10%', '20%', '30%'),
                          range = c(6,17)) +    
    theme(
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank()) + 
    theme_sfi()
  
  
  # Floating bubbles diagnol for national origin
  
  gd <- data %>% filter(group == 'National origin') %>%
    arrange(value) %>%
    mutate(nudgey = seq(0.5, 2, length = 6)) %>%
    mutate(x = seq(0, 6, length = 6),
           y = c(1, 2, 3, 4, 5, 6))
  g8 <-ggplot(data = gd, 
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
         title = 'Version 4 (National origin)') + 
    scale_size_area(name = '',
                          max_size = 40) +
    theme(legend.position = 'none',
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank()) + 
    theme_sfi(gM = FALSE,
              lp = 'none',
              gm = FALSE)
  

    # FOR THE REST OF THEM IM NOT DOING TWO PLOTS FOR EACH ONE, JUST STARTED MESSING ARONUD 
    # WITH GRIDS AND BUBBLES AND DIFFERENT SHAPES
    gd2 <- data %>% 
      filter(group == 'Claim type') %>%
      arrange(value) %>%
      mutate(x = c(seq(1, 7, length = 4), 2, 6, 4),
             y = c(1, 1, 1, 1, 4, 4, 6))
        g9 <-
    ggplot(data = gd2, 
           aes(x = x, y = y)) +
      xlim(c(0, 8)) + ylim(c(0, 8)) +
      geom_point(aes(size = value), 
                 alpha = 0.4,
                 color ='black',
                 stroke = 1) + 
      geom_text(aes(label = paste0(key)),
                alpha = 0.7,
                size = 3,
                color = 'black',
                vjust = -4) +
      geom_text(aes(label = paste0(value,'%')),
                alpha = 0.7,
                size = 3,
                color = 'black')+
      labs(x = '',
           y = '',
           title = 'Version 5 (Claim type)') + 
      scale_size_area(name = '',
                            max_size = 50) +
      theme(legend.position = 'none',
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank()) + 
      theme_sfi(gM = FALSE,lp = 'none',
                gm = FALSE)
    
  
    # bubble grid for n
    gd3 <- data %>% filter(group == 'National origin') %>%
      arrange(value) %>%
      mutate(x = c(1, 4, 7, 5.5, 2.5, 4),
             y = c(2, 2, 2, 4, 4, 6))
    g10 <-
    ggplot(data = gd3, 
           aes(x = x, y = y)) +
      xlim(c(0, 8)) + ylim(c(0,8)) +
      geom_point(aes(size = value), 
                 alpha = 0.4,
                 color ='black', 
                 stroke = 1,
                 shape = 15) + 
      geom_text(aes(label = paste0(key, ' ', '\n', value,'%')),
                alpha = 0.7,
                size = 3,
                color = 'black') +
      labs(x = '',
           y = '',
           title = 'Version 5 (National origin)') +
      scale_size_area(name = '',
                      max_size = 50) +
      theme(legend.position = 'none',
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks = element_blank()) + 
      theme_sfi(gM = FALSE,
                lp = 'none',
                gm = FALSE)
    
    g11 <-
    ggplot(data = data %>% 
             filter(group == 'Claim type') %>%
             arrange(value), 
           aes(x = c(1, 4, 7, 2, 4, 6, 4),
               y = c(1, 1, 1, 4, 4, 4, 7))) +
      xlim(c(0, 8)) + ylim(c(0, 8)) +
      geom_point(aes(size = value), 
                 alpha = 0.4,
                 color ='black',
                 stroke = 4,
                 shape = 15) + 
      geom_text(aes(label = paste0(key)),
                alpha = 0.7,
                size = 3,
                color = 'black',
                vjust = -4) +
      geom_text(aes(label = paste0(value,'%')),
                alpha = 0.7,
                size = 3,
                color = 'black')+
      labs(x = '',
           y = '',
           title = 'Version 6 (Claim type)') + 
      scale_size_area(name = '',
                      max_size = 35) +
      theme(legend.position = 'none',
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks = element_blank()) + 
      theme_sfi(gM = FALSE,lp = 'none',
                gm = FALSE)
  
    
    g12 <-
    ggplot(data = data %>% 
             filter(group == 'Claim type') %>%
             arrange(value), 
           aes(x = c(1, 4, 7, 1, 4, 7, 4),
               y = c(1, 1, 1, 4, 4, 4, 7))) +
      xlim(c(0, 8)) + ylim(c(0, 8)) +
      geom_point(aes(size = value), 
                 alpha = 0.4,
                 color ='black',
                 stroke = 4,
                 shape = 16) + 
      geom_text(aes(label = paste0(key)),
                alpha = 0.7,
                size = 3,
                color = 'black',
                vjust = -4) +
      geom_text(aes(label = paste0(value,'%')),
                alpha = 0.7,
                size = 3,
                color = 'black')+
      labs(x = '',
           y = '',
           title = 'Version 7 (Claim type)') + 
      scale_size_area(name = '',
                      max_size = 35) +
      theme(legend.position = 'none',
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks = element_blank()) + 
      theme_sfi(gM = FALSE,lp = 'none',
                gm = FALSE)
    
    
  # g13 THE REST ARE THE SIDE BAR CHARTS THAT THEY KINDA LIKED
    g13 <-
  ggplot(data = data %>% filter(group == 'Claim type'),
           aes(reorder(x = key, value),
               y = value)) +
    geom_bar(stat = 'identity',
             alpha = 0.8) +
    coord_flip() +
    theme_sfi() +
    labs(x = 'Claim type',
         y = 'Percent',
         title = 'Version 8 (Claim type)') +
    geom_text(aes(label = paste0(round(value, digits = 1), '%')),
              nudge_y = 3,
              size = 4,
              alpha = 0.7) +
    theme(axis.text.x = element_text(hjust = 0.5)) +
    ylim(0, 55)

  #g14 with percentage labels
 g14 <-
   ggplot(data = data %>% filter(group == 'National origin'),
         aes(reorder(x = key, value),
             y = value)) +
    geom_bar(stat = 'identity',
             alpha = 0.8) +
    coord_flip() +
    theme_sfi() +
    labs(x = 'Claim type',
         y = 'Percent',
         title = 'Version 9 (National origin)') +
    geom_text(aes(label = paste0(round(value, digits = 1), '%')),
              nudge_y = 5.5,
              size = 4,
              alpha = 0.7) +
    theme(axis.text.x = element_text(hjust = 0.5)) +
    ylim(0, 100)

  # with percentage labels without borders
  g15 <-
    ggplot(data = data %>% filter(group == 'Claim type'),
           aes(reorder(x = key, value),
               y = value)) +
    geom_bar(stat = 'identity',
             alpha = 0.8) +
    coord_flip() +
    theme_sfi() +
    labs(x = 'Claim type',
         y = 'Percent',
         title = 'Version 10 Claim type') +
    geom_text(aes(label = paste0(round(value, digits = 1), '%')),
              nudge_y = 3,
              size = 4,
              alpha = 0.7) +
    theme(axis.text.x = element_text(hjust = 0.5)) +
    ylim(0, 55)

  #with percentage labels without borders
 g16<-
   ggplot(data = data %>% filter(group == 'National origin'),
         aes(reorder(x = key, value),
             y = value)) +
    geom_bar(stat = 'identity',
             alpha = 0.8) +
    coord_flip() +
    theme_sfi() +
    labs(x = 'Claim type',
         y = 'Percent',
         title = 'Version 10 (National origin') +
    geom_text(aes(label = paste0(round(value, digits = 1), '%')),
              nudge_y = 5.5,
              size = 4,
              alpha = 0.7) +
    theme(axis.text.x = element_text(hjust = 0.5)) +
    ylim(0, 100)

 d17 <- data %>% filter(group == 'National origin')
 g17 <- ggplot(data = d17,
               aes(reorder(x = key, value),
                   y = value)) +
   geom_bar(stat = 'identity',
            alpha = 0.8) +
   geom_text(aes(label = paste0(gsub(' ', ' ', key), '\n', round(value, digits = 1), '%')),
             size = 2) +
   theme_sfi() +
   coord_polar(theta = "y") +
   ylim(0, 100) +
   # ylim(0, 2 * max(d17$value)) +
   # scale_fill_manual(name = '',
   #                   values = make_colors(n = length(unique(d17$key)),
   #                                        bw = TRUE,
   #                                        categorical = FALSE)) +
   theme(axis.text = element_blank(),
         # axis.title = element_blank(),
         axis.ticks = element_blank(),
         legend.position = 'none') +
   labs(x = '',
        y = '',
        title = 'Version 17')
   
 g18 <- ggplot(data = d17,
               aes(reorder(x = key, value),
                   y = value)) +
   geom_bar(stat = 'identity',
            alpha = 0.8) +
   geom_bar(stat = 'identity',
            alpha = 0.1,
            aes(y = max(value))) +
   geom_text(aes(label = paste0(gsub(' ', ' ', key)),
                 y = 0),
             size = 2,
             angle = 45,
             hjust = 1) +
   theme_sfi() +
   coord_polar(theta = "y", start = 4.72) +
   # ylim(0, ) +
   ylim(0, 2 * max(d17$value)) +
   # scale_fill_manual(name = '',
   #                   values = make_colors(n = length(unique(d17$key)),
   #                                        bw = TRUE,
   #                                        categorical = FALSE)) +
   theme(axis.text = element_blank(),
         # axis.title = element_blank(),
         axis.ticks = element_blank(),
         legend.position = 'none') +
   labs(x = '',
        y = '',
        title = 'Version 18') +
   geom_text(aes(y = max(value),
                  label = paste0(round(value, digits = 1), '')),
             angle = 0,
             alpha = 0.6)
 
  return(list(g1, g2, g3, g4, g5, g6, g7, g8, g9, g10,
              g11,g12,g13,g14,g15,g16, g17, g18))
}
