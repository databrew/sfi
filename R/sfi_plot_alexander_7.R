#' Alexander 7
#' 
#' Generate a plot for Alexander Figure 7
#' @export
#' @return a list of plots
#' @import ggplot2
#' @import dplyr
#' @import waffle

# sfi_plot_alexander_7 <- function(){
#   
  # # Get data
  data <- data$alexander$f7
  data$value <- data$value * 100
  
  # ---------------------------------------------
  # waffle chart
  
  # create names vectors for values and colors 
  values <- data$value
  names(values) <- paste0(data$key, ' (',values,'%)')
  values <- sort(values, decreasing = TRUE)
  colors <- make_colors(length(values), bw = TRUE)
  
  # plot - Each plot has two version 1: the first for Claim type the second for national origin
  g1 <- 
  waffle(parts = values, 
         title = 'Version 1',
         colors = colors, 
         rows = 10,
         size  = 1,
         flip = FALSE)
  
  # small size
  g2 <- 
  waffle(parts = values,
         title = 'Version 2',
         colors = colors, 
         rows = 10,
         size  = 0.1,
         flip = FALSE)
  
  
  # Floating bubbles diagnol for claim type
  g3 <- 
  ggplot(data = data %>% 
           arrange(value), 
         aes(x = c(1, 2, 3, 4, 5),
             y = c(1, 2, 3, 4, 5))) +
    xlim(c(0, 7)) + ylim(c(0,7)) +
    geom_point(aes(size = value), 
               alpha = 0.9,
               color ='grey') + 
    geom_text(aes(label = key), 
              color = '#535353')+
    scale_size_area(name = '',
                    max_size = 20)+ 
    labs(x = '',
         y = '',
         title = 'Version 3') + 
    theme(
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank()) + 
    theme_sfi()
  
  
  # Floating bubbles diagnol for national origin
  g4 <- 
  ggplot(data = data %>%
           arrange(value), 
         aes(x = c(1, 2, 3, 4, 5),
             y = c(1, 2, 3, 4, 5))) +
    xlim(c(0, 6)) + ylim(c(0,8)) +
    geom_point(aes(size = value), 
               alpha = 0.4,
               color ='black') + 
    geom_text(aes(label = paste0(key, ' ', '\n', value,'%')),
              alpha = 0.7,
              size = 3,
              color = 'black',
              vjust = -1)+
    labs(x = '',
         y = '',
         title = 'Version 4') + 
    scale_size_area(name = '',
                    max_size = 15) +
    theme(legend.position = 'none',
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank()) + 
    theme_sfi(gM = FALSE,
              lp = 'none',
              gm = FALSE)
  
  
  # FOR THE REST OF THEM IM NOT DOING TWO PLOTS FOR EACH ONE, JUST STARTED MESSING ARONUD 
  # WITH GRIDS AND BUBBLES AND DIFFERENT SHAPES
  g5 <- 
  ggplot(data = data %>% 
           arrange(value), 
         aes(x = c(5, 3, 1, 4, 2),
             y = c(1, 1, 1, 4, 4))) +
    xlim(c(0, 6)) + ylim(c(0, 8)) +
    geom_point(aes(size = value), 
               alpha = 0.4,
               color ='black',
               stroke = 1) + 
    geom_text(aes(label = paste0(key)),
              alpha = 0.7,
              size = 3,
              color = 'black',
              vjust = -2) +
    geom_text(aes(label = paste0(value,'%')),
              alpha = 0.7,
              size = 3,
              color = 'black')+
    labs(x = '',
         y = '',
         title = 'Version 5') + 
    scale_size_area(name = '',
                    max_size = 30) +
    theme(legend.position = 'none',
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank()) + 
    theme_sfi(gM = FALSE, lp = 'none',
              gm = FALSE)
  
  
  # g13 THE REST ARE THE SIDE BAR CHARTS THAT THEY KINDA LIKED
  g6 <- 
  ggplot(data = data,
         aes(reorder(x = key, value),y = value)) +
    geom_bar(stat = 'identity',
             alpha = 0.8,
             color = 'darkgrey') +
    coord_flip() +
    theme_sfi() +
    labs(x = 'Judges actions',
         y = 'Percent',
         title = 'Version 6') +
    geom_text(aes(label = paste0(round(value, digits = 1), '%')),
              nudge_y = 5,
              size = 4,
              alpha = 0.7) +
    theme(axis.text.x = element_text(hjust = 0.5)) +
    ylim(0, 100)
  

  # need to find plots that represent simple pie chart data

  # pie chart

  # side bar chart

  return(list(g1, g2, g3, g4, g5, g6))
  
# }
  
  