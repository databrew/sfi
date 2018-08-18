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
  values_with_per <- values
  names(values_with_per) <- paste0(data$key[data$group == 'Claim type'], ' (',values,'%)')
  names(values) <- data$key[data$group == 'Claim type']
  values_with_per <- sort(values_with_per, decreasing = TRUE)
  values <- sort(values, decreasing = TRUE)
  colors <- make_colors(length(values), bw = TRUE)
  
  # create names vectors for values and colors 
  values_2 <- data$value[data$group == 'National origin']
  values_2_with_per <- values_2
  names(values_2_with_per) <- paste0(data$key[data$group == 'National origin'], ' (',values_2,'%)')
  names(values_2) <- data$key[data$group == 'National origin']
  values_2_with_per <- sort(values_2_with_per, decreasing = TRUE)
  values_2 <- sort(values_2, decreasing = TRUE)
  colors_2 <- make_colors(length(values_2), bw = TRUE)
  
  
  # plot - Each plot has two version 1: the first for Claim type the second for national origin
  g1 <-
    waffle(parts = values_with_per, 
           title = 'Version 1 (Claim type)',
           colors = colors, 
           rows = 10,
           size  = 0.1,
           flip = FALSE) + 
    theme(legend.key.height = 
            unit(1, 'mm')) +
    annotate(geom = 'text',
             label = 'Retaliation',
             cex = 7,
             color = 'white',
             x = 2.5,
             y = 4) +
    annotate(geom = 'text',
             label = 'Sex',
             cex = 7,
             color = 'white',
             x = 5.5,
             y = 5) +
    annotate(geom = 'text',
             label = 'Race',
             cex = 7,
             color = 'white',
             x = 8,
             y = 6) +
    annotate(geom = 'text',
             label = 'Exemption',
             angle = 90,
             cex = 5,
             color = 'white',
             x = 10,
             y = 2) +
    annotate(geom = 'text',
             label = 'Religion',
             angle = 90,
             cex = 4,
             color = 'white',
             x = 10,
             y = 9.3) +
    annotate(geom = 'text',
             label = paste0('National', ' origin'),
             angle = 90,
             cex = 4.5,
             color = 'white',
             x = 10,
             y = 6) 
  
    # plot - Each plot has two version 1: the first for Claim type the second for national origin
  g2 <-
    waffle(parts = values, 
               title = 'Version 1 (Claim type)',
               colors = colors, 
               rows = 8,
               size  = 0.1,
               flip = FALSE) + 
  theme(legend.key.height = 
          unit(1, 'mm')) +
    annotate(geom = 'text',
             label = '38%',
             cex = 10,
             color = 'white',
             x = 2.5,
             y = 4) +
    annotate(geom = 'text',
             label = '26%',
             cex = 10,
             color = 'white',
             x = 7,
             y = 5) +
    annotate(geom = 'text',
             label = '24%',
             cex = 10,
             color = 'white',
             x = 10,
             y = 5) +
    annotate(geom = 'text',
             label = '5%',
             cex = 8,
             color = 'white',
             x = 12,
             y = 3) +
    annotate(geom = 'text',
             label = '5%',
             cex = 8,
             color = 'white',
             x = 12,
             y = 7) +
    annotate(geom = 'text',
             label = '2%',
             cex = 8,
             color = 'white',
             x = 13,
             y = 4) 
    
  
  # plot - Each plot has two version 1: the first for Claim type the second for national origin
  g3 <-
    waffle(parts = values_2_with_per, 
           title = 'Version 1 (National origin)',
           colors = colors_2, 
           rows = 10,
           size  = 0.1,
           flip = FALSE) + 
    theme(legend.key.height = unit(1, 'mm')) +
    annotate(geom = 'text',
             label = 'African American',
             cex = 7,
             color = 'white',
             x = 4,
             y = 5) +
    annotate(geom = 'text',
             label = 'Hispanic/Latino',
             angle = 90,
             cex = 7,
             color = 'white',
             x = 8,
             y = 5) +
    annotate(geom = 'text',
             label = 'Asian American',
             angle = 90,
             cex = 3,
             color = 'white',
             x = 9,
             y = 9) +
    annotate(geom = 'text',
             label = 'Indian',
             angle = 90,
             cex = 5,
             color = 'white',
             x = 10,
             y = 7) +
    annotate(geom = 'text',
             label = paste0('Middle', '\n', 'Eastern'),
             angle = 90,
             cex = 2.5,
             color = 'white',
             x = 10,
             y = 10) 
  
  # plot - Each plot has two version 1: the first for Claim type the second for national origin
  g4 <-
    waffle(parts = values_2, 
           title = 'Version 1 (National origin)',
           colors = colors_2, 
           rows = 10,
           size  = 0.1,
           flip = FALSE) + 
    theme(legend.key.height = unit(1, 'mm')) +
    annotate(geom = 'text',
             label = '71%',
             cex = 7,
             color = 'white',
             x = 6,
             y = 4) +
    annotate(geom = 'text',
             label = '15%',
             cex = 7,
             color = 'white',
             x = 8.5,
             y = 4) +
    annotate(geom = 'text',
             label = '8%',
             cex = 5,
             color = 'white',
             x = 9,
             y = 8) +
    annotate(geom = 'text',
             label = '5%',
             cex = 5,
             color = 'white',
             x = 10,
             y = 7) +
    annotate(geom = 'text',
             label = '1%',
             cex = 4,
             color = 'white',
             x = 10,
             y = 10) 
  
 
  return(list(g1, g2, g3, g4))
}
