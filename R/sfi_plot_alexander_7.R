#' Alexander 7
#' 
#' Generate a plot for Alexander Figure 7
#' @export
#' @return a list of plots
#' @import ggplot2
#' @import dplyr
#' @import waffle

sfi_plot_alexander_7 <- function(){
#   
  # # Get data
  data <- all_data$alexander$f7
  data$value <- data$value * 100
  
  # create names vectors for values and colors 
  values <- data$value
  values_with_per <- data$value
  names(values_with_per) <- paste0(data$key, ' (',values,'%)')
  names(values) <- paste0(data$key)
  values_with_per <- sort(values_with_per, decreasing = TRUE)
  values <- sort(values, decreasing = TRUE)
  colors <- make_colors(length(values), bw = TRUE)
  
  # ---------------------------------------------
  # waffle chart
  # plot - Each plot has two version 1: the first for Claim type the second for national origin
  g1 <-
    waffle(parts = values_with_per, 
           title = 'Version 1',
           colors = colors, 
           rows = 10,
           size  = 0.1,
           flip = FALSE) + 
    theme(legend.key.height = 
            unit(1, 'mm')) +
    annotate(geom = 'text',
             label = 'Adopted',
             cex = 7,
             color = 'white',
             x = 5,
             y = 5) +
    annotate(geom = 'text',
             label = 'Partial',
             angle = 90,
             cex = 7,
             color = 'white',
             x = 9,
             y = 7) +
    annotate(geom = 'text',
             label = 'Unknown',
             angle = 90,
             cex = 7,
             color = 'white',
             x = 10,
             y = 5) +
    annotate(geom = 'text',
             label = 'Rejected',
             cex = 2,
             color = 'white',
             x = 10,
             y = 9) +
    annotate(geom = 'text',
             label = 'Other',
             cex = 3,
             color = '#444444',
             x = 10,
             y = 10) 
  
  
  
  
  g2 <-
    waffle(parts = values, 
           title = 'Version 2',
           colors = colors, 
           rows = 10,
           size  = 0.1,
           flip = FALSE) + 
    theme(legend.key.height = 
            unit(1, 'mm')) +
    annotate(geom = 'text',
             label = '85%',
             cex = 10,
             color = 'white',
             x = 5,
             y = 5) +
    annotate(geom = 'text',
             label = '7%',
             angle = 90,
             cex = 7,
             color = 'white',
             x = 9,
             y = 7) +
    annotate(geom = 'text',
             label = '6%',
             angle = 90,
             cex = 7,
             color = 'white',
             x = 10,
             y = 7) +
    annotate(geom = 'text',
             label = '1%',
             cex = 5,
             color = 'white',
             x = 10,
             y = 9) +
    annotate(geom = 'text',
             label = '1%',
             cex = 5,
             color = '#444444',
             x = 10,
             y = 10) 
  
  
  
  
  return(list(g1, g2))
  
}
  
  