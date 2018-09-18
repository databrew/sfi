#' Frankenreiter 5
#' 
#' Generate a plot for Frankenreiter figure 5
#' @export
#' @return a list of plots
#' @import ggplot2
#' @import dplyr
#' @import tidyr
#' @import ggridges
#' @import ggrepel

sfi_plot_frankenreiter_5 <- function(){

  
  # Get data
  data <- all_data$frankenreiter$f5
  
  data1 <- data[data$source == 1,]
  data2 <- data[data$source == 2,]
  
  
  # plot the 1955 to 2014 data
  cols <- make_colors(length(unique(data1$year)), bw = TRUE)
  data1$year <- as.numeric(data1$year)
  g1 <- ggplot(data1,
         aes(x = coord2,
             y = coord1,
             color = year)) +
    geom_point(size = 4,
               pch = 16,
               alpha = 0.9)  +
    ggrepel::geom_text_repel(data=subset(data1, year ==  1960 | year==  2010),
                             aes(coord2, coord1, label=year), vjust = -1.5, hjust = 2.2) +
    theme_sfi(lp = 'bottom',
              y_axis_title_style = 'bold',
              x_axis_title_style = 'bold',
              title_style = 'bold') +
    labs(x = 'Coordinate 2',
         y = 'Coordinate 1',
         title = 'Figure 5.',
         subtitle = 'Multidimensional scaling of KL divergences between different years') +
    scale_color_gradient(name = '', low = "#2C2C2C", high = "#ABABAB") 
  

  
  

  # plot the 1955 to 2014 data
  cols <- make_colors(length(unique(data2$year)), bw = TRUE)
  data2$year <- as.numeric(data2$year)
  g2 <- ggplot(data2,
               aes(x = coord2,
                   y = coord1,
                   color = year)) +
    geom_point(size = 4,
               pch = 16,
               alpha = 0.9)  +
    ggrepel::geom_text_repel(data=subset(data2, year ==  1970 | year==  2010),
                             aes(coord2, coord1, label=year), vjust = -1.5, hjust = 2.2) +
    theme_sfi(lp = 'bottom',
              y_axis_title_style = 'bold',
              x_axis_title_style = 'bold',
              title_style = 'bold') +
    labs(x = 'Coordinate 2',
         y = 'Coordinate 1',
         title = 'Figure 5.',
         subtitle = 'Multidimensional scaling of KL divergences between different years') +
    scale_color_gradient(name = '', low = "#2C2C2C", high = "#ABABAB") 
  
  
  
  l <- list(g1,g2)
  return(l)
}
